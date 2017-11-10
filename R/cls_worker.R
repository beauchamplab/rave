#' @include cls_observable.R
#' @include options.R
NULL

# Future, async tools

#' Worker
#' @description
#' An way to execute long code in an asynchronized settings (without-session block).
#' @name async
NULL


#' @import future
#' @export
Worker <- R6::R6Class(
  classname = 'Worker',
  inherit = Observable,

  portable = FALSE,  # Haven't validated yet
  cloneable = FALSE,


  private = list(
    future_env = NULL
  ),

  public = list(
    observable = FALSE,
    status = NULL,     # ready running resolved error,
    flag = NULL,
    tmp_file = '',
    result = NULL,

    initialize = function(){
      self$observable = FALSE
      self$status = shiny::reactiveVal('ready')
      self$tmp_file = tempfile(fileext = '.dat')
    },

    run = function(x, env = parent.frame(), globals = TRUE, quoted = FALSE){
      func = exprToFunction(x, env, quoted = quoted)
      self$observable <- FALSE

      private$future_env <- future::future({
        res <- do.call('func', args = list())
        return(res)
      }, globals = globals)
      self$flag <- paste0(sample(LETTERS, 26, replace = T), collapse = '')
      self$status('running')
      self$observable <- TRUE
    },

    run_func = function(func, globals = TRUE){
      private$future_env <- future::future({
        res <- do.call('func', args = list())
        return(res)
      }, globals = globals)
      self$flag <- paste0(sample(LETTERS, 26, replace = T), collapse = '')
      self$status('running')
      self$observable <- TRUE
    },

    check = function(){

      if(shiny::isolate(self$status()) == 'resolved'){
        return(TRUE)
      }else if(shiny::isolate(self$status()) == 'error'){
        return(FALSE)
      }

      f <- futureOf(private$future_env)
      resolved = FALSE

      if(resolved(f)){
        tryCatch({
          # try to get value
          self$observable <- FALSE
          self$result <- value(f)
          self$status('resolved')
          resolved = TRUE
        },
        error = function(e){
          logger(toString(e), level = 'WARNING')
          self$status('error')
          self$result <- e
          # For debug use
          self$observable <- FALSE
        })

        unlink(self$tmp_file)
        gcinfo <- gc()
        return(resolved)
      }

      return(FALSE)
    },

    terminate = function(){
      if(!is.null(private$future_env) && 'Future' %in% class(private$future_env)){
        # This is not a standard way, but I have no idea how to terminate future env
        try({
          pid <- private$future_env$job$pid
          system(sprintf('kill %d', pid), wait = F)
          unlink(self$tmp_file)
          self$result <- NA
          self$observable <- FALSE
          gcinfo <- gc()
        })
      }
    }
  )
)

Manager <- R6::R6Class(
  classname = 'Manager',
  inherit = Observable,
  portable = FALSE,  # Haven't validated yet
  cloneable = FALSE,

  public = list(
    observable = TRUE,
    workers = c(),
    process = NULL,
    status = NULL,
    auto_process = TRUE,
    finished = 0,

    check = function(){
      self$finished = 0
      for(w in workers){
        if(w$check()){
          self$finished = self$finished + 1
        }
      }
      if(self$finished < length(self$workers)){
        return('running')
      }
      self$observable = FALSE
      if(self$auto_process){
        self$process()
      }

      self$status('resolved')
      return('resolved')
    },
    initialize = function(workers, process = NULL, auto_process = TRUE){
      self$workers <- workers
      self$status <- shiny::reactiveVal('running')
      self$auto_process <- auto_process
      if(is.null(process)){
        self$process <- function(){}
      }else{
        self$process <- process
      }
    }

  )
)






#' @rdname async
#' @param max_worker Max number of sessions running in the background.
#' @details
#' \code{use_multicores} is only available in Mac and Linux, which means if you are using Windows,
#' you have to use \code{\link{use_singlecore}}.
#' @export
use_multicores <- function(max_worker = rave_opts$get_options('max_worker')){
  future::plan(future::multicore, workers = as.numeric(max_worker))
}

#' @rdname async
#' @export
use_singlecore <- function(){
  future::plan(future::sequential)
}


#' @rdname async
#' @details
#' Run \code{print(run_script)} to see implementation. See details at \code{\link{Worker}}.
#' If \code{use_multicores()} is applied, the \code{expr} will be evaluated in an asynchronize
#' R session and will not block the main session if it takes long time to run.
#' @example ./inst/example/Worker_example.R
#'
#' @export
run_script <- function(expr){
  w <- Worker$new()
  w$run(expr, env = environment())
  if(!is.null(prophet$observer) && 'Observatory' %in% class(prophet$observer)){
    w$observable = TRUE
    prophet$observer$push(w)
  }
  return(w)
}

#' @export
save_RData = function(..., list = character(0), file = NULL){
  names <- as.character(substitute(list(...)))[-1L]
  env = parent.frame()
  logger('Saving ', names, ' ...')
  if(exists(".tmp_file")){
    file = .tmp_file
  }else if(is.null(file)){
    stop("'file' must be specified")
  }
  env$.save.time = Sys.time()
  list = c(list, '.save.time', names)
  if(length(list) == 1){
    stop('There is nothing to save, make sure ... are name-value pairs')
  }


  base::save(list = list, file = file, envir = env)
}

#' @rdname async
#' @export
async = function(func, .tmp_file = NULL, output_envir = new.env()){
  logger('Starting Worker')
  .worker = Worker$new()

  .worker$run_func(func)

  if(!exists('.count', envir = output_envir) || ! "reactive" %in% class(output_envir$.count)){
    output_envir$.count = shiny::reactiveVal(Sys.time())
  }

  output_envir$.tmp_file = .tmp_file
  output_envir$.save.time = Sys.time()
  output_envir$.worker = .worker

  logger('Register Observations')
  obs <- Observable$new(function(){
    tryCatch({
      last_saved = output_envir$.save.time
      load(.tmp_file, envir = output_envir)
      if(output_envir$.save.time != last_saved){
        count = shiny::isolate(output_envir$.count()) + 1
        output_envir$.count(1)
      }
    }, error = function(e){
      if(!stringr::str_detect(toString(e), 'empty')){
        stop(shiny::safeError(e))
      }
    })
    if(shiny::isolate(.worker$status()) == 'resolved'){
      output_envir$.result = .worker$result
      output_envir$.count(-1)
      obs$observable = FALSE
    }
  })

  output_envir$.observer = obs

  if(!is.null(prophet$observer) && 'Observatory' %in% class(prophet$observer)){
    .worker$observable = TRUE
    prophet$observer$push(.worker)
    obs$observable = TRUE
    prophet$observer$push(obs)
  }


  return(output_envir)
}
