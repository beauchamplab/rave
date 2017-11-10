
#' A template for observables (Abstruct Class)
#' @export
Observable <- R6::R6Class(
  'Observable',
  portable = FALSE,
  cloneable = FALSE,
  lock_objects = FALSE,

  public = list(
    observable = FALSE,
    check = function(){
      if(exists('update', envir = self)){
        self$update()
      }
    },
    initialize = function(check = NULL){
      if(is.function(check)){
        self$update = check
      }
    }
  )
)

#' @export
observable = function(expr, env = parent.frame(), add_to_observatory = TRUE){
  func <- exprToFunction(expr, env)
  obs <- Observable$new(func)
  obs$observable <- TRUE

  if(add_to_observatory && !is.null(prophet$observer) && 'Observatory' %in% class(prophet$observer)){
    obs$observable = TRUE
    prophet$observer$push(obs)
  }
  return(obs)
}


#' Collection of observables in Observer-Observables pattern
#' All Observables need to have public method *check()*, and in shiny, those observables will be checked for each \code{interval} time.
#' @param interval Measured in milliseconds, how long shiny should update (check) observables
#' @usage Observatory$new(interval = 1000)
#' @usage obs$push(observable)
#' @export
Observatory <- R6::R6Class(
  'Observatory',
  portable = FALSE,
  cloneable = FALSE,

  public = list(
    timer = NULL,
    observer = NULL,
    observables = list(),
    .status = 'Ready',

    initialize = function(interval = 1000){
      self$timer <- shiny::reactiveTimer(intervalMs = interval)
    },

    start = function(){

      self$observer <- shiny::observe({
        self$timer()
        if(self$.status == 'Running'){
          shiny::isolate(
            self$observe()
          )
        }
      })

      self$.status = 'Running'
    },

    suspend = function(){
      self$.status = 'Suspended'
      self$observer$suspend()
    },

    resume = function(){
      self$.status = 'Running'
      self$observer$resume()
    },

    destroy = function(){
      self$.status = 'destroyed'
      self$observer$destroy()
    },

    observe = function(){
      remove_index <- c()
      index = NULL
      for(w in self$observables){
        index = index + 1
        if(w$observable){
          # Sometimes the sub-process freezes because of the work load I guess? ;D
          tryCatch({
            w$check()
          }, error = function(e){
            # stop(shiny::safeError(e))
            logger(toString(e), level = 'ERROR')
            w$observable = FALSE # Set flag to this observable and release
                                 # at next period
          })

        }else{
          remove_index = c(remove_index, index)
        }
      }
      if(length(remove_index) > 0){
        self$observables[remove_index] <- NULL
      }
    },

    push = function(observable){
      if('Observable' %in% class(observable)){
        n = length(self$observables)
        self$observables[[n+1]] <- observable
      }else{
        logger('Not an Observable class, skipped.', level = 'WARNING')
      }
    }
  )
)


