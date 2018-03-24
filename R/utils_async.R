
.queue = new.env()
.queue_result = new.env()

async_start <- function(){
  future::plan(future::multisession)




  future::future({
    reults = list()
    for(nm in names(.queue)){
      results[[nm]] = lazyeval::f_eval(.queue[[nm]])
    }
    return(results)
  }) ->
    .f

  future::future({
    results = future::value(.f)
  })
}

#' @description Async function to execute code
#' @param expr expression for async exe
#' @param name see 'details'
#' @param envir environment of code
#' @details If \code{name} is NULL, expression will be evaluated and nothing will be
#'  returned. However, if name is specified, you can call function
#'  \code{get_async_result(name)} later to get the results if once evaluated.
#'  WARNINGS: if your expression contains large object, the memory will be doubled.
#'  This is because R doesn't support multi-thread and multisession mode will copy
#'  those objects if they are mentioned in the expression. Therefore please
#'  optimize your code carefully.
#' @examples
#' # NOT RUN
#' # Will not block
#' library(future)
#'
#' async({
#'   # block session for 3 secs
#'   Sys.sleep(5)
#'   return('Hello world!')
#' }, name = 'test') ->
#'   f
#'
#' # Still running
#' resolved(f)
#'
#' # wait for 5 sec
#' Sys.sleep(5)
#'
#' # ready!
#' resolved(f)
#' get_async_result('test')
#' # END
#' @import future
#' @export
async <- function(expr, name = NULL, envir = parent.frame(), plan = future::multisession){
  if(!is.null(plan)){
    future::plan(plan, workers = rave_options('max_worker'))
  }
  if(is.null(name)){
    future::future(substitute(expr), envir = envir, substitute = F) ->
      f
  }else{
    future::futureAssign(name, substitute(expr), assign.env = .queue_result, envir = envir,
                         substitute = F) ->
      f
  }
  assign(strftime(Sys.time(), 'ASYNC_%Y%m%s'), f, envir = .queue)

  return(invisible(f))
}


#' @export
get_async_result <- function(name, remove = T){
  if(name %in% ls(.queue_result)){
    re = get(name, envir = .queue_result)
    if(remove){
      rm(list = c(name), envir = .queue_result)
    }
    return(re)
  }else{
    return(NULL)
  }
}



#' @export
lapply_async <- function(x, fun, ..., .ncores = 0, .future_plan = future::multiprocess,
                         .call_back = NULL, .packages = NULL, .envir = environment(), .globals = TRUE, .gc = TRUE){
  if(.ncores <= 0){
    .ncores = rave_options('max_worker')
  }
  if(is.null(.packages)){
    .packages = stringr::str_match(search(), 'package:(.*)')
    .packages = .packages[,2]
    .packages = rev(.packages[!is.na(.packages)])
  }
  .niter = length(x)
  .ncores = as.integer(.ncores)
  .ncores = min(.ncores, .niter)
  future::plan(.future_plan, workers = .ncores)

  .future_list = list()
  .future_values = list()

  .i = 0
  while(.i < .niter){
    .i = .i+1
    .x = x[[.i]]

    .future_list[[length(.future_list) + 1]] = future::future({
      fun(.x)
    }, envir = .envir, substitute = T, lazy = F, globals = .globals, .packages = .packages, gc = .gc)

    if(length(.future_list) >= .ncores){
      # wait for one of futures resolved
      .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      .future_list[[1]] = NULL
      gc()
    }

    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }
  }

  return(c(.future_values, future::values(.future_list)))
}


