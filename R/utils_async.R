
.queue = new.env()
.queue_result = new.env()

# # Start async process (deprecated)
# async_start <- function(){
#   future::plan(future::multisession)
#
#
#
#
#   future::future({
#     reults = list()
#     for(nm in names(.queue)){
#       results[[nm]] = lazyeval::f_eval(.queue[[nm]])
#     }
#     return(results)
#   }) ->
#     .f
#
#   future::future({
#     results = future::value(.f)
#   })
# }

#' Async function to execute code (depricated)
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
#' \dontrun{
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
#' }
#' @export
async <- function(expr, name = NULL, envir = parent.frame()){
  rave_setup_workers()
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


#' Get async process result (deprecated)
#' @param name async name
#' @param remove remove future object?
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






