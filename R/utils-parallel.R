#' \code{lapply} using future package in asynchronous way
#' @param x,fun,... (See \code{\link{lapply}})
#' @param .ncores Number of cores to use. If the value is 0, the number of cores
#' will be determined by rave_options('max_worker').
#' @param .call_back A function takes current iteration number as argument, can be NULL.
#' @param .packages NULL be default, then the function will detect attached packages
#' automatically. Otherwise you have to specify the packages that you want to load.
#' @param .globals Automatically detect variables. See ?future::future
#' @param .gc Clean up environment after each iterations? Recommended for large datasets.
#' @param .envir internally used
#' @param .as_datatable logical, return result as \code{data.frame}. Experimental.
#' @param .nrows integer, if \code{.as_datatable=TRUE}, number of rows expected.
#' @param .callback function or \code{NULL}, callback function to monitor updates.
#' @examples
#' \dontrun{
#' lapply_async(1:10, function(x){
#'   Sys.sleep(2) # Run for 1 secs
#'   Sys.getpid()
#' }, .ncores = 3, .call_back = function(i){
#'   cat('Running iteration -', i, '\n')
#' })
#' }
#' @export
lapply_async <- function(
  x, fun, ..., .ncores = 0, .call_back = NULL, .packages = NULL,
  .envir = environment(), .globals = TRUE, .gc = TRUE, .as_datatable = FALSE,
  .nrows = 0
){
  call <- as.list(match.call())
  if(any(c(".envir", ".as_datatable", ".nrows") %in% names(call))) {
    warning('lapply_async: arguments ".envir", ".as_datatable", ".nrows" are deprecated. They will be ignored.')
  }
  
  .ncores <- as.integer(.ncores)
  if(.ncores <= 0){
    .ncores <- rave_options('max_worker')
  }
  if(is.null(.packages)){
    .packages <- stringr::str_match(search(), 'package:(.*)')
    .packages <- .packages[,2]
    .packages <- rev(.packages[!is.na(.packages)])
  }
  dipsaus::make_forked_clusters(workers = .ncores, clean = TRUE)
  
  if(is.function(.call_back)){
    if(length(formals(.call_back)) >= 1){
      .callback <- function(el) {
        idx <- which(vapply(x, function(el2){ identical(el, el2) }, FALSE))
        if(length(idx)){
          re <- .call_back(idx[[1]])
          if(!is.character(re)){ re <- "" }
        } else {
          re <- ""
        }
        re
      }
    } else {
      .callback <- function(el){
        re <- .call_back()
        if(!is.character(re)){ re <- "" }
        re
      }
    }
    
  } else {
    .callback <- NULL
  }
  dipsaus::lapply_async2(x = x, FUN = fun, FUN.args = list(...), 
                         callback = .callback,
                         plan = FALSE, 
                         future.globals = .globals, 
                         future.packages = .packages)
  
  # if(!length(x)){
  #   return(list())
  # }
  # .colnames = paste0('V', seq_along(x))
  # .ncores = as.integer(.ncores)
  # if(.ncores <= 0){
  #   .ncores = rave_options('max_worker')
  # }
  # # compatible with windows
  # args = list(...)
  # if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in') || .ncores == 1){
  #   return(lapply(seq_along(x), function(ii){
  #     if(is.function(.call_back)){
  #       try({
  #         .call_back(ii)
  #       })
  #     }
  #     do.call(fun, c( list(quote(x[ii])), args), envir = environment())
  #   }))
  # }
  # 
  # 
  # if(is.null(.packages)){
  #   .packages = stringr::str_match(search(), 'package:(.*)')
  #   .packages = .packages[,2]
  #   .packages = rev(.packages[!is.na(.packages)])
  # }
  # .niter = length(x)
  # 
  # 
  # rave_setup_workers(.ncores)
  # if(.ncores != rave_options('max_worker')){
  #   on.exit({
  #     rave_setup_workers()
  #   })
  # }
  # 
  # 
  # .future_list = list()
  # 
  # if(.as_datatable){
  #   .future_values = data.frame(
  #     V1 = rep(NA, .nrows),
  #     keep.rownames = FALSE, stringsAsFactors = FALSE
  #   )
  # }else{
  #   .future_values = list()
  # }
  # 
  # 
  # 
  # 
  # if(.niter == 0){
  #   return(list())
  # }
  # 
  # .this_env = environment()
  # ..started = FALSE
  # 
  # 
  # lapply(seq_along(x), function(.i){
  #   if(is.function(.call_back)){
  #     try({
  #       .call_back(.i)
  #     })
  #   }
  #   
  #   expr = rlang::quo_squash(rlang::quo({ do.call(fun, c(list(quote(x[[!!.i]])), args)) }))
  #   
  #   .this_env$.future_list[[length(.future_list) + 1]] = future::future(expr, envir = .envir, substitute = FALSE, lazy = FALSE, globals = .globals, .packages = .packages, gc = .gc)
  #   
  #   
  #   if(length(.future_list) >= .ncores){
  #     # wait for one of futures resolved
  #     if(!..started && .as_datatable){
  #       .this_env$..started = TRUE
  #       .this_env$.future_values[[1]] = future::value(.future_list[[1]])
  #     }else{
  #       .this_env$.future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
  #     }
  #     
  #     .this_env$.future_list[[1]] = NULL
  #   }
  # })
  # 
  # if(length(.future_list)){
  #   future::resolve(.future_list)
  #   while(length(.future_list)){
  #     .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
  #     .future_list[[1]] = NULL
  #   }
  # }
  # 
  # return(.future_values)
}


#' @rdname lapply_async
#' @export
lapply_async3 <- function(x, fun, ..., .globals = TRUE, .gc = TRUE, 
                          .callback = NULL, .ncores = 0){
  .ncores = as.integer(.ncores)
  if(.ncores <= 0){
    .ncores = rave_options('max_worker')
  }
  if(is.null(.packages)){
    .packages = stringr::str_match(search(), 'package:(.*)')
    .packages = .packages[,2]
    .packages = rev(.packages[!is.na(.packages)])
  }
  dipsaus::make_forked_clusters(workers = .ncores, clean = TRUE)
  dipsaus::lapply_async2(x = x, FUN = fun, FUN.args = list(...), 
                         callback = .callback,
                         plan = FALSE, 
                         future.globals = .globals, 
                         future.packages = .packages)
}


wrap_callback <- function(.callback){
  if(!is.function(.callback)){
    return()
  }
  fmcb <- formals(.callback)
  if(length(fmcb) >= 2 || "..." %in% names(fmcb)){
    callback <- function(el, ii){
      .callback(el, ii)
    }
  } else if (length(fmcb) == 1){
    callback <- function(el, ii){
      .callback(el)
    }
  } else {
    callback <- function(el, ii){
      .callback()
    }
  }
  callback
}

attached_packages <- function (include_base = FALSE) {
  info <- utils::sessionInfo()
  bk <- rev(info$basePkgs)
  pk <- vapply(info$otherPkgs, "[[", "Package", "Package", 
               USE.NAMES = FALSE)
  pk <- rev(pk)
  if (include_base) {
    pk <- c(bk, pk)
  }
  pk
}

lapply_callr2 <- function(
  x, fun, ..., .callback = NULL,
  .globals = list(), .ncores = 0,
  .packages = NULL,
  .focus_on_console = TRUE, .rs = FALSE, .quiet = FALSE,
  .name = ""){
  
  if( .ncores <= 0 ){
    .ncores <- rave_options('max_worker')
  }
  if(isTRUE(.packages == "")){
    .packages <- attached_packages()
  }
  .packages <- unique(c("dipsaus", .packages))
  
  res <- dipsaus::lapply_callr(x, fun, ..., .callback = .callback,
                               .globals = .globals, .ncores = .ncores,
                               .packages = .packages,
                               .focus_on_console = .focus_on_console, 
                               .rs = FALSE, .quiet = .quiet,
                               .name = .name)
  res
}
