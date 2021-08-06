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
  if(!length(x)){
    return(list())
  }
  .colnames = paste0('V', seq_along(x))
  .ncores = as.integer(.ncores)
  if(.ncores <= 0){
    .ncores = rave_options('max_worker')
  }
  # compatible with windows
  args = list(...)
  if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in') || .ncores == 1){
    return(lapply(seq_along(x), function(ii){
      if(is.function(.call_back)){
        try({
          .call_back(ii)
        })
      }
      do.call(fun, c( list(quote(x[ii])), args), envir = environment())
    }))
  }
  
  
  if(is.null(.packages)){
    .packages = stringr::str_match(search(), 'package:(.*)')
    .packages = .packages[,2]
    .packages = rev(.packages[!is.na(.packages)])
  }
  .niter = length(x)
  
  
  rave_setup_workers(.ncores)
  if(.ncores != rave_options('max_worker')){
    on.exit({
      rave_setup_workers()
    })
  }
  
  
  .future_list = list()
  
  if(.as_datatable){
    .future_values = data.frame(
      V1 = rep(NA, .nrows),
      keep.rownames = FALSE, stringsAsFactors = FALSE
    )
  }else{
    .future_values = list()
  }
  
  
  
  
  if(.niter == 0){
    return(list())
  }
  
  .this_env = environment()
  ..started = FALSE
  
  
  lapply(seq_along(x), function(.i){
    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }
    
    expr = rlang::quo_squash(rlang::quo({ do.call(fun, c(list(quote(x[[!!.i]])), args)) }))
    
    .this_env$.future_list[[length(.future_list) + 1]] = future::future(expr, envir = .envir, substitute = FALSE, lazy = FALSE, globals = .globals, .packages = .packages, gc = .gc)
    
    
    if(length(.future_list) >= .ncores){
      # wait for one of futures resolved
      if(!..started && .as_datatable){
        .this_env$..started = TRUE
        .this_env$.future_values[[1]] = future::value(.future_list[[1]])
      }else{
        .this_env$.future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      }
      
      .this_env$.future_list[[1]] = NULL
    }
  })
  
  if(length(.future_list)){
    future::resolve(.future_list)
    while(length(.future_list)){
      .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
      .future_list[[1]] = NULL
    }
  }
  
  return(.future_values)
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


lapply_callr2 <- function(
  x, fun, ..., .callback = NULL,
  .globals = list(), .ncores = 0,
  .packages = NULL,
  .focus_on_console = TRUE, .rs = FALSE, .quiet = FALSE,
  .name = ""){
  
  ns <- asNamespace('dipsaus')
  if( .ncores <= 0 ){
    .ncores <- rave_options('max_worker')
  }
  if(isTRUE(.packages == "")){
    .packages <- ns$attached_packages()
  }
  .packages <- unique(c("dipsaus", .packages))
  
  
  if(is.function(ns$lapply_callr)){
    res <- ns$lapply_callr(x, fun, ..., .callback = .callback,
                           .globals = .globals, .ncores = .ncores,
                           .packages = .packages,
                           .focus_on_console = .focus_on_console, 
                           .rs = FALSE, .quiet = .quiet,
                           .name = .name)
  } else {
    nm <- names(formals(fun))[[1]]
    tempdir(check = TRUE)
    f <- tempfile(fileext = '.rds')
    on.exit({ unlink(f) }, add = TRUE)
    globals <- list(
      formal = formals(fun),
      body = body(fun),
      globals = .globals,
      args = c(structure(list(""), names = nm), list(...))
    )
    saveRDS(globals, file = f)
    res <- dipsaus::fastqueue2()
    queue <- dipsaus::fastqueue2()
    if( .quiet ){
      callback <- NULL
    } else {
      callback <- wrap_callback(.callback)
    }
    
    old.handlers <- progressr::handlers(ns$handler_dipsaus_progress())
    on.exit({
      try({
        progressr::handlers(old.handlers)
      }, silent = TRUE)
    }, add = TRUE)
    
    p <- NULL
    progressr::with_progress({
      if(is.function(callback)){
        p <- progressr::progressor(along = x)
      }
      
      
      lapply(seq_along(x), function(ii){
        if(queue$size() >= .ncores){
          h <- queue$remove()
          while ((code <- h()) > 0) {
            Sys.sleep(0.2)
          }
          if( code < 0 ){
            stop(attr(code, "rs_exec_error"), call. = FALSE)
          }
          res$add(attr(code, "rs_exec_result"))
        }
        if(is.function(callback)){
          p(message = callback(x[[ii]], ii))
        }
        
        h <- dipsaus::rs_exec(bquote({
          .env <- new.env()
          .globals <- readRDS(.(f))
          
          list2env(.globals$globals, envir = .env)
          .globals$args[[.(nm)]] <- .(x[[ii]])
          do.call(
            new_function2(
              args = .globals$formal,
              body = .globals$body,
              env = .env,
              quote_type = 'quote'
            ),
            .globals$args
          )
        }), rs = FALSE, quoted = TRUE, name = sprintf("%s... - [%d]", .name, ii),
        wait = FALSE, packages = .packages, focus_on_console = FALSE)
        queue$add(h)
      })
      
    })
    
    while(queue$size() > 0){
      h <- queue$remove()
      while ((code <- h()) > 0) {
        Sys.sleep(0.2)
      }
      if( code < 0 ){
        stop(attr(code, "rs_exec_error"), call. = FALSE)
      }
      res$add(attr(code, "rs_exec_result"))
    }
    
  }
  
  
  res$as_list()
}
