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


# lapply_async <- function(
#   x, fun, ..., .ncores = 0, .call_back = NULL, .packages = NULL,
#   .envir = environment(), .globals = TRUE, .gc = TRUE, .as_datatable = FALSE,
#   .nrows = 0
# ){
#   if(!length(x)){
#     return(list())
#   }
#   .colnames = paste0('V', seq_along(x))
#   .ncores = as.integer(.ncores)
#   if(.ncores <= 0){
#     .ncores = rave_options('max_worker')
#   }
#   # compatible with windows
#   args = list(...)
#   if(stringr::str_detect(Sys.info()['sysname'], '^[wW]in') || .ncores == 1){
#     return(lapply(seq_along(x), function(ii){
#       if(is.function(.call_back)){
#         try({
#           .call_back(ii)
#         })
#         do.call(fun, c( list(quote(x[ii])), args), envir = environment())
#       }
#     }))
#   }
#   
#   
#   if(is.null(.packages)){
#     .packages = stringr::str_match(search(), 'package:(.*)')
#     .packages = .packages[,2]
#     .packages = rev(.packages[!is.na(.packages)])
#   }
#   .niter = length(x)
#   
#   
#   rave_setup_workers(.ncores)
#   if(.ncores != rave_options('max_worker')){
#     on.exit({
#       rave_setup_workers()
#     })
#   }
#   
#   
#   .future_list = list()
#   
#   if(.as_datatable){
#     .future_values = data.table::data.table(
#       V1 = rep(NA, .nrows),
#       keep.rownames = F, stringsAsFactors = F
#     )
#   }else{
#     .future_values = list()
#   }
#   
#   
#   
#   
#   if(.niter == 0){
#     return(list())
#   }
#   
#   .this_env = environment()
#   ..started = FALSE
#   
#   
#   lapply(seq_along(x), function(.i){
#     if(is.function(.call_back)){
#       try({
#         .call_back(.i)
#       })
#     }
#     
#     expr = rlang::quo_squash(rlang::quo({ do.call(fun, c(list(quote(x[[!!.i]])), args)) }))
#     
#     .this_env$.future_list[[length(.future_list) + 1]] = future::future(expr, envir = .envir, substitute = FALSE, lazy = FALSE, globals = .globals, .packages = .packages, gc = .gc)
#     
#     
#     if(length(.future_list) >= .ncores){
#       # wait for one of futures resolved
#       if(!..started && .as_datatable){
#         .this_env$..started = TRUE
#         .this_env$.future_values[[1]] = future::value(.future_list[[1]])
#       }else{
#         .this_env$.future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
#       }
#       
#       .this_env$.future_list[[1]] = NULL
#     }
#   })
#   
#   if(length(.future_list)){
#     future::resolve(.future_list)
#     while(length(.future_list)){
#       .future_values[[1 + length(.future_values)]] = future::value(.future_list[[1]])
#       .future_list[[1]] = NULL
#     }
#   }
#   
#   return(.future_values)
# }




# setup_async_evaluator <- local({
#   id = rand_string()
#   initialized = FALSE
#   eval_path = file.path(subject_cache_dir(), 'EVALUATOR', id)
#   nworkers = 0
#   evaluator = NULL
#   
#   function(reset = FALSE, ...){
#     
#     max_worker = rave_options('max_worker')
#     if(nworkers == 0){
#       nworkers <<- max_worker
#     }
#     
#     if(!reset){
#       if(!initialized && dir.exists(eval_path)){
#         unlink(eval_path, recursive = TRUE, force = TRUE)
#       }
#       
#       evaluator <<- dipsaus::make_async_evaluator(
#         name = '.__RAVE_INTERNAL__.', path = eval_path, 
#         n_nodes = 1, n_subnodes = nworkers, ...)
#       if(initialized && nworkers != max_worker){
#         scale_fun = ifelse(max_worker > nworkers, 'scale_up', 'scale_down')
#         nworkers <<- max_worker
#         evaluator[[scale_fun]](1, nworkers)
#       }
#       
#     }else{
#       if( initialized ){
#         evaluator <- dipsaus::make_async_evaluator(
#           name = '.__RAVE_INTERNAL__.', path = eval_path, 
#           n_nodes = 1, n_subnodes = nworkers)
#         evaluator$terminate()
#       }
#       
#       unlink(eval_path, recursive = TRUE, force = TRUE)
#       
#       evaluator <<- dipsaus::make_async_evaluator(
#         name = '.__RAVE_INTERNAL__.', path = eval_path, 
#         n_nodes = 1, n_subnodes = nworkers, ...)
#     }
#     
#     
#     if(!initialized){
#       RaveFinalizer$new(function(...){
#         try({
#           evaluator$terminate()
#         }, silent = TRUE)
#       })
#     }
#     
#     initialized <<- TRUE
#     
#     evaluator
#   }
# })
# async <- function(expr, varname, success = NULL, failure = NULL, 
#                   quoted = FALSE, assign_env = new.env(parent = emptyenv()), 
#                   eval_env = parent.frame(), ..., .list = list(), 
#                   ...map = NULL, ...debug = FALSE,
#                   evaluator = NULL){
#   if(is.null(map)){
#     map <- dipsaus::rds_map()
#   }
#   s = Sys.time()
#   if(!quoted){
#     expr <- rlang::enquo(expr)
#   }
#   expr1 <- rlang::quo({
#     ...map = !!...map
#     if(...map$get(!!varname, missing_default = 0) == 0){
#       re = rlang::eval_tidy(!!expr)
#       ...map$set(!!varname, 1)
#       re
#     }
#   })
#   expr2 <- rlang::quo({
#     ...map = !!...map
#     re = rlang::eval_tidy(!!expr)
#     ...map$set(!!varname, 2)
#     re
#   })
#   base::print(Sys.time() - s)
#   
#   # incase the result is not evaluated
#   
#   delayedAssign(varname, {
#     rlang::eval_tidy(expr2, env = eval_env)
#   }, assign.env = assign_env)
#   
#   # TODO: wrap this into a internal function
#   if(...debug){
#     catgl('Obtain evaluator')
#   }
#   base::print(Sys.time() - s)
#   
#   if(is.null(evaluator)){
#     evaluator <- setup_async_evaluator()
#   }
#   
#   base::print(Sys.time() - s)
#   
#   if(...debug){
#     catgl('Schedule task - ', varname)
#   }
#   
#   evaluator$run(
#     expr = expr1,
#     success = function(res){
#       base::print(Sys.time() - s)
#       if(...map$get(varname, 0) == 1){
#         if(...debug){
#           catgl('Captured - ', varname)
#         }
#         assign_env[[varname]] <- res
#         if(is.function(success)){
#           success(res)
#         }
#       }else{
#         if(...debug){
#           catgl('Value already calculated, skipping assignment - ', varname)
#         }
#       }
#     },
#     failure = failure, 
#     ..., .list = .list, quoted = TRUE
#   )
#   base::print(Sys.time() - s)
#   
#   
#   assign_env
#   
# }


