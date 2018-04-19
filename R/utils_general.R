# to be tested
#' @export
to_color <- function(x, default_length = 1, palette = NULL, shift = 2){
  if(length(x) == 1 && default_length > 1){
    x = rep(x, default_length)
  }
  shift = max(0, shift)

  env = new.env()
  env$text = paste(x)

  if(is.null(palette)){
    # if x is numeric but in factors
    z = x
    if(is.factor(x)){
      tryCatch({
        as.numeric(paste(x))
      }, error = function(e){
        NA
      }, warning = function(e){
        NA
      }) ->
        z
      if(sum(is.na(z))){
        z = x
      }
    }
    if(is.numeric(z)){
      # z is not a integer or improper
      if(sum(z < 1) || sum(abs(z - round(z))) > 1e-4){
        x = paste('X', x)
      }else{
        x = z + shift
      }
    }


    tryCatch({
      col2rgb(x, alpha = 1) / 255
    }, error = function(e){
      x = as.numeric(as.factor(x)) + shift
      col2rgb(x, alpha = 1) / 255
    }) ->
      cols
    env$colors = apply(cols, 2, function(y){
      do.call(rgb, as.list(y))
    })


  }else{
    if(!is.factor(x)){
      x = as.factor(x)
    }
    x = as.numeric(x)
    ncols = length(unique(x))
    if(is.function(palette)){
      palette = palette(ncols)
    }

    assertthat::assert_that(ncols >= length(palette), msg = 'Palette does not have enough length.')
    env$colors = palette[x]
  }

  # generate text
  env$palette = with(env, {
    unique(cbind(text, colors), MARGIN = 1)
  })

  return(as.list(env))

}



############################################### Internal
# utils, will be moved to rutabaga

`set_if_null<-` <- function(x, values) {
  if(is.null(x)) return(values)
  return (x)
}


# Module exec environment
add_to_session <- function(
  session,
  key = 'rave_id',
  val = paste(sample(c(letters, LETTERS, 0:9), 20), collapse = ''),
  override = FALSE
){
  if(!is.null(session)){
    if(override || !exists(key, envir = session$userData)){
      assign(key, val, envir = session$userData)
    }
    return(get(key, envir = session$userData))
  }
  return(NULL)
}

################### Exported methods

#' Concatenate two strings
#' @usage s1 \%&\% s2
#' @examples
#' you <- 'my friend.'
#' print('Hello, ' %&% you)
#' @export
`%&%` <- function(s1,s2) paste0(s1,s2)

#' Evaluate expressions
#' @usage eval_dirty(expr, env = parent,frame(), data = NULL)
#' @details \code{eval_dirty} uses \code{base::eval()} function to evaluate expressions.
#' Compare to \code{rlang::eval_tidy}, which won't affect original environment,
#' \code{eval_dirty} will cause changes to the environment. Therefore if \code{expr}
#' contains assignment, environment will be changed in this case.
#' @examples
#' expr = quote(a <- 111)
#' a = 1; env = globalenv()
#' rlang::eval_tidy(expr, env)
#' print(a)  # Will be 1
#' eval_dirty(expr, env)
#' print(a)  # a is changed
#' @importFrom rlang quo_get_expr
#' @importFrom rlang is_quosure
#' @export
eval_dirty <- function(expr, env = parent.frame(), data = NULL){

  if(is_quosure(expr)){
    expr = quo_get_expr(expr)
  }

  if(!is.null(data)){
    return(base::eval(expr, enclos = env, envir = data))
  }else{
    return(base::eval(expr, envir = env))
  }
}

#' Assign if not exists, or NULL
#' @examples
#' # Remove a if exists
#' if(exists('a', envir = globalenv()))  rm(a, envir = globalenv())
#'
#' # Assign
#' a %?<-% 1; print(a)
#'
#' # However, if assigned, nothing happens
#' a = 1;
#' a %?<-% 2;
#' print(a)
#'
#' # in a list
#' a = list()
#' a$e %?<-% 1; print(a$e)
#' a$e %?<-% 2; print(a$e)
#'
#' @importFrom rlang quo
#' @importFrom rlang !!
#' @export
`%?<-%` <- function(lhs, rhs){
  env = parent.frame()
  lhs = substitute(lhs)

  tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e){
    return(TRUE)
  }) ->
    isnull

  if(isnull){
    quo <- quo(!!lhs <- !!rhs)
    eval_dirty(quo, env = env)   # Need to assign values, no eval_tidy
  }
}

#' Check if value is within a data range
#' @usage
#' is_within(x, ref, strict = FALSE)
#' @examples
#' a <- 1:10
#' a[is_within(a, c(2,5))]
#' a[is_within(a, c(2,5), strict=T)]
#' a[is_within(a, 2:5)]
#'
#' a[a %within% 2:5]
#' @export
is_within <- function(x, ref, strict = FALSE){
  rg = range(ref)
  if(strict){
    return(x > rg[1] & x < rg[2])
  }else{
    return(x >= rg[1] & x <= rg[2])
  }
}

#' @rdname is_within
#' @export
`%within%` <- function(x,ref){
  is_within(x,ref)
}

#' Apply each element under "with" clause
#' @usage lapply_expr <- function(X, expr, wrapper = NULL, env = environment())
#' @details The goal of this function is to get rid of ugly "$" operator.
#' X should be a list of lists, for each elements, \code{expr} will be
#' evaluated. You can use the name of the lists directly. If the names are not
#' provided, use \code{.x} as default variable. See examples.
#' @examples
#' # X is a list of named lists, with name of each elements be "a"
#' X <- replicate(n = 3, list(a = rnorm(10)))
#' lapply_expr(X, {mean(a)})
#'
#' # X is a list of unnamed lists, use ".x" as your vairable names
#' X <- replicate(n = 3, list(rnorm(10)))
#' lapply_expr(X, {mean(.x)})
#'
#' # wrapper needs to be a function. It will be applied to the results
#' # before returning the values.
#'
#' # example 1: unlist the result to be a vector
#' X <- replicate(n = 3, list(rnorm(10)))
#' lapply_expr(X, {mean(.x)}, wrapper = unlist)
#'
#' # example 2: wrap up html components using htmltools::tags$ul
#' lapply_expr(1:10, {
#'   htmltools::tags$li(sprintf('line: %d', .x))
#' }, wrapper = htmltools::tags$ul)
#' @importFrom rlang !!
#' @importFrom rlang as_quosure
#' @importFrom rlang eval_tidy
#' @export
lapply_expr <- function(X, expr, wrapper = NULL, env = parent.frame()){
  expr = substitute(expr, env = environment()) # prevent pre-eval of expr
  ..nms = unique(names(X))
  if(length(..nms) != 1 || ..nms == '') ..nms = '.x'
  lapply(X, function(..x){
    if(!is.list(..x)){
      ..x = list(..x)
      names(..x) = ..nms
    }
    eval_tidy(as_quosure(expr, env = env), data = ..x)
  }) ->
    re
  if(is.function(wrapper)){
    re = wrapper(re)
  }
  re
}

#' Evaluate function as if it's run within another environment
#' @usage eval_within(FUN, env = parent.frame(), ..., .args = list(), .tidy = T)
#' @param FUN Function to be evaluated
#' @param env Environment for evaluation
#' @param ...,.args Parameters needed within function
#' @param .tidy Evaluate with side effect? see example
#' @examples
#' # Arbitrary function
#' f <- function(a){b <- a*a; print(b); b}
#'
#' # environment for evaluation
#' env <- new.env()
#' a = 'This is Invalid.'
#' env$a = 11  # a*a is only valid within env
#'
#' # Case 1: evaluate f with no side effect
#' result <- eval_within(f, env = env, .tidy = T)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' # Case 2: evaluate f with no side effect, but different "a"
#' result <- eval_within(f, env = env, a = 100, .tidy = T)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' # Case 3: evaluate f with side effect
#' result <- eval_within(f, env = env, a = 20, .tidy = F)
#' cat('Result:', result, '\nenv$a: ', env$a, '\nenv$b:', env$b)
#'
#' @importFrom rlang fn_body
#' @importFrom rlang quo
#' @importFrom rlang eval_tidy
#' @export
eval_within <- function(FUN, env = parent.frame(), ..., .args = list(), .tidy = F){
  args = c(.args, list(...))
  if(is.null(env)){
    return(do.call(FUN, args = args))
  }else{
    expr = fn_body(FUN)
    quo = quo(!!expr)
    if(.tidy){
      eval_tidy(quo, data = args, env = env)
    }else{
      if(length(args) == 0){
        args = NULL
      }else{
        list2env(args, env)
      }
      eval_dirty(quo, env = env)
    }
  }
}

#' Function to clear all elements within environment
#' @usage clear_env(env, all.names = T)
#' @example
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#' @export
clear_env <- function(env, all.names = T){
  if(is.environment(env)){
    rm(list = names(as.list(env, all.names = all.names)), envir = env)
  }
}


#' Check if an object is blank string ""
#' @export
is.blank <- function(s){
  s == ''
}

#' Check if value(s) is invalid
#' @usage is_invalid(x, any = F, .invalids = c('null', 'na'))
#' @param x Values to check
#' @param any If TRUE, then it will check if any element in x is invalid,
#' otherwise, it will check if all element of x is invalid
#' @param .invalids Possible choices: 'null', 'na', 'blank'
#' @examples
#' is_invalid(NULL)
#'
#' is_invalid(c(NA, 1))
#'
#' is_invalid(c(NA, 1), any = T)
#'
#' is_invalid('', .invalids = 'blank')
#' @export
is_invalid <- function(x, any = F, .invalids = c('null', 'na')){
  if('null' %in% .invalids){
    if(is.null(x) || !length(x)){
      return(TRUE)
    }
  }
  for(func in paste0('is.', .invalids)){
    res = do.call(func, args = list(x))
    if(length(res) > 1){
      if(any){
        res = any(res)
      }else{
        res = all(res)
      }
    }
    if(res){
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Get value, if value is invalid, then assign value
#' @usage get_val(x, key = NULL, ..., .invalids = c('null', 'na'))
#' @param x List or variable
#' @param key If not NULL, \code{x[[key]]} will be evaluated
#' @param ... Default value to be returned if x or x$key is invalid
#' @param .invalids See ?is_invalid
#' @export
get_val <- function(x, key = NULL, ..., .invalids = c('null', 'na')){

  if(is.null(key)){
    val = x
  }else{
    val = x[[key]]
  }
  if(is_invalid(val, .invalids = .invalids)){
    args = list(...)
    len = length(args)
    if(len){
      if(len == 1){
        val = args[[1]]
      }else{
        val = args
      }
    }
  }
  return(val)
}

#' Calculate if length of input(s) is zero
#' @usage zero_length(..., any = T, na.rm = F)
#' @param ... Element(s) to be evaluated in length
#' @param any Any element has zero-length? or all elements need to have zero-length
#' @param na.rm Should NA be removed before evaluation?
#' @examples
#' # any = TRUE, any element with zero length will yield "TRUE" result
#' # In this case, expressions c(1) and a==NULL are not evaluated
#' zero_length(NULL, c(1), a=={Sys.sleep(10)})
#'
#' # any = FALSE, if any one has non-zero length, return FALSE
#' # Notice that in these two cases, "Sys.sleep(10)" is not evaluated
#' zero_length(NULL, c(1), a=={Sys.sleep(10)}, any = F)
#'
#' # stop('') yields error, which will be counted as invalid/zero-length
#' zero_length(stop(''))
#' @export
zero_length <- function(..., any = T, na.rm = F){
  parent_env = parent.frame()
  args = as.list(match.call())[-1]
  len = length(args)
  if('any' %in% names(args)){
    len = len - 1
    args = args[1:len]
  }
  reNull = function(...){return(NULL)}
  for(i in 1:len){
    tryCatch({
      obj = eval(args[[i]], envir = parent_env)
      if(na.rm == TRUE || na.rm == i){
        obj = obj[!is.na(obj)]
      }
      obj
    }, error = reNull) ->
      obj
    if(any && length(obj) == 0){
      return(TRUE)
    }
    if(!any && length(obj) > 0){
      return(FALSE)
    }
  }
  return(!any)
}


#' Drop nulls within lists/vectors
#' @example
#' x <- list(NULL,NULL,1,2)
#' dropNulls(x)
#' @export
dropNulls <- function (x, .invalids = c('null')) {
  x[!vapply(x, is_invalid, FUN.VALUE = logical(1), .invalids = .invalids)]
}

#' Convert to string and never goes wrong
#' @usage safe_str_c(x, sep = '', collapse = NULL, .error = '')
#' @param .error If x can't be converted to string, return this message
#' @examples
#' safe_str_c('Count - ', 3:1)
#'
#' safe_str_c('Count - ', 3:1, collapse = '..., ')
#'
#' # aaa doesn't exist
#' safe_str_c('Count - ', aaa, .error = 'aaa not exists')
#'
#' # aaa exists
#' aaa <- 0
#' safe_str_c('Count - ', aaa, .error = 'aaa not exists')
#' @export
safe_str_c <- function(..., sep = '', collapse = NULL, .error = ''){
  tryCatch({
    args = dropNulls(list(...))
    if(length(args)){
      return(stringr::str_c(..., sep = sep, collapse = collapse))
    }else{
      return('NULL')
    }
  }, error = function(e){
    return(.error)
  })
}


#' Try to find absolute path without error
#' @usage try_normalizePath(path, sep = c('/', '\\\\'))
#' @details It's always good to use "/" to separate path. I haven't tested
#' on windows, but this function should work. Basically this function uses
#' base::normalizePath. However base::normalizePath returns error if file
#' does not exist. try_normalizePath will check parent directories and try to
#' find absolute path for parent directories.
#' @examples
#' # "./" exist
#' try_normalizePath('./')
#'
#' # Case when path not exist
#' try_normalizePath("./this/path/does/not/exist/")
#' @importFrom stringr str_split
#' @export
try_normalizePath <- function(path, sep = c('/', '\\\\')){
  if(file.exists(path)){
    path = base::normalizePath(path)
    attr(path, 'is_absolute') = TRUE
    return(path)
  }else{
    p = unlist(str_split(path, pattern = paste(sprintf('(%s)', sep), collapse = '|')))
    p = p[p != '']
    pre = p[-length(p)]
    if(length(pre)){
      post = p[length(p)]
      pre = try_normalizePath(str_c(pre, collapse = '/'), sep = sep)
      path = file.path(pre, post, fsep = '/')
      attr(path, 'is_absolute') = attr(pre, 'is_absolute')
      return(path)
    }else{
      attr(path, 'is_absolute') = FALSE
      return(path)
    }
  }
}


safe_object_size <- function(obj, env = NULL){
  if(is.character(obj) && !is.null(env)){
    obj = get(obj, envir = env, inherits = F)
  }
  tryCatch({
    pryr::object_size(obj)},
    error = function(e){
      return(0L)
    })->
    re
  re
}


#' Cache object
#' @usage cache(key, val, global = FALSE, swap = FALSE, file = tempfile(), name = 'data')
#' @param key Any R object, a named list would be the best.
#' @param val Value to cache, if key exists, then value will not be evaluated nor saved
#' @param global option for shiny app, where if global, then the the cache will ignore sessions.
#' @param swap When object size is too large, do you want to save it to local disk?
#' @param file,name If you use swap=T, see \code{\link{save_h5}}
#' @seealso \code{\link{clear_cache}}
#' @examples
#' \dontrun{
#' cache('a', 1) # returns 1
#' cache('a', 2) # still returns 1
#'
#' # clear cache
#' clear_cache()
#' cache('a', 2) # Now returns 2
#'
#' # Not run because a is cached
#' cache('a', 2)
#' cache('a', {Sys.sleep(10); 1})
#'
#' # Use swap
#'
#' y = cache('aa', 1:1000000, swap = T)
#' pryr::object_size(1:1000000)
#' pryr::object_size(y)
#' y[1:5]
#' }
#' @importFrom digest digest
#' @export
cache <- function(key, val, global = FALSE, replace = FALSE, session = NULL, swap = FALSE, file = tempfile(), name = 'data'){
  if(global){
    session = NULL
  }else{
    session %?<-% shiny::getDefaultReactiveDomain()
  }
  cache_env = getDefaultCacheEnvironment(session = session)

  k = digest(key)
  if(replace){
    cache_env[[k]] <- val
  }else{
    cache_env[[k]] %?<-% val
  }

  if(swap && any(
    is.matrix(cache_env[[k]]),
    is.array(cache_env[[k]]),
    is.vector(cache_env[[k]])
    ) &&
    is.numeric(cache_env[[k]])
  ){
    f = file
    name = 'junk'
    save_h5(cache_env[[k]], f, name = name, chunk = NULL, replace = T, new_file = T, level = 0)
    cache_env[[k]] = load_h5(f, name = name)
  }

  return(cache_env[[k]])
}


#' @title Clear cache
#' @seealso \code{\link{cache}}
#' @usage clear_cache(all = FALSE)
#' @param all Clear all cache? Don't turn it on in shiny app. This is for debug use.
#' @export
clear_cache <- function(all = FALSE, session = NULL){
  session %?<-% shiny::getDefaultReactiveDomain()
  cache_env = getDefaultCacheEnvironment(session = session)
  clear_env(cache_env)
  if(all){
    cache_env = getDefaultCacheEnvironment(session = NULL)
    clear_env(cache_env)
  }
}



#' @export
getDefaultCacheEnvironment <- function(
  session = shiny::getDefaultReactiveDomain()
){
  data_env = getDefaultDataRepository(session = session, session_based = T)
  data_env$.cache_env %?<-% new.env(parent = baseenv())
  data_env$.cache_env$.keys = c()
  return(data_env$.cache_env)
}

################################################### High performance functions

#' lapply using future package (async)
#' @usage lapply_async(x, fun, ..., .ncores = 0,
#'     .future_plan = future::multiprocess, .call_back = NULL,
#'     .packages = NULL, .globals = TRUE,
#'     .gc = TRUE)
#' @param x,fun,... (See ?lapply)
#' @param .ncores Number of cores to use. If the value is 0, the number of cores
#' will be determined by rave_options('max_worker').
#' @param .call_back A function takes current iteration number as argument, can be NULL.
#' @param .packages NULL be default, then the function will detect attached packages
#' automatically. Otherwise you have to specify the packages that you want to load.
#' @param .globals Automatically detect variables. See ?future::future
#' @param .gc Clean up environment after each iterations? Recommended for large datasets.
#' @examples
#' lapply_async(1:10, function(x){
#'   Sys.sleep(2) # Run for 1 secs
#'   Sys.getpid()
#' }, .ncores = 3, .call_back = function(i){
#'   cat('Running iteration -', i, '\n')
#' })
#' @importFrom future plan
#' @importFrom future future
#' @importFrom future value
#' @importFrom future values
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
    }

    if(is.function(.call_back)){
      try({
        .call_back(.i)
      })
    }
  }

  return(c(.future_values, future::values(.future_list)))
}
