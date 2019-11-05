# # Calculate time difference in seconds/minutes...
# time_delta <- function(t1, t2, units = 'secs'){
#   as.numeric(t2 - t1, units='secs')
# }




# get_os <- function(){
#   os = R.version$os
#   if(stringr::str_detect(os, '^darwin')){
#     return('darwin')
#   }
#   if(stringr::str_detect(os, '^linux')){
#     return('linux')
#   }
#   if(stringr::str_detect(os, '^solaris')){
#     return('solaris')
#   }
#   if(stringr::str_detect(os, '^win')){
#     return('windows')
#   }
#   return('unknown')
# }


# get_ram <- function(){
#   os = get_os()
#   ram = 128*1024^3
#   safe_ram = function(e){
#     suppressWarnings({
#       min(memory.limit(), 128*1024^3)
#     })
#   }
# 
#   ram = tryCatch({
#     switch (
#       os,
#       'darwin' = {
#         ram = substring(system("sysctl hw.memsize", intern = TRUE), 13)
#       },
#       'linux' = {
#         ram = system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern = TRUE)
#         ram = as.numeric(ram) * 1024
#       },
#       'solaris' = {
#         ram = system("prtconf | grep Memory", intern = TRUE)
#         ram = stringr::str_trim(ram)
#         ram = stringr::str_split(ram, '[ ]+')[[1]][3:4]
# 
#         power = match(ram[2], c("kB", "MB", "GB", "TB", "Kilobytes", "Megabytes", "Gigabytes", "Terabytes"))
#         ram = as.numeric(ram[1]) * 1024^(1 + (power-1) %% 4)
#       },
#       'windows' = {
#         ram = system("wmic MemoryChip get Capacity", intern = TRUE)[-1]
#         ram = stringr::str_trim(ram)
#         ram = ram[nchar(ram) > 0]
#         ram = sum(as.numeric(ram))
#       }, {
#         ram = min(memory.limit(), 128*1024^3)
#       }
#     )
#     ram
#   }, error = safe_ram, warning = safe_ram)
#   ram = as.numeric(ram)
#   ram
# }


# get_ncores <- function(){
#   as.numeric(future::availableCores())
# }





# Convert file to base64 format
to_datauri <- function(file, mime = ''){
  info = file.info(file)
  ss = jsonlite::base64_enc(input = readBin(file, what = 'raw', n = info$size))
  ss = sprintf('data:%s;base64,%s', mime, ss)
  ss
}
# s = base64enc::dataURI(mime = 'image/*', file = '~/Desktop/Enlight1.jpg', encoding = 'base64')
# stringr::str_sub(s, end = 500)
#
# stringr::str_sub(ss, end = 500)


#' Format Print Strings
#' @param ... characters
#' @param collapse character to collapse characters
#' @param lookup_env which environment to look for data?
fprintf <- function(..., collapse = '\n', lookup_env = parent.frame()){
  s = list(...)
  s = paste(sapply(s, as.character), collapse = collapse)
  s = stringr::str_remove_all(s, '\\$\\{\\{[\\ ]*\\}\\}')
  # find something wrapped with ${{...}}
  pattern = '\\$\\{\\{(.+?)\\}\\}'
  var_names = unlist(
    stringr::str_extract_all(s, pattern)
  )
  if(length(var_names)){
    var_names = stringr::str_match(var_names, pattern)[,2]
    var_keys = unique(var_names)
    vars = sapply(var_keys, function(nm){

      re = as.character(rlang::eval_tidy(rlang::parse_expr(nm), env = lookup_env))
      if(length(re) > 1){
        re = paste(re, collapse = ' ')
      }else if(!length(re)){
        re = ''
      }
      re
    }, simplify = F, USE.NAMES = T)
    vars = unlist(vars[var_names])
    s = stringr::str_split(s, pattern, simplify = T)
    # append '' to vars
    vars = c(vars, rep('', length(s) - length(vars)))
    s = as.vector(matrix(c(s, vars), nrow = 2, byrow = T))
    s = paste(s, collapse = '')
  }
  s
}




#' @export
as.character.rave_bytes <- function(x, digit=1, ...){
  sprintf(sprintf('%%.%df %s', digit, attr(x, 'unit')), x)
}

#' @export
print.rave_bytes <- function(x, digit=1, ...){
  re = as.character(x, digit = digit, ...)
  cat(re)
  invisible(re)
}



time_diff <- function(start, end){
  delta = unclass(end-start)
  list(
    delta = as.numeric(delta),
    units = attr(delta, 'units')
  )
}

crop_data <- function(x, range){
  assert_that(length(range) == 2, msg = 'Range must have length 2.')
  minr = min(range)
  maxr = max(range)
  x[x <= minr] = minr
  x[x >= maxr] = maxr
  x
}



rave_palette <- function(n=1000, one_sided = F, colors = c(
  '#1a237e', '#42b3d5', '#dcedc8', '#ffffff', '#ffecb3', '#e85285', '#6a1b9a'
), width = c(0.5,1,4), alpha = T
){

  width = c(rev(width), width)
  len = seq_along(width)
  nn = round(width / sum(width) * n * (2-(one_sided == 0)))

  cols = lapply(len, function(ii){
    colorRampPalette(colors[ii + c(0,1)], interpolate = 'spline', alpha = alpha)(nn[ii])
  })


  m = length(width) / 2

  if(one_sided == 0){
    cols = c(
      unlist(cols[1:m]), colors[m+1], unlist(cols[-(1:m)])
    )
    return(cols)
  }else if(one_sided > 0){
    cols = c(colors[m+1], unlist(cols[-(1:m)])
    )
    return(cols)
  }else{
    cols = c(unlist(cols[1:m]), colors[m+1])
    return(cols)
  }
}



is.na <- function(x, ...){
  if(!length(x)){
    return(logical(0L))
  }else{
    return(base::is.na(x))
  }
}

############################################### Internal

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



# eval_dirty <- function(expr, env = parent.frame(), data = NULL){
# 
#   if(is_quosure(expr)){
#     expr = quo_squash(expr)
#   }
# 
#   if(!is.null(data)){
#     return(base::eval(expr, enclos = env, envir = data))
#   }else{
#     return(base::eval(expr, envir = env))
#   }
# }


is_within <- function(x, ref, strict = FALSE){
  rg = range(ref)
  if(strict){
    return(x > rg[1] & x < rg[2])
  }else{
    return(x >= rg[1] & x <= rg[2])
  }
}

`%within%` <- function(x,ref){
  is_within(x,ref)
}



#' Function to clear all elements within environment
#'
#' @param env environment to clean
#' @param all.names clear all variables?
#'
#' @examples
#' \dontrun{
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#' }
#' @export
clear_env <- function(env, all.names = T){
  if(is.environment(env)){
    rm(list = names(as.list(env, all.names = all.names)), envir = env)
  }
}


#' Check if an object is blank string ""
#' @param x vector of characters
#' @export
is.blank <- function(x){
  x == ''
}

#' Check if value(s) is invalid
#' @param x Values to check
#' @param any If TRUE, then it will check if any element in x is invalid,
#' otherwise, it will check if all element of x is invalid
#' @param .invalids Possible choices: 'null', 'na', 'blank'
#' @examples
#' \dontrun{
#' is_invalid(NULL)
#'
#' is_invalid(c(NA, 1))
#'
#' is_invalid(c(NA, 1), any = T)
#'
#' is_invalid('', .invalids = 'blank')
#' }
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
#' @param ... Element(s) to be evaluated in length
#' @param any Any element has zero-length? or all elements need to have zero-length
#' @param na.rm Should NA be removed before evaluation?
#' @examples
#' \dontrun{
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
#' }
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
#'
#' @param x list
#' @param .invalids 'null', 'na', 'blank' default is null
#'
#' @examples
#' \dontrun{
#' x <- list(NULL,NULL,1,2)
#' dropNulls(x)
#' }
#' @export
dropNulls <- function (x, .invalids = c('null')) {
  x[!vapply(x, is_invalid, FUN.VALUE = logical(1), .invalids = .invalids)]
}


# Try to find absolute path without error
try_normalizePath <- function(path, sep = c('/', '\\\\')){
  if(file.exists(path)){
    path = base::normalizePath(path)
    attr(path, 'is_absolute') = TRUE
    return(path)
  }else{
    # if dirname = itself
    dirname = dirname(path)
    if(dirname == path){
      attr(path, 'is_absolute') = FALSE
      return(path)
    }else{
      pre = try_normalizePath(dirname, sep = sep)

      p = unlist(str_split(path, pattern = paste(sprintf('(%s)', sep), collapse = '|')))
      fname = tail(p, 1)

      path = file.path(pre, fname, fsep = '/')
      attr(path, 'is_absolute') = attr(pre, 'is_absolute')
      return(path)
    }
  }
}


#' Cache object
#' @param key Any R object, a named list would be the best.
#' @param val Value to cache, if key exists, then value will not be evaluated nor saved
#' @param global option for shiny app, where if global, then the the cache will ignore sessions.
#' @param replace Force replace cache?
#' @param session internally used
#' @param swap Save to swap? usually when val is a large matrix or vector
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
#' object.size(1:1000000)
#' object.size(y)
#' y[1:5]
#' }
#' @export
cache <- function(key, val, global = FALSE, replace = FALSE, session = NULL, swap = FALSE, file = tempfile(), name = 'data'){
  if(global){
    session = NULL
  }else{
    session %?<-% getDefaultReactiveDomain()
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
#' @param all Clear all cache? Don't turn it on in shiny app. This is for debug use.
#' @param session internally used
clear_cache <- function(all = FALSE, session = NULL){
  session %?<-% getDefaultReactiveDomain()
  cache_env = getDefaultCacheEnvironment(session = session)
  clear_env(cache_env)
  if(all){
    cache_env = getDefaultCacheEnvironment(session = NULL)
    clear_env(cache_env)
  }
}


#' Get Cache Environment
#' @param session internally used
#' @export
getDefaultCacheEnvironment <- function(
  session = getDefaultReactiveDomain()
){
  session_id = add_to_session(session)
  session_id %?<-% '.TEMP'
  global_env = globalenv()
  if(!is.environment(global_env[['.cache_rave']])){
    global_env[['.cache_rave']] = new.env(parent = emptyenv())
  }
  global_env[['.cache_rave']][[session_id]] %?<-% new.env(parent = emptyenv())
  return(global_env[['.cache_rave']][[session_id]])
}

################################################### High performance functions

#' lapply using future package (async)
#' @param x,fun,... (See ?lapply)
#' @param .ncores Number of cores to use. If the value is 0, the number of cores
#' will be determined by rave_options('max_worker').
#' @param .call_back A function takes current iteration number as argument, can be NULL.
#' @param .packages NULL be default, then the function will detect attached packages
#' automatically. Otherwise you have to specify the packages that you want to load.
#' @param .globals Automatically detect variables. See ?future::future
#' @param .gc Clean up environment after each iterations? Recommended for large datasets.
#' @param .envir intrnally used
#' @param .as_datatable logical, return result as \code{data.table}. Experimental.
#' @param .nrows integer, if \code{.as_datatable=TRUE}, number of rows expected.
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
        do.call(fun, c( list(quote(x[ii])), args), envir = environment())
      }
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
    .future_values = data.table::data.table(
      V1 = rep(NA, .nrows),
      keep.rownames = F, stringsAsFactors = F
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

# to be removed

restart_rave <- function(reload = T, quiet = FALSE){
  unloadns = function(ns_){
    ns = ns_
    if(isNamespaceLoaded(ns)){
      ns = asNamespace(ns)
      sub_ns = getNamespaceUsers(ns)
      for(sbns in sub_ns){
        unloadns(sbns)
      }
      if(!quiet){
        base::message("Unload namespace - ", ns_)
      }
      unloadNamespace(ns_)
    }
  }

  unloadns('rave')

  cmd = ''
  if(reload){
    cmd = 'base::library(rave)'
  }

  eval(quote(rm(list = ls(all.names = T, envir = globalenv()), envir = globalenv())))
  eval(parse(text = sprintf('rstudioapi::restartSession(%s)', cmd)))
}
