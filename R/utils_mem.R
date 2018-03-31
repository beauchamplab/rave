clear_env <- function(env, all.names = T){
  if(is.environment(env)){
    rm(list = ls(envir = env, all.names = all.names), envir = env)
  }
}

safe_str_c <- function(..., sep = '', collapse = NULL){
  tryCatch({
    args = dropNulls(list(...))
    if(length(args)){
      stringr::str_c(..., sep = sep, collapse = collapse)
    }else{
      return('NULL')
    }
  }, error = function(e){
    return('')
  })
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

safe_env_size <- function(env, sum_up = TRUE){
  if(exists('.keys', envir = env)){
    k = env$.keys
  }else{
    k = ls(envir = env, all.names = T)
  }
  if(length(k) == 0){
    return(0L)
  }
  lapply(k, safe_object_size, env = env) ->
    obj_sizes

  if(sum_up){
    sum(unlist(obj_sizes))
  }else{
    obj_sizes
  }
}

trim_env <- function(
  env,
  large_size = -1,
  recursive = FALSE,
  remove_biggest_only = TRUE
){
  if(large_size == 0){
    return()
  }
  if(large_size == -1){
    large_size = rave_options('big_object_size')
  }

  obj_sizes = safe_env_size(env, sum_up = F)
  env_size = sum(unlist(obj_sizes))

  if(env_size > large_size){
    i = which.max(unlist(obj_sizes))
    nm = names(obj_sizes)[i]

    rm(list = nm, envir = env)
    if(exists('.keys', envir = env)){
      env$.keys = env$.keys[env$.keys != nm]
    }
  }
}


copy_env <- function(from_env, to_env, deep = FALSE){
  l = as.list(from_env, all.names = T)
  l = sapply(l, function(x){
    if(deep && is.function(x)){
      environment(x) <- to_env
      return(x)
    }else if(deep && is.environment(x)){
      x = copy_env(x, new.env(), deep = deep)
      return(x)
    }else{
      return(x)
    }
  }, simplify = F, USE.NAMES = T)
  list2env(l, envir = to_env)
  to_env
}



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

is_invalid <- function(x, any = F, .invalids = c('null', 'na')){
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

is.blank <- function(s){
  (is.na(s) | s == '')
}

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

