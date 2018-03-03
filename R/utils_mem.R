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


copy_env <- function(from_env, to_env){
  lapply(ls(envir = from_env, all.names = T), function(nm){
    assign(nm, get(nm, envir = from_env), envir = to_env)
  })
}
