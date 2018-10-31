




#' Get environment size (deprecated)
#' @param env environment
#' @param sum_up sum up sizes
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

#' Remove large objects within environment
#' @param env environment to trim
#' @param large_size defines large file size
#' @param recursive recursively
#' @param remove_biggest_only only remove the biggest object
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


#' Copy objects from one environment to another
#' @param from_env from
#' @param to_env target
#' @param deep change function environments?
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



