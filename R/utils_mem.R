

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



