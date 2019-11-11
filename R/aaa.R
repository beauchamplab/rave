#' @import shiny
#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus collapse
NULL



tags = htmltools::tags
div = htmltools::div

### For dev use only:
gl <- function(..., .envir = parent.frame()){
  glue::glue(..., .envir = .envir)
}

catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal){
  if(missing(.pal)){
    dipsaus::cat2(gl(..., .envir = .envir), level = level)
  }else{
    dipsaus::cat2(gl(..., .envir = .envir), level = level, pal = .pal)
  }
}

cat2 <- dipsaus::cat2

soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'WARNING')
}

hard_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'FATAL')
}

### Stores internal settings (session-based)
..setup_env <- new.env(parent = baseenv())
..setup_env$..setup_env = ..setup_env
..setup_env$finalizers = list()

get_conf <- function(key, default = NULL){
  soft_deprecated()
  if(exists(key, envir = ..setup_env)){
    return(..setup_env[[key]])
  }else{
    default
  }
}

set_conf <- function(key, val, remove_if_null = TRUE){
  soft_deprecated()
  if(remove_if_null && (missing(val) || is.null(val))){
    rm(list = key, envir = ..setup_env, inherits = FALSE)
  }else{
    ..setup_env[[key]] = val
  }
}


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}


`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}


# ---------------- Deprecated functions ------------------

py_console <- function(...){
  hard_deprecated()
}

register_compoundInput <- function(...){
  soft_deprecated()
}



