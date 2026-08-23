`%OF%` <- function(lhs, rhs) {
  if (length(rhs)) {
    de <- rhs[[1]]
  } else {
    de <- rhs
  }
  lhs <- lhs[!is.na(lhs)]
  if (!length(lhs)) {
    return(de)
  }
  sel <- lhs %in% rhs
  if (any(sel)) {
    return(lhs[sel][[1]])
  }
  return(de)
}


# `value` to `lhs` only if `lhs` is `NULL` or does not exist.
`%?<-%` <- function(lhs, value) {
  env <- parent.frame()
  lhs <- substitute(lhs)
  
  isnull <- tryCatch({
    is.null(eval(lhs, envir = env))
  }, error = function(e) {
    return(TRUE)
  })
  
  if (isnull) {
    eval(as.call(list( quote(`<-`), lhs, value )), envir = env)
  }
}

