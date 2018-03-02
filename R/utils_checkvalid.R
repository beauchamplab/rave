dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

dropInvalid <- function(x, deep = FALSE, nulls = c('')){
  if(deep && is.list(x)){
    x = lapply(x, dropInvalid, deep = deep, nulls = nulls)
    x = dropNulls(x)
  }else{
    x = x[vapply(x, is_valid, FUN.VALUE = logical(1), nulls = nulls)]
    if(length(x) == 0){
      x = NULL
    }
  }
  x
}

is_valid <- function(x, nulls = c('')){
  if(
    length(x) == 0 || is.null(x) ||
    ('' %in% nulls && x == '') ||
    ('NA' %in% nulls && length(x) == 1 && is.na(x)) ||
    ('Inf' %in% nulls && is.numeric(x) && length(x) == 1 && is.infinite(x))
  ){
    return(FALSE)
  }
  return(TRUE)
}
