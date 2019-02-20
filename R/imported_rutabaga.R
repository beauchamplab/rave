collapse <- function(x, keep, average = FALSE){
  f = get_from_package('collapse', pkg = 'rutabaga', check = FALSE)
  if(is.function(f)){
    return(f(x, keep, average))
  }else{
    if(any(!is.finite(x))){
      x[!is.finite(x)] = 0
    }

    if(any(is.complex(x))){
      re = collapse(Re(x), keep = keep, average = average)
      im = collapse(Im(x), keep = keep, average = average)
      return(re + 1i * im)
    }
    if(average){
      re = apply(x, keep, mean)
    }else{
      re = apply(x, keep, sum)
    }
    dims = dim(x)

    return(re)
  }
}


cat2 <- function(
  ..., level = 'DEBUG', print_level = FALSE,
  file = "", sep = " ", fill = FALSE, labels = NULL,
  append = FALSE, end = '\n', pal = list(
    'DEBUG' = 'grey60',
    'INFO' = '#1d9f34',
    'WARNING' = '#ec942c',
    'ERROR' = '#f02c2c',
    'FATAL' = '#763053',
    'DEFAULT' = '#000000'
  )
){
  f = get_from_package('cat2', pkg = 'rutabaga', check = FALSE)
  if(is.function(f)){
    call = match.call(expand.dots = FALSE)
    call[[1]] = quote(f)
    eval(call)
  }else{
    cat(..., sep = sep, end = end, file = file, fill = fill, labels = labels, append = append)
  }
  return(invisible())
}
