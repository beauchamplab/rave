
#' Function to make an environment printable
#' @param env environment to be converted
#' @export
as_printable = function(env){
  stopifnot2(is.environment(env), msg = 'env MUST be an environment.')
  cls = c('rave_printable', class(env))
  cls = unique(cls[!cls %in% ''])
  class(env) = cls
  return(env)
}

#' Override print method for printable environment
#' @param x an rave_printable environment
#' @param ... passed from or to other methods
#' @export
print.rave_printable = function(x, ...){
  stopifnot2(is.environment(x), msg = 'x MUST be an environment.')
  print.default(
    paste0('<environment key=[', paste(ls(x), collapse = ', '), ']>'), ...
  )
  return(x)
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
