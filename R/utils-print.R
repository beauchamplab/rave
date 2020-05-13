

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


lower_letters_only <- function(s){
  stringr::str_to_lower(s)
  stringr::str_remove_all(s, '[^a-zA-Z0-9]')
}
