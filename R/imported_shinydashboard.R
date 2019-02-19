#' Wrapper for "box" in shinydashboard
#' @param ... passed to shinydashboard::box
box <- function(...){
  f = get_from_package('box', pkg = 'shinydashboard', check = F)
  f(...)
}

