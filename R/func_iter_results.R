#' Literally does nothing
#' @param ... nothing
#' @export
do_nothing <- function(...){

}



#' Convert object to strings
#' @param obj R object to deparse
#' @export
rave_deparse <- function(obj){
  paste0(deparse(obj), collapse = '')
}


