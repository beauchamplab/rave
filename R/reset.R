#' @export
reset_rave <- function(){
  data_repository$reset()
  prophet$reset()
  if(exists('suma') && "SUMA" %in% class(suma)){
    suma$reset()
  }
}
