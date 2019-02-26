write_fst <- function(x, path, ...){
  logger('Writing to path: ', path)
  fst::write_fst(x = x, path = path, ...)
}
