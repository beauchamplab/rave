write_fst <- function(x, path, ...){
  logger('Writing to path: ', path)
  dir = dirname(path)
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  fst::write_fst(x = x, path = path, ...)
}


read_fst <- function(path, ..., as.data.table = TRUE){
  fst::read_fst(path, ..., as.data.table = as.data.table)
}
