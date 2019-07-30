write_fst <- function(x, path, ...){
  logger('Writing to path: ', path)
  dir = dirname(path)
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  fst::write_fst(x = x, path = path, ...)
}


read_fst <- function(path, ..., as.data.table = TRUE){
  tryCatch({
    fst::read_fst(path, ..., as.data.table = as.data.table)
  }, error = function(e){
    logger('File failure: ', path)
    stop(e)
  })

}
