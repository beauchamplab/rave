write_fst <- function(x, path, ...){
  cat2('Writing to path: ', path)
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
    cat2('File failure: ', path, level = 'FATAL')
  })
  
}

#' Function try to load "fst" file cache, if not found, read "HDF5" file
#' @param fst_path \code{fst} cache path
#' @param h5_path alternate \code{hdf5} path
#' @param h5_name \code{hdf5} data name
#' @param fst_need_transpose does \code{fst} data need transpose?
#' @param fst_need_drop drop dimensions
#' @param ram read to ram?
#'
load_fst_or_h5 <- function(
  fst_path, h5_path, h5_name, fst_need_transpose = F, fst_need_drop = F, ram = F
){
  # check if fst_path exists
  if(file.exists(fst_path)){
    if(ram){
      re = as.matrix(read_fst(fst_path))
      dimnames(re) = NULL
      if(fst_need_transpose){
        re = t(re)
      }
      if(fst_need_drop){
        re = drop(re)
      }
      return(re)
    }else{
      re = LazyFST$new(file_path = fst_path, transpose = fst_need_transpose)
      return(re)
    }
  }else{
    re = load_h5(file = h5_path, name = h5_name, read_only = T, ram = ram)
    return(re)
  }
}

