
#' Lazy Load "HDF5" File via \code{\link[hdf5r]{hdf5r-package}}
#'
#' @description Wrapper for class \code{\link[rave]{LazyH5}}, which load data with 
#' "lazy" mode - only read part of dataset when needed.
#' 
#' @param file "HDF5" file
#' @param name \code{group/data_name} path to dataset
#' @param read_only default is TRUE, read dataset only
#' @param ram load to RAM immediately
#'
#' @seealso \code{\link[rave]{save_h5}}
#' @export
load_h5 <- function(file, name, read_only = TRUE, ram = FALSE){
  
  re = tryCatch({
    re = LazyH5$new(file_path = file, data_name = name, read_only = read_only)
    re$open()
    re
  }, error = function(e){
    
    if(!read_only){
      stop('Another process is locking the file. Cannot open file with write permission; use ', sQuote('save_h5'), ' instead...\n  file: ', file, '\n  name: ', name)
    }
    cat2('Open failed. Attempt to open with a temporary copy...', level = 'INFO')
    
    # Fails when other process holds a connection to it!
    # If read_only, then copy the file to local directory 
    tmpf = tempfile(fileext = 'conflict.h5')
    file.copy(file, tmpf)
    LazyH5$new(file_path = tmpf, data_name = name, read_only = read_only)
  })
  
  if(ram){
    f = re
    re = re[]
    f$close()
  }
  re
}









#' Save objects to "HDF5" file without trivial checks
#' @param x array, matrix, or vector
#' @param file \code{HDF5} file
#' @param name path to dataset in format like \code{"group/data_name"}
#' @param chunk chunk size
#' @param level compress level
#' @param replace if dataset exists, replace?
#' @param new_file remove old file if exists?
#' @param ctype dataset type: numeric? character?
#' @param ... passed to other \code{LazyH5$save}
#'
#' @seealso \code{\link{load_h5}}
#' @examples
#' 
#' file <- tempfile()
#' x <- 1:120; dim(x) <- 2:5
#' 
#' # save x to file with name /group/dataset/1
#' save_h5(x, file, '/group/dataset/1', chunk = dim(x))
#' 
#' # read data
#' y <- load_h5(file, '/group/dataset/1')
#' y[]
#' @export
save_h5 <- function(x, file, name, chunk = 'auto', level = 4,replace = TRUE, new_file = FALSE, ctype = NULL, ...){
  f = tryCatch({
    f = LazyH5$new(file, name, read_only = FALSE)
    f$open()
    f$close()
    f
  }, error = function(e){
    cat2('Saving failed. Attempt to unlink the file and retry...', level = 'INFO')
    # File is locked, 
    tmpf = tempfile(fileext = 'conflict.w.h5')
    file.copy(file, tmpf)
    unlink(file, recursive = FALSE, force = TRUE)
    file.copy(tmpf, file)
    unlink(tmpf)
    LazyH5$new(file, name, read_only = FALSE)
  })
  on.exit({
    f$close(all = TRUE)
  }, add = TRUE)
  f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)
  
  return()
}

