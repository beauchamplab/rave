
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
load_h5 <- function(file, name, read_only = T, ram = F){
  f = re = LazyH5$new(file_path = file, data_name = name, read_only = read_only)
  if(ram){
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
  f = LazyH5$new(file, name, read_only = FALSE)
  f$save(x, chunk = chunk, level = level, replace = replace, new_file = new_file, ctype = ctype, force = TRUE, ...)
  f$close(all = TRUE)
  return()
}

