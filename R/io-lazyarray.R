#' @title Convert 'LazyArray' instance to 'Tensor' instance
#' @description \code{\link[lazyarray]{ClassLazyArray}} and \code{\link{Tensor}}
#' shared the same storage format, hence it's possible to convert from
#' 'LazyArray' to 'Tensor' instances without moving files.
#' @param arr 'LazyArray' instance, see \code{\link[lazyarray]{ClassLazyArray}}
#' @param drop_partition whether to drop partition if partition file is
#' missing; only valid when \code{arr$is_multi_part()} is true. The result
#' dimensions might be different if \code{drop_partition} is true
#' @return \code{\link{Tensor}} instance
#' @seealso \code{\link{Tensor}}, \code{\link[lazyarray]{ClassLazyArray}}
#'
#' @details
#' \code{arr} must either have multi-part mode turned off or multi-part with
#' \code{mode=1}. \code{mode=2} might result in error or data loss.
#'
#' Because \code{LazyArray} allows missing partitions and data on those
#' partitions are marked as \code{NA}s. This
#' means not all files required by \code{Tensor} exist. Under such condition,
#' there are two options: one is to create those partition files; the other
#' is to drop those partitions as they are \code{NA}s anyway.
#'
#' If \code{drop_partition} is set to false, the returned data dimension will be
#' "as-is". However, if \code{arr} has missing partitions, then new partition
#' files will be created and filled with \code{NA}s. This is useful when
#' data dimension is required to be the same as input.
#'
#' If \code{drop_partition} is set to true, then partition files (obtained
#' via \code{arr$get_partition_fpath()}) will be
#' tested. The missing files will be dropped from the result. For example,
#' given input with dimension \code{2x3x4}, if first two partitions are missing,
#' i.e. \code{arr[,,1:2]} are \code{NA}s, then the returned tensor will have
#' dimension \code{2x3x2} and only keep what already exist. This is especially
#' useful when data are very large.
#'
#' @examples
#'
#' path <- tempfile()
#' arr <- lazyarray::lazyarray(path, storage_format = 'double', dim = 2:4)
#'
#' arr[,,1] <- 1:6
#'
#' # arr is a 'LazyArray' and its partition files are
#' # missing except for the first one
#' arr[]
#'
#' file.exists(arr$get_partition_fpath())
#'
#' # only keep the existing partition
#' ts1 <- lazyarray_to_tensor(arr, drop_partition = TRUE)
#'
#' # the last 3 partitions are dropped, result in 2x3x1 dimension
#' ts1
#' dim(ts1)
#'
#' # Fill in all missing partitions with NA
#' ts2 <- lazyarray_to_tensor(arr, drop_partition = FALSE)
#'
#' # ts2 dimension is 22x3x4
#' ts2
#' ts2$subset(dim3 ~ dim3 == 4, data_only = TRUE, drop = TRUE)
#'
#'
#' @export
lazyarray_to_tensor <- function(arr, drop_partition = FALSE){
  
  stopifnot2(inherits(arr, 'LazyArray'), msg = 'arr must be a lazyarray::LazyArray instance')
  
  # path <- tempfile()
  # arr <- lazyarray::lazyarray(path, storage_format = 'double', dim = 2:4)
  pa <- arr$storage_path
  meta_name <- stringr::str_extract(pa, '[^/\\\\]+$')
  arr <- lazyarray::load_lazyarray(dirname(pa), read_only = FALSE, meta_name = meta_name)
  
  dimnames <- dimnames(arr)
  dim <- dim(arr)
  n_dims <- length(dim)
  
  part_files <- arr$get_partition_fpath(full_path = TRUE)
  for(ii in seq_along(part_files)){
    f <- part_files[[ii]]
    if(!file.exists(f)){
      if(arr$is_multi_part() && drop_partition){
        part_files[[ii]] <- ''
      } else {
        expr <- paste('arr[', paste(rep('', n_dims), collapse = ','), ii, '] <- NA')
        eval(parse(text = expr))
      }
    }
  }
  
  dropped <- part_files == ''
  
  
  if(length(dimnames) != n_dims || !is.list(dimnames)){
    varnames <- paste0('dim', seq_along(dim))
    dimnames <- structure(lapply(dim, seq_len), names = varnames)
  } else {
    varnames <- names(dimnames)
    if(length(dimnames) != n_dims){
      varnames <- paste0('dim', seq_along(dim))
      names(dimnames) <- varnames
    } else if('' %in% varnames){
      varnames[varnames == ''] <- paste0('dim', which(varnames == ''))
      names(dimnames) <- varnames
    }
  }
  
  varnames <- names(dimnames)
  
  if(any(dropped)){
    dim[[n_dims]] <- sum(!dropped)
    dimnames[[n_dims]] <- dimnames[[n_dims]][!dropped]
    part_files <- part_files[!dropped]
  }
  
  re <- Tensor$new(data = 1, dim = rep(1, n_dims),
                   dimnames = dimnames,
                   varnames = varnames, hybrid = FALSE)
  
  re$swap_file <- part_files
  re$.use_multi_files(TRUE)
  re$hybrid <- TRUE
  re$set_data(NULL)
  re$dim <- dim
  re$dimnames <- dimnames
  re$temporary <- FALSE
  re
  
}

