# fst IO, documented: 2019-11-21



#' @title R6 Class to Load 'fst' Files
#' @author Zhengjia Wang
#' @description provides hybrid data structure for 'fst' file
#' @examples 
#' 
#' # Data to save, total 8 MB
#' x <- matrix(rnorm(1000000), ncol = 100)
#' 
#' # Save to local disk
#' f <- tempfile()
#' fst::write_fst(as.data.frame(x), path = f, compress = 100)
#' 
#' # Load via LazyFST
#' dat <- LazyFST$new(file_path = f, dims = c(10000, 100))
#' 
#' pryr::object_size(dat)
#' #> 236 kB
#' 
#' # Check whether the data is identical
#' range(dat[] - x)
#' 
#' # The reading of column is very fast
#' system.time(dat[,100])
#' 
#' # Reading rows might be slow
#' system.time(dat[1,])
#' 
#' @export
LazyFST <- R6::R6Class(
  classname = 'LazyFST',
  private = list(
    file_path = NULL,
    transpose = FALSE,
    meta = NULL,
    dims = NULL,
    data = NULL,
    last_visited = NULL,
    delayed = 3
  ),
  public = list(
    
    #' @description to be compatible with \code{\link[rave]{LazyH5}}
    #' @param ... ignored
    #' @return none
    open = function(...){},
    
    #' @description close the connection
    #' @param ... ignored
    #' @param .remove_file whether to remove the file when garbage collected
    #' @return none
    close = function(..., .remove_file = FALSE){
      if(.remove_file){
        unlink(private$file_path)
      }
    },
    
    #' @description to be compatible with \code{\link[rave]{LazyH5}}
    #' @param ... ignored
    #' @return none
    save = function(...){
      warning('NOT Implemented yet')
    },
    
    #' @description constructor
    #' @param file_path where the data is stored
    #' @param transpose whether to load data transposed
    #' @param dims data dimension, only support 1 or 2 dimensions
    #' @param ... ignored
    initialize = function(file_path, transpose = FALSE, dims = NULL, ...){
      private$file_path = file_path
      private$transpose = transpose
      # check if dimension matches
      private$meta = fst::metadata_fst(file_path)
      if(length(dims) == 2){
        if(private$meta$nrOfRows * length(private$meta$columnNames) == prod(dims)){
          private$dims = dims
        }else{
          stop('cached data has different dimensions than the given value')
        }
      }else{
        if(is.null(dims)){
          private$dims = c(private$meta$nrOfRows, length(private$meta$columnNames))
          if(transpose){
            private$dims = private$dims[c(2,1)]
          }
        }else{
          stop('fast cache only supports 2 dimension data')
        }
      }
    },
    
    #' @description get data dimension
    #' @param ... ignored
    #' @return vector, dimensions
    get_dims = function(...){
      private$dims
    },
    
    #' @description subset data
    #' @param i,j,... index along each dimension
    #' @param drop whether to apply \code{\link{drop}} the subset
    #' @return subset of data
    subset = function(i = NULL, j = NULL, ..., drop = TRUE){
      if(!length(j)){
        j = seq_len(private$dims[2])
      }
      if(!length(i)){
        i = seq_len(private$dims[1])
      }
      if(is.logical(i)){
        i = which(i)
      }
      if(is.logical(j)){
        j = which(j)
      }
      
      
      real_i = i <= private$dims[1]
      real_j = j <= private$dims[2]
      
      re = matrix(NA, nrow = length(i), ncol = length(j))
      
      private$last_visited = Sys.time()
      
      # if(is.null(private$data)){
      #   # load all data
      #   private$data = as.matrix(fst::read_fst(private$file_path))
      # }
      
      
      # if(private$transpose){
      #   re[real_i, real_j] = t(private$data[j[real_j], i[real_i]])
      # }else{
      #   re[real_i, real_j] = private$data[i[real_i], j[real_j]]
      # }
      
      if(private$transpose){
        col_names = private$meta$columnNames[i[real_i]]
        dat = as.matrix(read_fst(private$file_path, columns = col_names))
        dat = dat[j[real_j], ]
        re[real_i, real_j] = t(dat)
      }else{
        col_names = private$meta$columnNames[j[real_j]]
        dat = as.matrix(read_fst(private$file_path, columns = col_names))
        dat = dat[i[real_i], ]
        re[real_i, real_j] = dat
      }
      rm(dat)
      
      # Profiling shows gc() here will take lots of time
      # gc()
      
      dimnames(re) = NULL
      
      # wait 10 secs to see if data idle, if true, remove private$data
      # later::later(function(){
      #   d = as.numeric(difftime(Sys.time(), private$last_visited, units = 'secs') )
      #   if(d >= private$delayed){
      #     private$data = NULL
      #     gc()
      #   }
      # }, delay = private$delayed)
      
      if(drop){
        return(drop(re))
      }else{
        return(re)
      }
      
    }
  )
)



#' @export
`[.LazyFST` <- function(obj, i, j, ..., drop = F){
  if(missing(i)){
    i = NULL
  }
  if(missing(j)){
    j = NULL
  }
  obj$subset(i, j, ..., drop = drop)
}

#' @export
`+.LazyFST` <- function(a, b){
  b + a$subset()
}

#' @export
`-.LazyFST` <- function(a, b){
  -(b - a$subset())
}

#' @export
`*.LazyFST` <- function(a, b){
  b * (a$subset())
}

#' @export
`/.LazyFST` <- function(a, b){
  if(inherits(b, 'LazyFST')){
    b = b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyFST <- function(x){
  dim_info = x$get_dims()
  if(length(dim_info) == 1){
    dim_info = NULL
  }
  dim_info
}

#' @export
length.LazyFST <- function(x){
  dim_info = x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyFST <- function(x, ...){
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyFST <- function(z){
  base::Mod(z$subset())
}

#' @export
Arg.LazyFST <- function(z){
  base::Arg(z$subset())
}


#' @export
exp.LazyFST <- function(x){
  base::exp(x$subset())
}
