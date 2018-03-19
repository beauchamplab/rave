#' @import rhdf5
#' @export
LazyH5 <- R6::R6Class(
  classname = 'LazyH5',
  portable = F,
  private = list(
    data = NULL,
    file = NULL,
    name = NULL,
    read_only = TRUE
  ),
  public = list(
    print = function(){
      dims = self$get_dims()

      cat('HDF5 DATASET\n', '\tRank:', dims$rank, '\n\tSize:', paste(dims$size, collapse = ' x '))
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = T){
      private$file = tools::file_path_as_absolute(file_path)
      assertthat::assert_that(H5Fis_hdf5(private$file), msg = 'File doesn\'t have H5 format')
      private$name = data_name
      private$read_only = read_only
    },

    open = function(){
      if(!is(private$data, "H5IdComponent") || !H5Iis_valid(private$data)){
        if(private$read_only){
          f = rhdf5::H5Fopen(private$file, "H5F_ACC_RDONLY")
        }else{
          f = rhdf5::H5Fopen(private$file)
        }
        on.exit({H5Fclose(f)})
        private$data = rhdf5::H5Dopen(f, private$name)
      }
    },

    close = function(){
      rhdf5::H5Dclose(private$data)
    },

    subset = function(i, j, ..., drop = TRUE){
      env = parent.frame()
      self$open()
      on.exit({self$close()})

      index = as.list(sys.call())[-1]

      index = lapply(index, function(x){
        if(length(x) == 0 || x == ''){
          NULL
        }else{
          eval(x, envir = env)
        }
      })
      if(length(dropNulls(index)) == 0){
        index = NULL
      }
      rhdf5:::h5readDataset(h5dataset = private$data, index = index)

    },

    get_dims = function(){
      self$open()
      s = H5Dget_space(private$data)
      on.exit({
        self$close()
        H5Sclose(s)
      })
      H5Sget_simple_extent_dims(s)
    }
  )
)

#' @export
`[.LazyH5` <- function(obj, ...){
  env = parent.frame()
  index = as.list(sys.call())[-c(1,2)]

  index = lapply(index, function(x){
    if(length(x) == 0 || x == ''){
      NULL
    }else{
      eval(x, envir = env)
    }
  })
  if(length(dropNulls(index)) == 0){
    index = list()
  }
  do.call(obj$subset, index)
}

#' @export
`+.LazyH5` <- function(a, b){
  a$subset() + b
}

#' @export
dim.LazyH5 <- function(obj){
  dim_info = obj$get_dims()
  if(dim_info$rank > 1){
    dim_info$size
  }else{
    NULL
  }
}

#' @export
length.LazyH5 <- function(obj){
  dim_info = obj$get_dims()
  prod(dim_info$size)
}

#' @export
as.array.LazyH5 <- function(obj, ...){
  as.array(obj[], ...)
}

