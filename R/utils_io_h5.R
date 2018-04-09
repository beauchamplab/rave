# File IO

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



#' Lazy load HDF5 file via rhdf5 package
#' @usage load_h5(file, name, read_only = T)
#' @details load_h5 is a wrapper for class LazyH5, which load data with "lazy" mode -
#' only read part of dataset when needed.
#' @seealso \code{\link{save_h5}}
#' @examples
#' \dontrun{
#' f <- system.file('data/data_dir/Subject_RAVE_Demo/rave/cache/6.h5', package = 'rave')
#' name <- '/wavelet/power/008'
#' dat <- load_h5(f, name)
#' dim(dat)
#' dat[,1:3]
#' }
#' @export
load_h5 <- function(file, name, read_only = T){
  LazyH5$new(file_path = file, data_name = name, read_only = read_only)
}









#' Save objects to H5 file without trivial checkings
#' @seealso \code{\link[rhdf5]{h5save}}, \code{\link{load_h5}}
#' @examples
#' \dontrun{
#' file <- tempfile()
#' x <- 1:120; dim(x) <- 2:5
#'
#' # save x to file with name /group/dataset/1
#' rave:::save_h5(x, file, '/group/dataset/1', chunk = dim(x))
#'
#' # read data
#' y <- load_h5(file, '/group/dataset/1')
#' y[]
#' }
#' @import rhdf5
#' @import stringr
#' @export
save_h5 <- function(x, file, name, chunk, level = 7,replace = FALSE, new_file = FALSE, ctype = NULL){

  # Create file if needed
  if(new_file){
    if(file.exists(file)){
      file.remove(file)
    }
  }

  if(!file.exists(file)){
    h5createFile(file)
  }

  # create group if needed
  info = h5ls(file)


  if(str_sub(name, end=1) != '/'){
    name = str_c('/', name)
  }

  group = as.vector(str_split(name, '/',simplify = T))[-1]
  gl = length(group)
  g = ''

  H5close()
  if(gl > 1){
    for(i in 1:(gl - 1)){
      flag = TRUE
      subg = group[i]
      g = str_c(g, '/', subg)
      sel = str_detect(info$otype, 'GROUP') & info$name == subg

      if(
        sum(sel) == 0 || !g %in% as.vector(sprintf('%s/%s', info$group[sel], info$name[sel]))
      ){
        suppressMessages(h5createGroup(file, g))
        logger('[HDF5] ', file, ' -> ', g, ' (Group) created.')
      }
    }
  }else{
    g = '/'
  }



  # Create dataset
  info = h5ls(file)
  dname = group[gl]

  data_exists = sum(
    # has the dataset
    (info$name == dname) &
      # in that group
      (info$group == g) &
      # is not a group
      str_detect(info$otype, 'DATASET')) > 0
  if(!data_exists){
    dim = dim(x)
    if(is.null(dim)){ dim = length(x) }

    ctype = get('ctype', envir = environment())
    if(is.null(ctype)){
      if(is.character(x)){
        ctype = 'character'
      }else if(is.numeric(x)){
        ctype = 'double'
      }else{
        logger('You MUST provide storage.mode if x is not numerics or characters. ',
               'Trying double type',
               level = 'WARNING')
        ctype = 'double'
      }
    }
    H5close()
    h5createDataset(file, dataset = name, dims = dim,
                    storage.mode = ctype, chunk = chunk, level = level)
    logger('[HDF5] ', file, ' -> ', name, ' (Dataset) created.')
  }

  H5close()

  if((data_exists & replace) || (!data_exists)){
    h5write(x, file, name)
    if(!data_exists){
      logger('[HDF5] ', file, ' -> ', name, ' (Data) saved.')
    }else{
      logger('[HDF5] ', file, ' -> ', name, ' (Data) replaced')
    }
  }

  H5close()
}







