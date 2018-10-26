# fst IO


LazyFST <- R6::R6Class(
  classname = 'LazyFST',
  private = list(
    file_path = NULL,
    transpose = F,
    meta = NULL,
    dims = NULL,
    data = NULL,
    last_visited = NULL,
    delayed = 3
  ),
  public = list(
    open = function(...){},
    close = function(..., .remove_file = F){
      if(.remove_file){
        unlink(private$file_path)
      }
    },
    save = function(...){
      warning('NOT Implemented yet')
    },
    initialize = function(file_path, transpose = F, dims = NULL, ...){
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
    get_dims = function(...){
      private$dims
    },
    subset = function(i = NULL, j = NULL, ..., drop = T){
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
        dat = as.matrix(fst::read_fst(private$file_path, columns = col_names))
        dat = dat[j[real_j], ]
        re[real_i, real_j] = t(dat)
      }else{
        col_names = private$meta$columnNames[j[real_j]]
        dat = as.matrix(fst::read_fst(private$file_path, columns = col_names))
        dat = dat[i[real_i], ]
        re[real_i, real_j] = dat
      }
      rm(dat)
      gc()

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
  if(is(b, 'LazyFST')){
    b = b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyFST <- function(obj){
  dim_info = obj$get_dims()
  if(length(dim_info) == 1){
    dim_info = NULL
  }
  dim_info
}

#' @export
length.LazyFST <- function(obj){
  dim_info = obj$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyFST <- function(obj, ...){
  as.array(obj$subset(), ...)
}

#' @export
Mod.LazyFST <- function(obj){
  base::Mod(obj$subset())
}

#' @export
Arg.LazyFST <- function(obj){
  base::Arg(obj$subset())
}


#' @export
exp.LazyFST <- function(obj){
  base::exp(obj$subset())
}




load_fst_or_h5 <- function(
  fst_path, h5_path, h5_name, fst_need_transpose = F, fst_need_drop = F, ram = F
){
  # check if fst_path exists
  if(file.exists(fst_path)){
    if(ram){
      re = as.matrix(fst::read_fst(fst_path))
      dimnames(re) = NULL
      if(fst_need_transpose){
        re = t(re)
      }
      if(fst_need_drop){
        re = drop(re)
      }
      return(re)
    }else{
      re = rave:::LazyFST$new(file_path = fst_path, transpose = fst_need_transpose)
      return(re)
    }
  }else{
    re = load_h5(file = h5_path, name = h5_name, read_only = T, ram = ram)
    return(re)
  }
}
