#' File IO: HDF5 file wrapper
#' @export
LazyH5 <- R6::R6Class(
  classname = 'LazyH5',
  portable = F,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL
  ),
  public = list(
    finalize = function(){
      self$close(all = TRUE)
    },
    print = function(){
      if(!is.null(private$data_ptr)){
        if(private$data_ptr$is_valid){
          base::print(private$data_ptr)
        }else{
          base::cat('Pointer closed. Information since last open:\nDim: ', paste(private$last_dim, collapse = 'x'), ' \tRank: ', length(private$last_dim))
        }
      }
      invisible(self)
    },
    initialize = function(file_path, data_name, read_only = F){
      
      # First get absolute path, otherwise hdf5r may report file not found error
      if(read_only){
        private$file = base::normalizePath(file_path)
        
        stopifnot2(
          hdf5r::is_hdf5(private$file),
          msg = 'File doesn\'t have H5 format'
        )
      }else{
        private$file = file_path
      }
      private$name = data_name
      private$read_only = read_only
    },
    
    save = function(x, chunk = 'auto', level = 7, replace = TRUE, new_file = FALSE,
                    force = TRUE, ctype = NULL, size = NULL, ...){
      # ctype and size is deprecated but kept in case of compatibility issues
      # ptr$create_dataset =
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      if(private$read_only){
        if(!force){
          stop('File is read-only. Use "force=TRUE"')
        }else{
          # Close current pointer
          self$close(all = TRUE)
          private$read_only = F
          
          on.exit({
            private$read_only = T
          }, add = T, after = F)
        }
      }
      
      if(new_file && file.exists(private$file)){
        self$close(all = TRUE)
        file.remove(private$file)
      }
      
      self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)
      
      self$close(all = TRUE)
      
    },
    
    open = function(new_dataset = F, robj, ...){
      
      # check data pointer
      # if valid, no need to do anything, otherwise, enter if clause
      if(new_dataset || is.null(private$data_ptr) || !private$data_ptr$is_valid){
        
        # Check if file is valid,
        if(is.null(private$file_ptr) || !private$file_ptr$is_valid){
          # if no, create new link
          mode = ifelse(private$read_only, 'r', 'a')
          tryCatch({
            private$file_ptr = hdf5r::H5File$new(private$file, mode)
          }, error = function(e){
            # Open for writting, we should close all connections first
            # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
            # will lock the file for writting
            f = hdf5r::H5File$new(private$file, 'r')
            cat2('Closing all other connections to [', private$file, '] - ', f$get_obj_count() - 1)
            f$close_all()
            private$file_ptr = hdf5r::H5File$new(private$file, mode)
          })
        }
        
        has_data = private$file_ptr$path_valid(private$name)
        
        if(!private$read_only && (new_dataset || ! has_data)){
          # need to create new dataset
          g = stringr::str_split(private$name, '/', simplify = T)
          g = g[stringr::str_trim(g) != '']
          
          ptr = private$file_ptr
          nm = ''
          
          for(i in g[-length(g)]){
            nm = sprintf('%s/%s', nm, i)
            if(!ptr$path_valid(path = nm)){
              ptr = ptr$create_group(i)
              cat2(private$file, ' => ', nm, ' (Group Created)\n')
            }else{
              ptr = ptr[[i]]
            }
          }
          
          # create dataset
          nm = g[length(g)]
          if(ptr$path_valid(path = nm)){
            # dataset exists, unlink first
            cat2(private$file, ' => ', private$name, ' (Dataset Removed)\n')
            ptr$link_delete(nm)
          }
          # new create
          cat2(private$file, ' => ', private$name, ' (Dataset Created)\n')
          ptr$create_dataset(nm, robj = robj, ...)
          if(ptr$is_valid && inherits(ptr, 'H5Group')){
            ptr$close()
          }
        }else if(!has_data){
          stop('File [', private$file, '] has no [', private$name, '] in it.')
        }
        
        private$data_ptr = private$file_ptr[[private$name]]
        
      }
      
      private$last_dim = private$data_ptr$dims
      
    },
    
    close = function(all = FALSE){
      # check if data link is valid
      if(!is.null(private$data_ptr) && private$data_ptr$is_valid){
        private$data_ptr$close()
      }
      
      # if file link is valid, get_obj_ids() should return a vector of 1
      if(all && !is.null(private$file_ptr) && private$file_ptr$is_valid){
        private$file_ptr$close_all()
      }
    },
    
    subset = function(
      ...,
      drop = FALSE, stream = F,
      envir = parent.frame()
    ) {
      self$open()
      dims = self$get_dims()
      
      # step 1: eval indices
      args = eval(substitute(alist(...)))
      if(length(args) == 0 || (length(args) == 1 && args[[1]] == '')){
        return(private$data_ptr$read())
      }
      args = lapply(args, function(x){
        if(x == ''){
          return(x)
        }else{
          return(eval(x, envir = envir))
        }
      })
      
      # step 2: get allocation size
      sapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(sum(args[[ii]]))
        }else if(is.numeric(args[[ii]])){
          return(length(args[[ii]]))
        }else{
          # must be blank '', otherwise raise error
          return(dims[ii])
        }
      }) ->
        alloc_dim
      
      # step 3: get legit indices
      lapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(args[[ii]])
        }else if(is.numeric(args[[ii]])){
          return(
            args[[ii]][args[[ii]] <= dims[ii] & args[[ii]] > 0]
          )
        }else{
          return(args[[ii]])
        }
      }) ->
        legit_args
      
      # step 4: get mapping
      lapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(
            rep(T, sum(args[[ii]]))
          )
        }else if(is.numeric(args[[ii]])){
          return(args[[ii]] <= dims[ii] & args[[ii]] > 0)
        }else{
          return(args[[ii]])
        }
      }) ->
        mapping
      
      # alloc space
      re = array(NA, dim = alloc_dim)
      
      if(stream){
        re = do.call(`[<-`, c(list(re), mapping, list(
          value = private$data_ptr$read(
            args = legit_args,
            drop = F,
            envir = environment()
          )
        )))
      }else{
        re = do.call(`[<-`, c(list(re), mapping, list(
          value = do.call('[', c(list(private$data_ptr$read()), legit_args, list(drop = F)))
        )))
      }
      
      self$close(all = !private$read_only)
      
      
      if(drop){
        return(drop(re))
      }else{
        return(re)
      }
    },
    
    get_dims = function(stay_open = T){
      self$open()
      re = private$data_ptr$dims
      if(!stay_open){
        self$close(all = !private$read_only)
      }
      re
    }
  )
)


#' @export
`[.LazyH5` <- function(obj, ...){
  on.exit({obj$close()}, add = T)
  obj$subset(..., envir = parent.frame())
}

#' @export
`+.LazyH5` <- function(a, b){
  b + a$subset()
}

#' @export
`-.LazyH5` <- function(a, b){
  -(b - a$subset())
}

#' @export
`*.LazyH5` <- function(a, b){
  b * (a$subset())
}

#' @export
`/.LazyH5` <- function(a, b){
  if(inherits(b, 'LazyH5')){
    b = b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyH5 <- function(x){
  dim_info = x$get_dims(stay_open = F)
  if(length(dim_info) == 1){
    dim_info = NULL
  }
  dim_info
}

#' @export
length.LazyH5 <- function(x){
  dim_info = x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5 <- function(x, ...){
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyH5 <- function(z){
  base::Mod(z$subset())
}

#' @export
Arg.LazyH5 <- function(z){
  base::Arg(z$subset())
}


#' @export
exp.LazyH5 <- function(x){
  base::exp(x$subset())
}







