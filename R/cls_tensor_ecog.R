#' ECoG Tensor class inherit from Tensor
#' @export
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  inherit = Tensor,
  public = list(
    flatten = function(include_index = T, value_name = 'value'){
      nrow = prod(self$dim)
      re = data.frame(V = as.vector(self$get_data()))
      names(re) = value_name
      if(include_index){
        for(i in 1:length(self$varnames)){
          vn = self$varnames[i]
          if(i > 1){
            each = prod(self$dim[1: (i - 1)])
          }else{
            each = 1
          }
          times = nrow / self$dim[i] / each

          re[[vn]] = rep(self$dimnames[[i]], each = each, times = times)
          if(i == 1){
            re[['Trial_Number']] = rep(1:self$dim[1], each = 1, times = times)
          }
        }
        re = cbind(re[-1], re[1])
      }
      re
    },
    initialize = function(data, dim, dimnames, varnames, hybrid = F,
                          swap_file = tempfile(), temporary = TRUE,
                          multi_files = FALSE, use_index = TRUE, ...){
      self$temporary = temporary
      # get attributes of data
      dim %?<-% base::dim(data)
      dim %?<-% length(data)
      dimnames %?<-% base::dimnames(data)
      dimnames %?<-% lapply(1:length(varnames), function(v){ seq_len(dim[v]) })

      names(dimnames) = varnames

      self$last_used = Sys.time()
      self$dimnames = dimnames
      self$dim = dim




      tryCatch({
        if('Frequency' %in% varnames){
          self$dimnames$Frequency = as.numeric(self$dimnames$Frequency)
        }
      }, error = function(e){})
      tryCatch({
        if('Time' %in% varnames){
          self$dimnames$Time = as.numeric(self$dimnames$Time)
        }
      }, error = function(e){})
      tryCatch({
        if('Electrode' %in% varnames){
          self$dimnames$Electrode = as.numeric(self$dimnames$Electrode)
        }
      }, error = function(e){})

      super$initialize(
        data = data, dim = dim, dimnames = dimnames, varnames = varnames, hybrid = hybrid,
        swap_file = swap_file, temporary = temporary,
        multi_files = multi_files, use_index = use_index, ...
      )
      rm(data)

      # private$.data = data
      #
      # self$hybrid = hybrid
      # self$use_index = T
      #
      # self$swap_file = swap_file

      # to_swap
      if(hybrid){
        self$to_swap_now(use_index = T)
      }
    }
  )
)


join_tensors <- function(tensors, temporary = TRUE){
  # Join tensors by the last dim. This is a quick and dirty way - doesn't do any checks
  if(!length(tensors)){
    return(NULL)
  }

  dim = dim(tensors[[1]])
  n_dims = length(dim)
  dimnames = dimnames(tensors[[1]])
  last_dnames = unlist(lapply(tensors, function(tensor){
    tensor$dimnames[[n_dims]]
  }))
  dimnames[[n_dims]] = last_dnames
  dim[n_dims] = length(last_dnames)

  swap_files = unlist(lapply(tensors, function(tensor){
    # swap!
    tensor$to_swap_now(use_index = F)

    tensor$swap_file
  }))



  cls = Tensor
  if('ECoGTensor' %in% class(tensors[[1]])){
    cls = ECoGTensor
  }

  varnames = names(dimnames)
  re = cls$new(data = 1, dim = rep(1, n_dims),
               dimnames = sapply(varnames, function(nm){1}, simplify = F, USE.NAMES = T),
               varnames = varnames, hybrid = FALSE)
  re$swap_file = swap_files
  re$.__enclos_env__$private$multi_files = TRUE
  re$hybrid = TRUE
  re$.__enclos_env__$private$.data = NULL
  re$dim = dim
  re$dimnames = dimnames
  re$temporary = temporary
  re
}
