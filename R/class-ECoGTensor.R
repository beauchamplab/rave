# Documented on 2019-10-11

#' @title ECoG Tensor class inherit from \code{\link[rave]{Tensor}}
#' @author Zhengjia Wang
#' @description Four-mode tensor (array) especially designed for 
#' \code{iEEG/ECoG} data. The Dimension names are: \code{Trial}, 
#' \code{Frequency}, \code{Time}, and \code{Electrode}. 
#' @export
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  inherit = Tensor,
  public = list(
    
    #' @description converts tensor (array) to a table (data frame)
    #' @param include_index logical, whether to include dimension names
    #' @param value_name character, column name of the value
    #' @return a data frame with the dimension names as index columns and 
    #' \code{value_name} as value column
    flatten = function(include_index = TRUE, value_name = 'value'){
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
    
    #' @description constructor
    #' @param data array or vector
    #' @param dim dimension of data, mush match with \code{data}
    #' @param dimnames list of dimension names, equal length as \code{dim}
    #' @param varnames names of \code{dimnames}, recommended names are: 
    #' \code{Trial}, \code{Frequency}, \code{Time}, and \code{Electrode}
    #' @param hybrid whether to enable hybrid mode to reduce RAM usage
    #' @param swap_file if hybrid mode, where to store the data
    #' @param temporary whether to clean up the space when exiting R session
    #' @param multi_files logical, whether to use multiple files instead of 
    #' one giant file to store data
    #' @param use_index logical, when \code{multi_files} is true, whether use 
    #' index dimension as partition number
    #' @param ... further passed to \code{\link[rave]{Tensor}} constructor
    #' @return an \code{ECoGTensor} instance
    initialize = function(data, dim, dimnames, varnames, hybrid = FALSE,
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
        self$to_swap_now(use_index = use_index)
      }
    }
  )
)


#' @title Join Multiple Tensors into One Tensor
#' @author Zhengjia Wang
#' @param tensors list of \code{\link[rave]{Tensor}} instances
#' @param temporary whether to garbage collect space when exiting R session
#' @return A new \code{\link[rave]{Tensor}} instance with the last dimension
#' @details Merges multiple tensors. Each tensor must share the same dimension 
#' with the last one dimension as 1, for example, \code{100x100x1}. Join 3 
#' tensors like this will result in a \code{100x100x3} tensor. This function 
#' is handy when each sub-tensors are generated separately. However, it does no 
#' validation test. Use with cautions.
#' @examples 
#' tensor1 <- Tensor$new(data = 1:9, c(3,3,1), dimnames = list(
#' A = 1:3, B = 1:3, C = 1
#' ), varnames = c('A', 'B', 'C'))
#' tensor2 <- Tensor$new(data = 10:18, c(3,3,1), dimnames = list(
#'   A = 1:3, B = 1:3, C = 2
#' ), varnames = c('A', 'B', 'C'))
#' merged <- join_tensors(list(tensor1, tensor2))
#' merged$get_data()
#' 
#' @export
join_tensors <- function(tensors, temporary = TRUE){
  # Join tensors by the last dim. This is a quick and dirty way - doesn't 
  # do any checks
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
  re$.use_multi_files(TRUE)
  re$hybrid = TRUE
  re$set_data(NULL)
  re$dim = dim
  re$dimnames = dimnames
  re$temporary = temporary
  re
}
