#' @export
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  inherit = Tensor,
  public = list(
    flatten = function(include_index = T, value_name = 'value'){
      nrow = prod(self$dim)
      re = data.frame(V = as.vector(self$data))
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
    }
  )
)

#' @export
dim.Tensor <- function(obj){
  obj$dim
}

#' @export
dimnames.Tensor <- function(obj){
  obj$dimnames
}

#' @export
`[.ECoGTensor` <- function(obj, i, j, k, l){
  dim = obj$dim
  if(missing(i)){
    i = 1:dim[1]
  }
  if(missing(j)){
    j = 1:dim[2]
  }
  if(missing(k)){
    k = 1:dim[3]
  }
  if(missing(l)){
    l = 1:dim[4]
  }
  nd <- obj$data[i,j,k,l, drop = FALSE]
  dimnames = obj$dimnames
  dimnames[['Trial']] = dimnames[['Trial']][i]
  dimnames[['Frequency']] = dimnames[['Frequency']][j]
  dimnames[['Time']] = dimnames[['Time']][k]
  dimnames[['Electrode']] = dimnames[['Electrode']][l]
  rave:::ECoGTensor$new(data = nd,
                 dim = dim(nd),
                 dimnames = dimnames,
                 varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))
}

#' @export
content.Tensor <- function(obj){
  obj$data
}

#' @export
content <- function(x, ...){
  UseMethod('content')
}

#' @export
subset.Tensor <- function(obj, ..., .env = parent.frame()){
  obj$subset(...,.env = .env)
}
