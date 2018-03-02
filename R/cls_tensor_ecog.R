#' @export
#' @exportClass ECoGTensor
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  inherit = Tensor
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
subset.Tensor <- function(obj, ...){
  obj$subset(...)
}
