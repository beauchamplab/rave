#' @import rhdf5
LazyH5 <- R6::R6Class(
  classname = 'LazyH5',
  portable = F,
  cloneable = F,
  public = list(
    data = NULL,
    ls = NULL,
    initialize = function(file){
      self$data = new.env()
      self$ls = h5ls(file)
    }
  )
)
