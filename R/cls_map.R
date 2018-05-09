ls <- function(envir = parent.frame(), all.names = FALSE,
               pattern, sorted = FALSE){
  if(missing(pattern) && !sorted){
    names(as.list(envir, all.names = all.names))
  }else if (missing(pattern)){
    base::ls(..., envir = envir, all.names = all.names, sorted = sorted)
  }else{
    base::ls(..., envir = envir, all.names = all.names, pattern = pattern, sorted = sorted)
  }
}

has.class <- function(obj, class, all.match = T, element.wise = F){
  if(element.wise){
    unlist(lapply(obj, has.class, class = class, all.match = all.match, element.wise = F))
  }
  if(length(class)){
    hit = sum(class %in% class(obj))
    return((!all.match && length(hit)) || hit == length(class))
  }else{
    return(TRUE)
  }
}

#' @export
Map <- R6::R6Class(
  'Map',
  portable = FALSE,
  cloneable = F,
  private = list(
    env = NULL,
    class_type = NULL,
    .finalize = NULL
  ),
  public = list(
    finalize = function(){
      if(is.function(private$.finalize)){
        private$.finalize()
      }
    },
    initialize = function(class_type = NULL, finalize = NULL) {
      private$env <- new.env(parent=emptyenv())
      private$class_type = class_type
      private$.finalize = finalize
    },
    add = function(value){
      if(!has.class(value, class = private$class_type)){
        stop("Map: object class doesn't match")
      }
      key = digest::digest(value)
      private$env[[key]] <- value
      value
    },
    get = function(key) {
      private$env[[key]]
    },
    set = function(key, value) {
      if(!has.class(value, class = private$class_type)){
        stop("Map: object class doesn't match")
      }
      private$env[[key]] <- value
      value
    },
    mget = function(keys) {
      keys = keys[keys %in% self$keys()]
      if(length(keys)){
        base::mget(keys, private$env)
      }else{
        return(NULL)
      }
    },
    mset = function(...) {
      args <- list(...)
      args = args[has.class(value, class = private$class_type, element.wise = T)]
      if (length(args) == 0)
        return()

      arg_names <- names(args)
      if (is.null(arg_names) || any(!nzchar(arg_names)))
        stop("All elements must be named")

      list2env(args, envir = private$env)
    },
    remove = function(key) {
      if (!self$contains(key))
        return(NULL)

      result <- private$env[[key]]
      rm(list=key, envir=private$env, inherits=FALSE)
      result
    },
    contains = function(key) {
      exists(key, envir=private$env, inherits=FALSE)
    },
    keys = function() {
      ls(private$env, all.names = T)
    },
    values = function() {
      as.list(private$env, all.names=TRUE)
    },
    clear = function() {
      private$env <- new.env(parent=emptyenv())
      invisible(NULL)
    },
    size = function() {
      length(private$env)
    }
  )
)

#' @export
as.list.Map <- function(map) {
  map$values()
}

#' @export
length.Map <- function(map) {
  map$size()
}
