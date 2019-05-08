#' Internal use to fast-list all elements in an environment
#' @param envir environment
#' @param all.names include hidden objects
#' @param pattern see ls
#' @param sorted sort name? default FALSE
#' @param ... pass to base::ls
ls <- function(envir = parent.frame(), all.names = FALSE,
               pattern, sorted = FALSE, ...){
  if(missing(pattern) && !sorted){
    names(as.list(envir, all.names = all.names))
  }else if (missing(pattern)){
    base::ls(..., envir = envir, all.names = all.names, sorted = sorted)
  }else{
    base::ls(..., envir = envir, all.names = all.names, pattern = pattern, sorted = sorted)
  }
}

#' Check object has classes
#' @param obj object
#' @param class vector of classes
#' @param all.match all classes needs to be included or only need one match?
#' @param element.wise default FALSE
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

#' R6 implementation of hashMap (internal use)
Map <- R6::R6Class(
  'Map',
  portable = FALSE,
  cloneable = F,
  private = list(
    env = NULL,
    .finalize = NULL
  ),
  public = list(
    finalize = function(){
      if(is.function(private$.finalize)){
        private$.finalize()
      }
    },
    initialize = function(finalize = NULL, env = emptyenv()) {
      private$env <- new.env(parent=env)
      private$.finalize = finalize
    },
    add = function(value){
      key = digest::digest(value)
      private$env[[key]] <- value
      value
    },
    get = function(key, default = NULL) {
      if(exists(key, envir = private$env)){
        private$env[[key]]
      }else{
        default
      }
    },
    set = function(key, value) {
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
    keys = function(all.names = T) {
      ls(private$env, all.names = all.names)
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
as.list.Map <- function(x, ...) {
  x$values(...)
}

#' @export
length.Map <- function(x) {
  x$size()
}


#' A special type of Map
MVCAdapter <- R6::R6Class(
  classname = 'MVCAdapter',
  inherit = Map,
  portable = FALSE
)



#' @export
`$.MVCAdapter` <- function(obj, key){
  if(key %in% names(obj)){
    obj[[key]]
  }else{
    obj[['get']](key)
  }
}

