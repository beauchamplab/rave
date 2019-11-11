# Finalize stuff when the session ends

RaveFinalizer <- R6::R6Class(
  classname = 'RaveFinalizer',
  cloneable = FALSE,
  portable = TRUE,
  parent_env = ..setup_env,
  public = list(
    .fin = NULL,
    files = NULL,
    finalize = function(){
      if(is.function(self$.fin)){
        try({
          self$.fin()
        }, silent = TRUE)
      }
      
      # unlink files
      if(length(self$files)){
        lapply(self$files, unlink, recursive = TRUE)
      }
      
    },
    initialize = function(fun){
      self$.fin = fun
      
      # Trick: make this class persistent so that it won't trigger gc()
      # unless R session closed
      ..setup_env$finalizers[[length(..setup_env$finalizers) + 1]] = self
    }
  )
)
