RaveFinalizer <- R6::R6Class(
  classname = 'RaveFinalizer',
  public = list(
    .fin = NULL,
    finalize = function(){
      try({
        self$.fin()
      }, silent = TRUE)
    },
    initialize = function(fun){
      self$.fin = fun

      # Trick: make this class persistent so that it won't trigger gc()
      # unless R session closed
      ..setup_env$finalizers %?<-% list()
      ..setup_env$finalizers[[length(..setup_env$finalizers) + 1]] = self
    }
  )
)
