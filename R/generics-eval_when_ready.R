#' @title Add Function to run once Module is Ready
#' @description Usually contains reactive functions that requires shiny reactive
#' context
#' @param FUN function that takes an environment (runtime environment) as
#' arguments.
#' @export
eval_when_ready <- rave_context_generics('eval_when_ready', function(FUN){})

#' @export
eval_when_ready.default <- function(FUN){
  stop('Please enable debug mode to test this function.')
}

#' @export
eval_when_ready.rave_module_debug <- function(FUN){
  rave_context()
  env = parent.frame()
  FUN(env)
}


#' @export
eval_when_ready.rave_running <- function(FUN){
  ctx = rave_context()
  e = ctx$instance
  if(is.function(FUN)){
    e$ready_functions[[length(e$ready_functions) + 1]] = FUN
  }
}

#' @export
eval_when_ready.rave_running_local <- function(FUN){
  invisible()
}

