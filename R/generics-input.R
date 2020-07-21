
#' @title Get `shiny' "input" and "output" objects under current context
#' @return In shiny context, returns special \code{\link[shiny]{reactiveValues}}
#' that refers to the inputs and outputs of shiny applications. In non-shiny 
#' contexts, returns a fake environment related to current fake session, 
#' for debug purpose.
#' @param session shiny session instance
#' @name session-reactives
NULL

#' @rdname session-reactives
#' @export
getDefaultReactiveInput <- rave_context_generics('getDefaultReactiveInput', 
                                                 function(session){})
#' @rdname session-reactives
#' @export
getDefaultReactiveInput.default <- function(session){
  stop('Please enable debug mode to test this function.')
}

#' @rdname session-reactives
#' @export
getDefaultReactiveInput.rave_module_debug <- function(session){
  env = new.env(parent = emptyenv())
  env$..warn = TRUE
  class(env) = c('ravedev_ReactiveInput', 'environment')
  env
}

#' @rdname session-reactives
#' @export
getDefaultReactiveInput.rave_running <- function(
  session = shiny::getDefaultReactiveDomain()
){
  ctx = rave_context()
  # session = shiny::getDefaultReactiveDomain()
  # session = session$makeScope(ctx$module_id)
  session$makeScope(ctx$module_id)$input
}

#' @rdname session-reactives
#' @export
getDefaultReactiveInput.rave_running_local <- getDefaultReactiveInput.rave_module_debug


#' @export
print.ravedev_ReactiveInput <- function(x, ...){
  catgl('<Reactive Input> (Read-only)', level = 'INFO')
  for(k in ls(x, all.names = FALSE)){
    catgl(' ', k, '= ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
    s = paste(deparse(x[[k]]), sep = '\n\t')
    catgl(s, level = 'INFO', pal = list('INFO' = 'dodgerblue3'), sep = '\n\t')
  }
  invisible(x)
}
.ravedev_ReactiveInput_assign <- function(x, i, value){
  if(isTRUE(x$..warn)){
    assign('..warn', FALSE, envir = x)
    catgl('$<-, or [[<- type of assignment only works for debug purpose.\n  (This warning only display once for this object)', level = 'WARNING')
  }
  
  assign(i, value, envir = x)
  invisible(x)
}
#' @export
`$<-.ravedev_ReactiveInput` <- .ravedev_ReactiveInput_assign
#' @export
`[[<-.ravedev_ReactiveInput` <- .ravedev_ReactiveInput_assign
#' @export
`$.ravedev_ReactiveInput` <- function(x, name){
  get0(name, envir = parent.frame(), inherits = TRUE, ifnotfound = NULL)
}
#' @export
`[[.ravedev_ReactiveInput` <- `$.ravedev_ReactiveInput`
