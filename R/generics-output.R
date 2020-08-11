
#' @rdname session-reactives
#' @export
getDefaultReactiveOutput <- rave_context_generics(
  'getDefaultReactiveOutput', 
  function(session = shiny::getDefaultReactiveDomain()){
    
  })

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.default <- function(session = shiny::getDefaultReactiveDomain()){
  stop('Please enable debug mode to test this function.')
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_module_debug <- function(session = shiny::getDefaultReactiveDomain()){
  env = new.env(parent = emptyenv())
  env$..warn = TRUE
  class(env) = c('ravedev_ReactiveOutput', 'environment')
  env
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_running <- function(session = shiny::getDefaultReactiveDomain()){
  ctx = rave_context()
  session$rootScope()$makeScope(ctx$module_id)$output
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_running_local <- getDefaultReactiveOutput.rave_module_debug


#' @export
print.ravedev_ReactiveOutput <- function(x, ...){
  catgl('<Reactive Output> (Write-only)', level = 'INFO')
  for(k in ls(x, all.names = FALSE)){
    catgl(' ', k, '= ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
    s = paste(deparse(x[[k]]), sep = '\n\t')
    catgl(s, level = 'INFO', pal = list('INFO' = 'dodgerblue3'), sep = '\n\t')
  }
  invisible(x)
}
.ravedev_ReactiveOutput_assign <- function(x, i, value){
  value = substitute(value)
  assign(i, value, envir = x)
  invisible(x)
}
#' @export
`$<-.ravedev_ReactiveOutput` <- .ravedev_ReactiveOutput_assign
#' @export
`[[<-.ravedev_ReactiveOutput` <- .ravedev_ReactiveOutput_assign

