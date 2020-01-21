
#' @rdname session-reactives
#' @export
getDefaultReactiveOutput <- rave_context_generics('getDefaultReactiveOutput', function(){})

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.default <- function(){
  stop('Please enable debug mode to test this function.')
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_module_debug <- function(){
  env = new.env(parent = emptyenv())
  env$..warn = TRUE
  class(env) = c('ravedev_ReactiveOutput', 'environment')
  env
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_running <- function(){
  ctx = rave_context()
  session = shiny::getDefaultReactiveDomain()
  session = session$makeScope(ctx$module_id)
  session$output
}

#' @rdname session-reactives
#' @export
getDefaultReactiveOutput.rave_running_local <- getDefaultReactiveOutput.rave_module_debug


#' @export
print.ravedev_ReactiveOutput <- function(x, ...){
  cat2('<Reactive Output> (Write-only)', level = 'INFO')
  for(k in ls(x, all.names = FALSE)){
    cat2(' ', k, '= ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
    s = paste(deparse(x[[k]]), sep = '\n\t')
    cat2(s, level = 'INFO', pal = list('INFO' = 'dodgerblue3'), sep = '\n\t')
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

