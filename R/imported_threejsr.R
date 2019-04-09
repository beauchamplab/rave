#' Wrapper for "renderThreejs" (threejsr)
#' @param expr R expression
#' @param env environment to evaluate expr
#' @param quoted is expr quoted
renderThreejs <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) { expr <- substitute(expr) }
  f = get_from_package('renderThreejs', pkg = 'threejsr', check = FALSE)
  f(expr, env, quoted = TRUE)
}

#' Wrapper for "read.freesurf.asc" (threejsr)
#' @param file freesurf file path
read.freesurf.asc <- function(file){
  f = get_from_package('read.freesurf.asc', pkg = 'threejsr', check = FALSE)
  f(file)
}


#' Wrapper for "read.freesurf.gii" (threejsr)
#' @param file freesurf file path
read.freesurf.gii <- function(file){
  f = get_from_package('read.freesurf.gii', pkg = 'threejsr', check = FALSE)
  f(file)
}

#' Wrapper for "threejsOutput" (threejsr)
#' @param outputId Output ID
#' @param ... passed to threejsr::threejsOutput
threejsOutput <- function(outputId, ...){
  f = get_from_package('threejsOutput', pkg = 'threejsr', check = FALSE)
  f(outputId, ...)
}


#' Wrapper for "threejs_scene" (threejsr)
#' @param ... passed to threejsr::threejs_scene
threejs_scene <- function(...){
  f = get_from_package('threejs_scene', pkg = 'threejsr', check = FALSE)
  f(...)
}
