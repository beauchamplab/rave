
threejsBrainOutput <- function(outputId, ...){
  f = get_from_package('threejsBrainOutput', 'threeBrain', check = F)
  if(is.function(f)){
    f(outputId = outputId, ...)
  }else{
    NULL
  }
}

renderBrain <- function(expr,env = parent.frame(), quoted = FALSE){

  if (!quoted) { expr <- substitute(expr) }

  f = get_from_package('renderBrain', 'threeBrain', check = F)
  if(is.function(f)){
    f(expr, env, quoted = TRUE)
  }
}
