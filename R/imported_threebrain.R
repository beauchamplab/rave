
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


save_threebrain <- function(widget, directory, filename = 'index.html', title = '3D Viewer', as_zip = FALSE){
  f = get_from_package('save_brain', 'threeBrain', check = F)
  f(widget, directory, filename, title, as_zip)
}
