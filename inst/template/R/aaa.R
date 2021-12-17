# Imports from other packages
# There are two ways to import
# First method is to import the entire package by @import pkg
# Second is to import a specific function @importFrom pkg function
#
# If your package depends heavily on one package, use the first one
# otherwise, it's recommended to use the second method to avoid potential conflicts

# Make sure to declare ALL dependencies here to make sure R can find them.
NULL

#' @import shiny
#' @import raveio
#' @import rave
#' @import dipsaus
NULL


#' Function to load all development functions within an environment
#' @param expose_functions logical indicating whether to expose all development 
#' functions to the global environment
#' @param reload logical, do you want to fast-reload the package before load the 
#' functions?
#' @export
dev_${{PACKAGE}} <- function(expose_functions = FALSE, reload = TRUE){
  .__rave_context__. = 'rave_module_debug'
  .__rave_package__. = '${{PACKAGE}}'
  if(reload){
    env <- rave::reload_module_package(expose_functions)
  }else{
    if(expose_functions){
      env = globalenv()
    }else{
      env = new.env(parent = globalenv())
    }
    rave::load_rave_module_package(env, 'rave_module_debug')
  }
  rave::rave_context(spos = 1L, tenv = globalenv())
  env
}

`%within%` <- function (a, b) {
  (a >= min(b)) & (a <= max(b))
}

finalize_installation <- function(upgrade = c("ask", "always", "never"), async = TRUE){
  upgrade <- match.arg(upgrade)
  
  dst_path <- tools::R_user_dir("${{PACKAGE}}", which = "data")
  # dir.create(dst_path, showWarnings = FALSE, recursive = TRUE)
  
  # Please write code of how you want to handle additional installations

  return(invisible())  
  
}
  
