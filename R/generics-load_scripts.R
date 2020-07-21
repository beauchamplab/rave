#' @title Load scripts that cannot put into package R folder
#' @description Use in \code{comp.R} to load scripts that cannot be put into 
#' package \code{"R/"} folder. Usually the scripts contains shiny reactive
#' values that changes dynamically. 
#' @param ... script files that are wrapped by \code{\link[rave]{get_path}}, 
#' or R quasi-quotations wrapped by \code{\link[rlang]{quo}}.
#' @param asis if the scripts to be loaded is a file, whether to copy to a
#' temporary directory when launching 'RAVE'. Usually we set this to be true
#' to save loading time. However, if your scripts also source other scripts 
#' in relative path, we recommend setting \code{asis=FALSE} and also load 
#' additional scripts using this function.
#' @details This function raises error when running in default contexts, and 
#' requires debug mode, or run inside of 'RAVE' instance.
#' @return None, but will source, or run whatever code provided.
#' @seealso \code{\link[rave]{rave_context}}, \code{\link[rave]{get_path}},
#' \code{\link[rlang]{quo}}
#' @export
load_scripts <- rave_context_generics('load_scripts', function(..., asis = FALSE){})

#' @export
load_scripts.default <- function(...){
  stop('Please enable debug mode to test this function.')
}

#' @export
load_scripts.rave_module_debug <- function(..., asis = FALSE){
  src = c(...)
  
  mount_demo_subject()
  
  for(s in src){
    if(rlang::is_quosure(s)){
      catgl('Executing a dynamic script...\n', level = 'INFO', sep = '')
      dipsaus::eval_dirty(s, globalenv())
    }else{
      catgl('Loading source - ', s, '\n', level = 'INFO', sep = '')
      source(get_path(s), local = FALSE)
    }
  }
  invisible()
}

#' @export
load_scripts.rave_running <- function(..., asis = FALSE){
  rave_context()
  
  fs = unlist(list(...))
  fs = sapply(fs, function(x){
    if(rlang::is_quosure(x)){ x }else{
      get_path(x) 
    }
  })
  
  scripts = get('...scripts', envir = parent.frame(), inherits = TRUE)
  scripts[['source']] = c(scripts[['source']], fs)
  
  if(is.null(scripts[['asis']])){
    scripts[['asis']] = asis
  }
}

#' @export
load_scripts.rave_running_local = load_scripts.rave_running
