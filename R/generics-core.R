
prepend_ns <- function(arg, call){
  if(is.call(arg) && length(arg) && arg[[1]] == 'ns'){
    return(arg)
  }else{
    # Need to manually add ns
    as.call(list(quote(ns), arg))
  }
}


#' @export
getExecEnvirFromContext <- function(){
  ctx = rave_context()
  ctx$instance
}

#' @export
getModuleEnvirFromContext <- function(){
  e = getExecEnvirFromContext()
  if(!is.null(e)){
    e = e$module_env
  }
  e
}





