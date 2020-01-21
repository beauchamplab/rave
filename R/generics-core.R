
prepend_ns <- function(arg, call){
  if(is.call(arg) && length(arg) && arg[[1]] == 'ns'){
    return(arg)
  }else{
    # Need to manually add ns
    as.call(list(quote(ns), arg))
  }
}

#' @title Get Module Runtime Environment from Current Context
#' @return An \code{\link[rave]{ExecEnvir}} instance
#' @export
getExecEnvirFromContext <- function(){
  ctx = rave_context()
  ctx$instance
}


#' @title Get Module Instance from Current Context
#' @return An \code{\link[rave]{ModuleEnvir}} instance
#' @export
getModuleEnvirFromContext <- function(){
  e = getExecEnvirFromContext()
  if(!is.null(e)){
    e = e$module_env
  }
  e
}





