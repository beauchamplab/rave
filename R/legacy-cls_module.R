# Environment for ECoG data and modules
# As of rave-Ent, data_repository nolonger succeed from globalenv()
# Instead, its parent is now baseenv()
# All packages needed are imported via loadnamespace within modules
# This will help create a clean environment for modules.
NULL

getDefaultReactiveDomain <- function(session = shiny::getDefaultReactiveDomain()){
  session %?<-% get0('session', envir = globalenv())
  return(session)
}


data_repository <- new.env(parent = baseenv())

#' Get environment where subject data is loaded
#' @param session shiny session, default is NULL
#' @param session_id internal use
#' @param session_based internal use
#' @export
getDefaultDataRepository <- function(
  session = getDefaultReactiveDomain(),
  session_id,
  session_based = NULL
){
  data_repository
}

#' Attach subject data
#' @param unload TRUE if you want to detach
#' @param rave_data internally used
#' @export
attachDefaultDataRepository <- function(
  unload = TRUE, rave_data = getDefaultDataRepository()){
  
  if(unload){
    try({detach('rave_data')}, silent = TRUE)
  }

  rave_idx = which(search() == "package:rave")

  if(length(rave_idx)){
    do.call('attach', list(rave_data, name = 'rave_data', pos = rave_idx))
  }else{
    do.call('attach', list(rave_data, name = 'rave_data'))
  }
}



#' Functions for development use
#' @param ... Expressions
#' @export
rave_ignore <- function(...){
  quos = rlang::quos(...)
  for(i in 1:length(quos)){
    catgl('> ', rlang::quo_squash(quos[[i]]), level = 'INFO')
    dipsaus::eval_dirty(quos[[i]], globalenv())
  }
}


rave_inputs <- function(..., .input_panels = list(), 
                        .tabsets = list(), .env = globalenv(),
                        .manual_inputs = NULL, .render_inputs = NULL){
  quos = rlang::quos(...)
  parser = comp_parser()
  lapply(quos, function(quo){
    comp = parser$parse_quo(quo)
    value = eval(comp$initial_value, envir = .env)
    inputId = comp$inputId
    .env[[inputId]] = value

    return(list(inputId = inputId, value = value))
  }) ->
    re
  nms = lapply(re, function(x){x$inputId})
  vals = lapply(re, function(x){x$value})
  names(vals) = nms
  .env[['.tmp_init']] = vals
  invisible(vals)
}

rave_outputs <- function(..., .output_tabsets = list()){
  # do nothing
  return(invisible())
}


rave_updates <- function(..., .env = globalenv()){

  res = rlang::quos(...)
  nms = names(res)
  if(length(nms) == 0){
    return()
  }
  lapply(res[nms == ''], function(quo){
    dipsaus::eval_dirty(quo, env = .env)
  })

  nms = nms[nms != '']

  # parser = comp_parser()
  for(nm in nms){
    val = dipsaus::eval_dirty(res[[nm]], env = .env)
    try({
      re = val$value
      re %?<-% val$selected
      .env[[nm]] = re
    })
  }

  invisible(res)

}


rave_execute <- function(..., auto = TRUE, .env = globalenv()){
  soft_deprecated()
  
  assign('.is_async', TRUE, envir = .env)
  quos = rlang::quos(...)

  for( quo in quos ){
    catgl('> ', rlang::quo_squash(quo), level = 'INFO')
    dipsaus::eval_dirty(quo, .env)
  }

}


# Get x or default
async_var <- function(x, default = NULL){
  tryCatch({
    if(is.null(x)){
      re = default
    }else{
      re = x
    }
    re
  }, error = function(e){
    default
  }) ->
    re
  re
}


export_report <- function(expr, inputId){

}


