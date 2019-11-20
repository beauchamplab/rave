# Environment for ECoG data and modules
# As of rave-Ent, data_repository nolonger succeed from globalenv()
# Instead, its parent is now baseenv()
# All packages needed are imported via loadnamespace within modules
# This will help create a clean environment for modules.
NULL

#' Wrapper for shiny::getDefaultReactiveDomain
getDefaultReactiveDomain <- function(){
  session = shiny::getDefaultReactiveDomain()
  session %?<-% get0('session', envir = globalenv())
  return(session)
}



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
  
  # get namespace
  data_repository = namespace::getRegisteredNamespace('rave:data')
  if(is.null(data_repository)){
    data_repository = new.env(parent = asNamespace('rave'))
    data_repository$.clean = function(){}
    namespace::registerNamespace(name = 'rave:data', env = data_repository)
  }
  
  return(data_repository)
}

#' Attach subject data
#' @param unload TRUE if you want to detach
#' @export
attachDefaultDataRepository <- function(unload = T){
  if(unload){
    try({detach('rave_data')}, silent = T)
  }

  rave_data = getDefaultDataRepository()

  rave_idx = which(search() == "package:rave")

  if(length(rave_idx)){
    do.call('attach', list(rave_data, name = 'rave_data', pos = rave_idx))
  }else{
    do.call('attach', list(rave_data, name = 'rave_data'))
  }
}



#' Functions for dev use
#' @param ... Expressions
#' @export
rave_ignore <- function(...){
  quos = rlang::quos(...)
  for(i in 1:length(quos)){
    cat2('> ', rlang::quo_squash(quos[[i]]), level = 'INFO')
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
    cat2('> ', rlang::quo_squash(quo), level = 'INFO')
    dipsaus::eval_dirty(quo, .env)
  }

}



#' Cache input values
#' @param inputId input ID
#' @param val value if not cached
#' @param read_only logical, if FALSE, replace cache
#' @export
cache_input <- function(inputId, val, read_only = T){
  return(val)
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


#' Get Cache Environment
#' @param session internally used
#' @export
getDefaultCacheEnvironment <- function(
  session = getDefaultReactiveDomain()
){
  session_id = add_to_session(session)
  session_id %?<-% '.TEMP'
  global_env = globalenv()
  if(!is.environment(global_env[['.cache_rave']])){
    global_env[['.cache_rave']] = new.env(parent = emptyenv())
  }
  global_env[['.cache_rave']][[session_id]] %?<-% new.env(parent = emptyenv())
  return(global_env[['.cache_rave']][[session_id]])
}





