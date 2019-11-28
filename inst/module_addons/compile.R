
is_local_debug <- function(){
  is.null(shiny::getDefaultReactiveDomain())
}

getDefaultReactiveDomain <- function(){
  if(is_local_debug()){
    rave:::fake_session()
  }else{
    e = getCurrentExecEnvir()
    e$wrapper_env$getDefaultReactiveDomain()
  }
}




print.dev_ReactiveValues <- function(x){
  cat2('<Reactive Values> (Write-only)', level = 'INFO')
  for(k in ls(x, all.names = FALSE)){
    cat2(' ', k, '= ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
    s = paste(deparse(x[[k]]), sep = '\n\t')
    cat2(s, level = 'INFO', pal = list('INFO' = 'dodgerblue3'), sep = '\n\t')
  }
  invisible(x)
}


reactiveValues <- function(...){
  if(is_local_debug()){
    env = new.env(parent = emptyenv())
    list2env(list(...), env)

    class(env) = c('dev_ReactiveValues', 'environment')
    env
  }else{
    shiny::reactiveValues(...)
  }
}



trigger_recalculate <- function(){
  cat2('Re-calculate will be triggered.')
}

register_auto_calculate_widget <- function(...){
  dipsaus::cat2('Debug mode')
}


