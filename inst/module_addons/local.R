
observe = function(...){}
observeEvent = function(...){}

getDefaultReactiveDomain <- function(){
  rave::fake_session()
}

reactiveValues <- function(...){
  env = new.env(parent = emptyenv())
  list2env(list(...), env)
  class(env) = c('dev_raveReactiveValues', 'environment')
  env
}



trigger_recalculate <- function(){
  cat2('Re-calculate will be triggered.')
}

register_auto_calculate_widget <- function(...){
  dipsaus::cat2('Debug mode')
}


