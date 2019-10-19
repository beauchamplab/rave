# Function to bind functions to exec_env's wrappers
# e is the execenv and w is its wrapper
bind_wrapper_env <- function(self, w, shiny_mode = TRUE){
  w$async_var = function(x, default = NULL){
    x_name = deparse(substitute(x))
    val = NULL
    future_env = self$param_env[['..rave_future_env']]
    if(is.environment(future_env) || is.list(future_env)){
      val = future_env[[x_name]]

      if(!is.null(val)){
        return(val)
      }
    }
    return(default)
  }

  w$reloadUI = function(){
    self$reload()
  }

  w$launch_selector = function(){
    self$global_reactives$launch_selector = Sys.time()
  }

  w$monitor_subject_change = function(){
    
    if(shiny::is.reactivevalues(self$local_reactives)){
      if( self$local_reactives$initialized && self$local_reactives$has_data && self$local_reactives$focused ){
        base::print('subject changed?')
        return(TRUE)
      }
      return(FALSE)
    }else{
      return(FALSE)
    }
  }
  w$get_execenv_local_reactive = function(){
    self$local_reactives
  }
  w$eval_when_ready = function(FUN){
    if(is.function(FUN)){
      self$ready_functions[[length(self$ready_functions) + 1]] = FUN
    }
  }
  
  w$get_client_size = function(){
    if(is.reactivevalues(self$global_reactives)){
      return(shiny::isolate(self$global_reactives$client_size))
    }
    return(NULL)
  }

  w$switch_to = function(module_id, varriable_name = NULL, value = NULL, quiet = F, ...){
    if(is.reactivevalues(self$global_reactives)){
      # if missing module_id, jump to last activated module
      # This is a hidden feature if not specifying module_id
      # 1. in the dev mode, I'll raise error if module_id is not string
      # 2. Be careful when using this hidden feature since it might cause infinite loop
      if(missing(module_id)){
        module_id = NULL
        hist = isolate(self$global_reactives$view_history)
        if(length(hist) > 1){
          ind = which(vapply(hist, '[[', logical(1L), 'activated'))
          if(length(ind)){
            ind = ind[length(ind)]
            module_id = hist[[ind]]$module_id
          }
        }
      }
      if(length(module_id)){
        self$global_reactives$switch_module = c(
          list(
            module_id = module_id,
            varriable_name = varriable_name,
            value = value,
            timestamp = Sys.time()
          ),
          list(...)
        )
      }else{
        # showNotification(p('Cannot switch back. You have not opened any modules yet.'), type = 'warning')
        w$launch_selector()
      }
    }
  }

  w$reload_module = function(){
    self$clear_cache()
    self$input_update(list(), init = TRUE)
  }

  w$current_module = function(){
    if(is.reactivevalues(self$global_reactives)){
      return(isolate(get_val(self$global_reactives, 'execute_module', default = '')))
    }
    return('')
  }

  w$get_brain = function(surfaces = 'pial', multiple_subject = FALSE){
    subject = get0('subject', envir = rave::getDefaultDataRepository(), ifnotfound = NULL)
    brain = NULL
    if( !is.null(subject) ){
      brain = rave_brain2(subject = subject, surfaces = surfaces, compute_template = FALSE)
      if( multiple_subject ){
        brain = threeBrain::merge_brain(brain)
      }
    }
    brain
  }

  w$require = function(package, ..., character.only = TRUE){
    p = as.character(substitute(package))
    if(!package_installed(p)){
      try({
        logger("Installing Package ", p, level = 'WARNING')
        install.packages(p, type = 'binary')
      })
    }
    do.call('require', args = c(list(
      package = p,
      character.only = TRUE
    ),
    list(...)))
  }
  w$library = w$require

  w$observe = function(x, env = NULL, quoted = FALSE, priority = 0, domain = NULL, ...){
    if(!quoted){
      x = substitute(x)
    }

    # Make sure shiny doesn't crash
    x = rlang::quo_squash(rlang::quo(
      tryCatch({
        shiny::withLogErrors({!!x})
      }, error = function(e){
        showNotification(htmltools::p(htmltools::strong('An error occurred'), htmltools::br(), 'Details: ',
                                      htmltools::span(as.character(e), style = 'font-style:italic;')), type = 'error')
      })
    ))


    if(!is.environment(env)){
      env = self$runtime_env
    }
    if(is.null(domain)){
      domain = self$wrapper_env$getDefaultReactiveDomain()
    }
    shiny::observe(
      x = x,
      env = env,
      quoted = T,
      priority = priority - 1L,
      domain = domain,
      ...
    )
  }


  w$observeEvent = function(
    eventExpr, handlerExpr, event.env = NULL,
    event.quoted = FALSE, handler.env = NULL, handler.quoted = FALSE,
    priority = 0, domain = NULL, ...
  ){
    if(!event.quoted){
      eventExpr = substitute(eventExpr)
    }
    if(!is.environment(event.env)){
      event.env = self$runtime_env
    }

    if(!handler.quoted){
      handlerExpr = substitute(handlerExpr)
    }
    if(!is.environment(handler.env)){
      handler.env = self$runtime_env
    }
    if(is.null(domain)){
      domain = self$wrapper_env$getDefaultReactiveDomain()
    }

    # Make sure shiny doesn't crash
    handlerExpr = rlang::quo_squash(rlang::quo(
      tryCatch({
        shiny::withLogErrors({!!handlerExpr})
      }, error = function(e){
        showNotification(htmltools::p(htmltools::strong('An error occurred'), htmltools::br(), 'Details: ',
                                      htmltools::span(as.character(e), style = 'font-style:italic;')), type = 'error')
      })
    ))

    shiny::observeEvent(
      eventExpr = eventExpr, handlerExpr = handlerExpr, event.env = event.env,
      event.quoted = T, handler.env = handler.env, handler.quoted = T,
      priority = priority - 1L, domain = domain, ...
    )
  }

  if(shiny_mode){
    w$rave_inputs = self$rave_inputs
    w$rave_outputs = self$rave_outputs
    w$rave_updates = self$rave_updates
    w$rave_execute = self$rave_execute
    w$rave_checks = function(...){ f = self$static_env[['rave_checks']]; if(is.function(f)) f(...) }
    w$cache = self$cache
    w$cache_input = self$cache_input
    w$rave_ignore = do_nothing
  }else{
    w$rave_inputs = rave_inputs
    w$rave_outputs = rave_outputs
    w$rave_updates = rave_updates
    w$rave_execute = rave_execute
    w$rave_checks = rave_checks
    w$cache = cache
    w$cache_input = cache_input
    w$rave_ignore = rave_ignore
  }

  w$export_report = self$export_report
  w$rave_prepare = do_nothing
  w$ns = function(id){
    # ns will be changed during shinirize process
    self$ns(id)
  }

}
