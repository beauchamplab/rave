# Function to initialize inputs in R session
init_inputs = function(){
  'Initialize inputs for this module. Not a RAVE function, use for debug only.'
  if(!'rave_data' %in% search()){
    rave::attachDefaultDataRepository()
  }
  quos = modularize_inputs(config = config, is_reactive = F)
  rave::eval_dirty(quos$rave_input_quo, env = globalenv())
  rave::eval_dirty(quos$rave_updates_quo, env = globalenv())
}

# Function to show help functions
showdoc = function(topic, ...){

  if(missing(topic) || is.null(topic) || topic == '' || topic == 'showdoc'){
    cat('--------------------------------------------------------\n')
    cat('Use showdoc("FUN") to see internal dev helps and additional information.',
        'Possible funcitons:\n', sep = '\n')
    cat('\t')
    nms = names(toolbox)
    has_doc = sapply(nms, function(nm){
      f = toolbox[[nm]]
      if(!is.function(f)){ return(FALSE) }
      e = body(f)[[2]]
      if(is.character(e)){
        return(TRUE)
      }else{
        FALSE
      }
    })
    nms = nms[has_doc]
    cat(nms, sep = ', ')
    cat('\n--------------------------------------------------------\n')
    return(invisible())
  }

  if(is.function(toolbox[[topic]])){
    cat('--------------------------------------------------------\n')
    rutabaga::cat2('Dev help:\n', level = 'INFO')
    e = body(toolbox[[topic]])
    s = e[[2]]
    if(is.character(s)){
      s = stringr::str_trim(s)
      cat(s)
    }
    cat('\n--------------------------------------------------------\n')
    rutabaga::cat2('Additional docs:\n', level = 'INFO')
  }

  utils::help(topic = topic, ...)
}


rave_checks = function(..., data = NULL){
  'Definition: rave_checks(..., data = NULL) \n\
    Example: \
    \t# The following example means the module requires power (referenced) and\
    \t# phase (raw) to be loaded. RAVE will pop up a dialogue asking users \
    \t# to load them
    \trave_checks("power referenced", "phase raw")\n\
    rave_checks checks whether ECoG data is loaded. The format is: \
    \tDATA+(blankspace)+TYPE. \n\
    * DATA can be \
    \t"power" (Wavelet transform amplitude), \
    \t"phase" (Complex angle), or \
    \t"volt"/"voltage" (Before wavelet). \
    * TYPE can be \
    \t"raw" (no reference), \
    \t"referenced" (referenced by common average reference, white matter \
    \t  reference, or bipolar reference). \
    For voltage data, there is one more special type "full" which loads voltage\
    data for all electrodes.\
    '

  args = unlist(c(list(...), data))
  tryCatch({
    do.call(rave::rave_checks, args)
  }, error = function(e){
    rutabaga::cat2('[ DEV ] You have some data to be loaded. Use function module_tools$get_XXX to load data.', level = 'WARNING')
  })
}


cache_input = function(inputId, val){
  'Definition: cache_input(inputId, val)\
    cache_input is usually used inside of input initialization functions to \
    recover last users\' inputs. \
    \
    Parameters:\
    \tinputId \t- character, defined in confid.yaml
    \tval\t\t- default value to use if user input is missing
    '
  rave::cache_input(inputId, val)
}





print.helper = function(x, ...){
  if(is.function(x)) cat(body(x)[[2]])
  return(x)
}

print.msg = function(x, ...){
  y = x
  msg = attr(x, 'msg')
  attr(x, 'msg') = NULL
  cls = attr(x, 'class')
  cls = cls[cls != 'msg']
  class(x) = cls
  s = capture.output(base::print(x))
  rutabaga::cat2(s, sep = '\n', level = 'INFO', pal = list('INFO' = 'blue'))
  if( !is.null(msg) && msg != '' ){
    rutabaga::cat2(sprintf('\t(%s)\n', msg), level = 'INFO', pal = list('INFO' = 'grey'))
  }
  invisible(y)
}



preview = function(auto_reload = TRUE, test.mode = TRUE, launch.browser = TRUE){

  rave_env = new.env(parent = loadNamespace('rave'))

  rave_env$`...monitor_file_changed` = monitor_file_changed
  rave_env$`...reload` = dev_env_${{PACKAGE}}
  rave_env$`...module_id` = module_id

  rave_env$`...tmp_file` = tempfile(fileext = '.R')
  rave_env$`...test_module` = test_module
  rave_env$test.mode = test.mode
  rave_env$launch.browser = launch.browser
  rave_env$`...auto_reload` = auto_reload

  with(rave_env, {
    rave_app = init_app
    body(rave_app) = rlang::quo_squash(
      rlang::quo({
        {!!body(rave_app)} ->
          app
        return(environment())
      })
    )

    m = `...test_module`(launch = FALSE, temp_file = `...tmp_file`)

    `...env` = rave_app(modules = m, theme = 'green', test.mode = test.mode)

    if(!`...auto_reload`){
      return(shinyApp(`...env`$ui, server = `...env`$server, options = list(launch.browser = launch.browser, test.mode = test.mode)))
    }


    `...env`$`...monitor_file_changed` = `...monitor_file_changed`
    `...env`$`...reload` = `...reload`
    `...env`$`...module_id` = `...module_id`
    body(...env$server) = rlang::quo_squash(
      rlang::quo({
        !!body(`...env`$server)
        # monitor file change
        debug_timer = shiny::reactiveTimer()
        observeEvent(debug_timer(), {

          if(`...monitor_file_changed`()){
            rutabaga::cat2('Detected file change in folder ./R or ./inst, reload package. Wait for a second.')
            `...reload`(`...module_id`)
            shinyjs::runjs('setTimeout(function(){ location.reload(true); }, 1500);')
            stopApp()
            rstudioapi::sendToConsole('preview(launch.browser = FALSE)', execute = TRUE)
          }
        })
      })
    )

    shinyApp(`...env`$ui, server = `...env`$server, options = list(launch.browser = launch.browser, test.mode = test.mode))

  })
}
