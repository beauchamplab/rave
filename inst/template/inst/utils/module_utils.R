# package = 'XXX'
# root_path = '.'
# restart_command
# config

module_utils <- function(package, root_path, restart_command, config){

#' Converts configurations into RAVE input components
modularize_inputs <- function(config, is_reactive = FALSE){
  # get namespace
  penv = loadNamespace(package, partial = T)

  input_ids = names(config$inputs)

  lapply(input_ids, function(inputId){
    comp = config$inputs[[inputId]]
    comp$inputId = inputId
    comp$label %?<-% '(Missing label)'
    init_quo = do.call(penv$to_input_component, comp)

    # get update quos
    if(
      !is.null(comp$init) &&
      length(comp$init) == 1 &&
      is.character(comp$init) &&
      is.function(penv[[comp$init]])
    ){
      update_quo = rlang::quo(local(!!body(penv[[comp$init]])))
    }else{
      update_quo = NULL
    }
    list(
      inputId = comp$inputId,
      init_quo = init_quo,
      update_quo = update_quo
    )
  }) ->
    inputs

  input_quos = lapply(inputs, function(comp){
    # extract expressions
    rlang::quo_squash(comp$init_quo)
  })

  # Generate rave_inputs
  rave_input_quo = rlang::quo({
    rave_inputs(
      !!!input_quos,
      .input_panels = !!config$input_layouts
    )
  })

  # Generate rave_updates
  # names
  var_names = c('', input_ids)


  checks = unlist(config$data_checks)
  if(length(checks) && is_reactive){
    check_expr = rlang::quo({
      rave_checks(data = !!checks)
    })
  }else{
    check_expr = quote({})
  }

  if(is.function(penv[[config$init_function]])){
    init_expr = rlang::quo(!!body(penv[[config$init_function]]))
  }else{
    init_expr = quote({})
  }

  update_quos = c(
    rlang::quo({
      !!check_expr
      !!init_expr
    }),
    lapply(inputs, function(comp){ comp$update_quo })
  )

  names(update_quos) = var_names
  update_quos = dropNulls(update_quos)
  update_quos = sapply(update_quos, rlang::quo_squash, USE.NAMES = T, simplify = F)


  rave_updates_quo = rlang::quo({
    rave_updates(!!!update_quos)
  })

  return(list(
    rave_input_quo = rave_input_quo,
    rave_updates_quo = rave_updates_quo
  ))
}

#' Converts configurations into RAVE output components
modularize_outputs <- function(config, is_reactive = FALSE){
  # get namespace
  penv = loadNamespace(package, partial = T)

  # Get input elements
  varnames = names(config$inputs)

  body = body(penv[[config$main_function]])
  args = formals(penv[[config$main_function]])

  # generate rave_execute
  rave_execute_quo = rlang::quo({
    rave_execute({
      # get params
      ._env = ..runtime_env
      ._args = sapply(!!varnames, get, envir = ._env, inherits = TRUE, simplify = F, USE.NAMES = T)

      # make function
      ._f = function(){!!!body}
      formals(._f) = !!args

      result = do.call(._f, ._args)

      result
    })
  })


  # generate output quos
  output_titles = sapply(config$outputs, '[[', 'title')
  output_ids = names(config$outputs)

  output_quos = lapply(output_ids, function(outputId){
    comp = config$outputs[[outputId]]

    # create a fake function takes no arg
    fake_fname = paste0('._tmp_output_', outputId)

    fake_quo = rlang::quo(
      assign(!!fake_fname, function(){

        # Check if function exists
        fname = !!outputId
        f = NULL
        if(exists(fname, inherits = T)){
          f = get0(fname)
        }

        if(!is.function(f)){
          message = paste0('Cannot find function ', fname)
          cond = structure(list(message = message),
                           class = c("shiny.silent.error", "validation", "error", "condition"))
          stop(cond)
        }

        if(!exists('result') || !is.list(result)){
          result = list()
        }

        f(result)
      })
    )

    comp$outputId = fake_fname

    out_quo = do.call(penv$to_output_component, comp)

    list(
      fake_quo = fake_quo,
      out_quo = out_quo
    )
  })

  # extract fake calls
  fake_quos = lapply(output_quos, '[[', 'fake_quo')
  output_quos = lapply(output_quos, '[[', 'out_quo')

  names(output_quos) = output_titles

  # convert layouts for outputs
  width = config$output_layouts[['width']]
  width %?<-% 12L
  config$output_layouts[['width']] = NULL
  output_layouts = sapply(config$output_layouts, function(comp){
    if(is.list(comp)){
      sapply(comp, function(outputId){
        paste0('._tmp_output_', outputId)
      }, simplify = F, USE.NAMES = T)
    }
  }, simplify = F, USE.NAMES = T)
  output_layouts[['width']] = width

  rave_output_quo = rlang::quo({
    !!!fake_quos

    rave_outputs(!!!output_quos, .output_tabsets = !!output_layouts)
  })

  # Obtain reactive expressions
  if(is.function(penv[['__reactive__']])){
    rave_reactive_quo = rlang::quo(!!body(penv[['__reactive__']]))
  }else{
    rave_reactive_quo = quote({})
  }

  return(list(
    rave_reactive_quo = rave_reactive_quo,
    rave_execute_quo = rave_execute_quo,
    rave_output_quo = rave_output_quo
  ))
}

#' Converts configurations into RAVE components and launch GUI
modularize = function(config, launch_gui = T){
  launch_gui %?<-% T
  quos = c(
    modularize_inputs(config, is_reactive = launch_gui),
    modularize_outputs(config, is_reactive = launch_gui)
  )
  content = paste(unlist(lapply(quos, rlang::quo_text)), collapse = '\n')

  # write to a temp file
  tf = tempfile(fileext = '.R')

  writeLines(text = content, con = tf, sep = '\n')

  parent_env = loadNamespace(package, partial = T)

  m = ModuleEnvir$new(
    module_id = config$module_id, label_name = config$module_label, parent_env = parent_env,
    packages = package, script_path = tf)

  init_app(m, test.mode = T)
}

get_toolbox = function(reactive = FALSE){
  # Create toolbox environment, which will be attached to search path
  toolbox = new.env(parent = emptyenv())

  # A local environment storing inputs and outputs
  localenv = new.env(parent = emptyenv())
  localenv$reactive_inputs = list()
  localenv$reactive_outputs = list()

  if('rave_toolbox' %in% search()){
    detach('rave_toolbox', character.only = T)
  }

  # Function to attach toolbox
  attach_toolbox = function(){
    sel = search() == 'rave_data'
    if(any(sel)){
      pos = which(sel) + 1
    }else{
      pos = 2L
    }
    attach(toolbox, name = 'rave_toolbox', warn.conflicts = FALSE, pos = pos)
  }

  # ------------------------ Toolbox ------------------------
  reload_package = function(path = root_path, command = restart_command){
    'Re-compile the package and install the package.
  Parameters:
      path: path to package dir
      command: R command after restarting session
  '
    devtools::install(path)
    if(is.character(command)){
      rstudioapi::restartSession(command = command)
    }
  }


  reload_init = function(){
    'Initialize inputs for this module
  '
    if(!'rave_data' %in% search()){
      rave::attachDefaultDataRepository()
    }
    quos = modularize_inputs(config = config, is_reactive = F)
    rave::eval_dirty(quos$rave_input_quo, env = globalenv())
    rave::eval_dirty(quos$rave_updates_quo, env = globalenv())
  }


  init_inputs = reload_init

  preview = function(){
    'Launch GUI from RAVE
  '
    modularize(config)
  }

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
    if(!is.null(msg) && msg != ''){
      rutabaga::cat2(sprintf('\t(%s)\n', msg), level = 'INFO', pal = list('INFO' = 'grey'))
    }
    invisible(y)
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


  sapply(c(
    'reload_package', 'preview', 'init_inputs', 'reload_init', 'showdoc',
    'print.helper', 'print.msg', 'rave_checks', 'cache_input'
  ), function(f){
    toolbox[[f]] = get(f, envir = environment())
  })

  if(!reactive){
    attach_toolbox()
    return(invisible())
  }


  # ------------------------ Toolbox: reactive ------------------------


  getDefaultReactiveDomain = function(){
    'Usage: session <- getDefaultReactiveDomain()
    This is not often used, unless you want to update some input elements in
    customized inputs/outputs. For example in a customized UI, we define a
    textInput:
        ...
        # customized input
        textInput(inputId = ns("text_id"), label = "Input Text: ")
        ...
    Later in other places, you want to update the input label and change
    "Input Text: " to "Updated Label: ", you need to use shiny input update
    function `updateTextInput` (use help function to see the details):
        updateTextInput(session, inputId = "text_id", label = "Updated Label: ")

    Usually this is the only place to use session object. For more details,
    please refer to
        1. shiny input widgets:
              https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
        2. A shint input demo:
              https://shiny.rstudio.com/gallery/update-input-demo.html
    '

    rave::logger('A fake session is created. Do NOT read, assign any value to this object')
    fakesession = new.env()
    fakesession$sendInputMessage = function(inputId, message){
      key = NULL
      if('value' %in% names(message)){ key = 'value' }
      if('selected' %in% names(message)){ key = 'selected' }

      if(!is.null(key)){
        old_val = localenv$reactive_outputs[[inputId]]
        if(!is.null(old_val)){
          msg = attr(old_val, 'msg')
        }else{
          msg = NULL
        }

        localenv$reactive_outputs[[inputId]] = message[[key]]
        attr(localenv$reactive_outputs[[inputId]], 'msg') = msg
        class(localenv$reactive_outputs[[inputId]]) = 'msg'
      }

      localenv$reactive_outputs[inputId] = message
      return(message)
    }
    fakesession$userData = new.env(parent = emptyenv())
    fakesession$userData$rave_id = '__fake_session__'

    class(fakesession) = c('msg', 'environment')
    attr(fakesession, 'msg') = 'This is a fake session for dev use'
    lockEnvironment(fakesession, bindings = T)
    fakesession
  }

  session = getDefaultReactiveDomain()

  print.reactive_dev <- function(x, ...){
    print(x())
  }

  `[[.reactive_dev` = `$.reactive_dev` = function(x, k){
    y = x()
    y[[k]]
  }
  `[.reactive_dev` = `[.reactive_dev<-` = function(...){
    stop('No `[`, `[<-` methods for reactive values')
  }



  # Reactives
  get_inputs = function(){
    ids = names(config$inputs)
    inputId = lapply(ids, function(inputId){
      comp = config$inputs[[inputId]]
      comp$inputId = inputId
      if(comp$type != 'customizedInput'){
        comp$inputId
      }
    })

    ids = unlist(inputId)
    g = globalenv()
    ip = sapply(ids, function(id){
      get0(id, envir = g, inherits = F)
    })

    c(ip, localenv$reactive_inputs)
  }
  class(get_inputs) = c('reactive_dev', 'function')
  makeActiveBinding('input', get_inputs, toolbox)


  `[[<-.reactive_output_dev` = `$<-.reactive_output_dev` = function(x, k, value){
    tmp = localenv$reactive_outputs[[k]]
    if(is.null(tmp)){
      tmp = ''
      class(tmp) = 'msg'
      msg = ''
    }else{
      msg = attr(tmp, 'msg')
    }
    if(length(msg) < 2){
      msg = c(msg, '')
    }
    msg[2] = paste('Render: ', paste(capture.output(print(value)), collapse = '\n\t'))

    attr(tmp, 'msg') = msg
    localenv$reactive_outputs[[k]] = tmp
    return(invisible(get_outputs))
  }


  get_outputs = function(){
    localenv$reactive_outputs
  }
  class(get_outputs) = c('reactive_output_dev', 'reactive_dev', 'function')
  makeActiveBinding('output', get_outputs, toolbox)

  getDefaultReactiveInput = function(){
    'Usage: input <- getDefaultReactiveInput()
    This is not often used. Actually when launching the package, an reactive
    input object has been created and ready to use. So, use "input" directly!

    Example 1:
          observeEvent(input$ELECTRODE, {
            print("Electrode is changed!")
          })

    Example 2:
          # A customized UI, register outputId "customized_output" in
          # config.yaml - outputs and specify type "customizedOutput"
          customized_output <- function(){
            plotOutput(ns("plot_id"))
          }

          # Render it in `__reactives__`
          output$plot_id <- renderPlot({
            # Plot something here
            plot(1:10)
          })
    '

    toolbox$input
  }


  getDefaultReactiveOutput = function(){
    'Usage: output <- getDefaultReactiveOutput()
    This is not often used. Actually when launching the package, an reactive
    output object has been created and ready to use. So, use "output" directly!

    Example:
          # A customized UI, register outputId "customized_output" in
          # config.yaml - outputs and specify type "customizedOutput"
          customized_output <- function(){
            plotOutput(ns("plot_id"))
          }

          # Render it in `__reactives__`
          output$plot_id <- renderPlot({
            # Plot something here
            plot(1:10)
          })
    '

    toolbox$output
  }


  ns = function(id){
    'Definition: ns(id)
    Used in customized UIs and wrap up input/output ids so that different
    modules won\'t interrupt each others.

    Usage:
          # Example as inputs. Here we use numeric input as an example
          # See ?numericInput for more example
          numericInput(ns("my_age"), "Enter a Number:", value = 20)

    WARNING:
          1. Any input components defined in comfig.yaml is automatically
          wrapped up by ns(.)
          2. When updating inputs, just use its raw IDs. For this example,
          if you want to update input "my_age" defined in the `usage` above
          to 30, just use:
              updateNumericInput(session, "my_age", value = 30)



    See alse < http://shiny.rstudio.com/articles/modules.html >
    '

    shiny::NS(package)(id)
  }


  rave_updates = function(..., .env = globalenv()){
    rave::logger('Updating inputs')
    res = rlang::quos(...)
    nms = names(res)
    if(length(nms) == 0){
      return()
    }
    lapply(res[nms == ''], function(quo){
      rave::eval_dirty(quo, env = .env)
    })

    nms = nms[nms != '']

    for(nm in nms){
      val = rave::eval_dirty(res[[nm]], env = .env)
      try({
        re = val$value
        re %?<-% val$selected
        .env[[nm]] = re
      })
    }

    invisible(res)
  }

  sapply(c(
    'getDefaultReactiveDomain', 'session', 'print.reactive_dev',
    '[[.reactive_dev', '$.reactive_dev', '[.reactive_dev', '[.reactive_dev<-',
    '[[<-.reactive_output_dev', '$<-.reactive_output_dev',
    'getDefaultReactiveInput', 'getDefaultReactiveOutput', 'ns',
    'rave_updates'
  ), function(f){
    toolbox[[f]] = get(f, envir = environment())
  })


  # ------------------------ Toolbox: Components ------------------------
  proty = function(fname, isInput, ip, label, value = NULL, type = character, size = 0){
    parent_env = parent.frame(n = 2)

    type_str = as.character(substitute(type))

    # check
    io_type = ifelse(isInput, 'inputId', 'outputId')

    if(!is.call(ip) || !ip[[1]] == 'ns'){
      stop(sprintf('\n%s Must be wrapped up by ns(...). Please use
      \t%s(%s = ns("%s"), ...', io_type, fname, io_type, ip))
    }

    input_id = eval(ip[[2]], envir = list(), enclos = parent_env)

    if(!stringr::str_detect(input_id, '^[a-zA-Z]')){
      stop(sprintf('inputId is suggested to starting with a to z or A to Z, recommended: (%s --> %s)',
                   input_id, stringr::str_remove(input_id, '^[^a-zA-Z]*')))
    }


    if(isInput){
      if(is.null(value)){
        value = type(size)
      }
      class(value) = 'msg'
      attr(value, 'msg') = sprintf('%s [%s, ID:%s, Label:%s] input', fname, type_str, input_id, label)
      localenv$reactive_inputs[[input_id]] = value
    }else{
      value = sprintf("Usage: output$%s <- %s({...})", input_id, value)
      attr(value, 'msg') = sprintf('%s [%s, ID:%s] output', fname, type_str, input_id)
      class(value) = 'msg'
      localenv$reactive_outputs[[input_id]] = value
    }


    value
  }

  selectInput = function(
    inputId, label, choices, selected = NULL, multiple = FALSE,
    selectize = TRUE, width = NULL, size = NULL
  ){
    ip = substitute(inputId)
    force(choices)
    proty('selectInput', TRUE, ip, label, value = selected, type = character)
    return(shiny::checkboxGroupInput(inputId, label, choices, selected , multiple,
                                     selectize, width, size ))
  }

  checkboxGroupInput = function(
    inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL
  ){
    ip = substitute(inputId)
    proty('checkboxGroupInput', TRUE, ip, label, value = selected, type = character)
    return(shiny::checkboxGroupInput(inputId, label, choices, selected , inline, width ))
  }

  radioButtons = function (inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                           width = NULL){
    ip = substitute(inputId)
    proty('radioButtons', TRUE, ip, label, value = selected, type = character)
    return(shiny::radioButtons(inputId, label, choices, selected , inline, width ))
  }


  sliderInput = function(
    inputId, label, min, max, value, step = NULL, round = FALSE,
    format = NULL, locale = NULL, ticks = TRUE, animate = FALSE,
    width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
    timezone = NULL, dragRange = TRUE
  ){
    force(min); force(max); force(value)
    ip = substitute(inputId)
    proty('sliderInput', TRUE, ip, label, value = value, type = numeric, size = length(value))
    return(shiny::sliderInput(inputId, label, min, max, value, step, round, format, locale, ticks, animate, width, sep, pre, post, timeFormat, timezone, dragRange))
  }


  textInput = function(
    inputId, label, value = "", width = NULL, placeholder = NULL
  ){
    ip = substitute(inputId)
    proty('textInput', TRUE, ip, label, value = value, type = character, size = 1)
    return(shiny::textInput(inputId, label, value, width , placeholder ))
  }

  numericInput = function(
    inputId, label, value, min = NA, max = NA, step = NA, width = NULL
  ){
    ip = substitute(inputId)
    proty('numericInput', TRUE, ip, label, value = value, type = numeric, size = 1)
    return(shiny::numericInput(inputId, label, value, min , max , step , width ))
  }



  checkboxInput = function(inputId, label, value = FALSE, width = NULL){
    ip = substitute(inputId)
    proty('checkboxInput', TRUE, ip, label, value = value, type = logical, size = 1)
    return(shiny::checkboxInput(inputId, label, value, width))
  }
  toolbox$checkboxInput = checkboxInput

  compoundInput = function(
    inputId, label = '', components = NULL, max_ncomp = 10, inital_ncomp = 1,
    prefix = 'Group', style = ''
  ){
    ip = substitute(inputId)
    components = substitute(components)

    quos = rlang::eval_tidy(rlang::quo(rlang::quos(!!!components)))

    lapply(quos, function(quo){
      expr = rlang::quo_squash(quo)
      fname = capture.output({print(expr[[1]])})

      expr = match.call(get(fname), call = expr, expand.dots = T)
      value = eval(expr[['value']])
      value %?<-% eval(expr[['selected']])

      inputId = eval(expr[['inputId']])
      if(!is.character(inputId)){
        stop('In compoundInput, ', capture.output(expr), ' inputId is wrong. Make sure it is a character')
      }
      if(inputId == ""){
        stop(capture.output(expr), ' inputId cannot be blank.')
      }
      re = list(
        k = inputId,
        v = value)

    }) ->
      components

    v = lapply(components, '[[', 'v')
    k = sapply(components, '[[', 'k')
    names(v) = k

    proty('compoundInput', TRUE, ip, prefix, value = list(v), type = list, size = NULL)
    return(rave::compoundInput(inputId, label, components = {}, max_ncomp, inital_ncomp,
                               prefix, style))
  }


  uiOutput = function(outputId, inline = FALSE, container = if (inline) span else div,
                      ...){
    ip = substitute(outputId)
    proty('uiOutput', FALSE, ip, label = '', value = 'renderUI')
    return(shiny::uiOutput(outputId, container = container, inline = inline))
  }

  textOutput = function(outputId, container = if (inline) span else div, inline = FALSE){
    ip = substitute(outputId)
    proty('textOutput', FALSE, ip, label = '', value = 'renderText')
    return(shiny::textOutput(outputId, container, inline))
  }

  verbatimTextOutput = function(outputId, placeholder = FALSE){
    ip = substitute(outputId)
    proty('verbatimTextOutput', FALSE, ip, label = '', value = 'renderPrint')
    return(shiny::verbatimTextOutput(outputId, placeholder))
  }

  plotOutput = function(
    outputId, width = "100%", height = "400px", click = NULL,
    dblclick = NULL, hover = NULL, brush = NULL, clickId = NULL
  ){
    ip = substitute(outputId)
    proty('plotOutput', FALSE, ip, label = '', value = 'renderPlot')
    return(shiny::plotOutput(outputId, width, height, inline))
  }


  tableOutput = function(outputId){
    ip = substitute(outputId)
    proty('tableOutput', FALSE, ip, label = '', value = 'renderTable')
    return(shiny::tableOutput(outputId))
  }

  threejsOutput = function(outputId, width = "100%", height = "600px"){
    ip = substitute(outputId)
    proty('threejsOutput', FALSE, ip, label = '', value = 'renderThreejs')
    return(threejsr::threejsOutput(outputId, width, height ))
  }

  sapply(c(
    'selectInput', 'checkboxGroupInput', 'radioButtons', 'sliderInput', 'textInput',
    'numericInput', 'checkboxInput', 'compoundInput', 'uiOutput', 'textOutput',
    'verbatimTextOutput', 'plotOutput', 'tableOutput', 'threejsOutput'
  ), function(f){
    toolbox[[f]] = get(f, envir = environment())
  })

  return(environment())
}


return(environment())
}
