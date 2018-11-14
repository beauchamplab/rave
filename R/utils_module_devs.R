
#' Function to create rave package development environment
#' @export
init_dev_environment <- function(package, reload_subject = FALSE, reactive = FALSE){

  lapply(c('rutabaga', 'shiny', 'threejsr', 'rave'), function(p){
    do.call('library', list(
      package = p,
      warn.conflicts = FALSE,
      quietly = TRUE,
      character.only = TRUE
    ))
  })


  if('rave_toolbox' %in% search()){
    detach('rave_toolbox', character.only = T)
  }




  # First, source util functions
  rave_env = loadNamespace('rave')
  utils = new.env(parent = rave_env)
  utils$.package_name = package
  utils$utils_path = system.file('rave_utils.R', package = package)
  cat2('loading package utils: ', utils$utils_path)
  with(utils, {
    source(utils_path, local = TRUE)
  })


  # Next, load config file
  config = utils$load_rave_yaml()

  # Let's import demo subject
  rave_data = rave::getDefaultDataRepository()
  if(!reload_subject){
    if(!all(c("data_check", "module_tools", "preload_info", "subject") %in% ls(rave_data))){
      reload_subject = TRUE
    }
  }

  if(reload_subject){
    do.call(utils$load_demo_subject, config$dev_subject)
  }

  if(!'rave_data' %in% search()){
    attach(rave_data)
  }





  # Next create some wrapper functions to use
  toolbox = new.env(parent = emptyenv())
  localenv = new.env(parent = emptyenv())
  localenv$reactive_inputs = list()
  localenv$reactive_outputs = list()
  tmpenv = new.env(parent = environment())


  toolbox$reload_init = function(){
    quos = utils$modularize_inputs(config = config, is_reactive = F)
    rave::eval_dirty(quos$rave_input_quo, env = globalenv())
    rave::eval_dirty(quos$rave_updates_quo, env = globalenv())
  }

  toolbox$init_inputs = toolbox$reload_init

  toolbox$reload_package = function(path = '.', command = sprintf("rave::init_dev_environment('%s')", package)){
    devtools::install('.')
    .rs.api.restartSession(command = command)
  }

  toolbox$preview = function(){
    utils$modularize(config)
  }

  attach_toolbox = function(){
    sel = search() == 'rave_data'
    if(any(sel)){
      pos = which(sel) + 1
    }else{
      pos = 2L
    }
    attach(toolbox, name = 'rave_toolbox', warn.conflicts = FALSE, pos = pos)
  }
  if(!reactive){
    attach_toolbox()
    return(invisible())
  }

  # Help function
  showdoc = function(topic, ...){
    if(is.function(toolbox[[topic]]) && is(toolbox[[topic]], 'helper')){
      e = body(toolbox[[topic]])
      cat(e[[2]])
    }

    utils::help(topic = topic, ...)
  }
  toolbox$showdoc = showdoc

  print.helper = function(x, ...){
    if(is.function(x)) cat(body(x)[[2]])
    return(x)
  }
  toolbox$print.helper = print.helper


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
  toolbox$print.msg = print.msg

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
      warning('You have some data to be loaded. Use function module_tools$get_XXX to load data')
    })
  }
  class(rave_checks) = c('helper', 'function')
  toolbox$rave_checks = rave_checks


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
  class(cache_input) = c('helper', 'function')
  toolbox$cache_input = cache_input


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
  class(getDefaultReactiveDomain) = c('helper', 'function')
  toolbox$getDefaultReactiveDomain = getDefaultReactiveDomain

  session = getDefaultReactiveDomain()
  toolbox$session = session


  # Create input bindings
  print.reactive_dev <- function(x, ...){
    print(x())
  }
  toolbox$print.reactive_dev = print.reactive_dev

  `[[.reactive_dev` = `$.reactive_dev` = function(x, k){
    y = x()
    y[[k]]
  }
  toolbox$`[[.reactive_dev` = `[[.reactive_dev`
  toolbox$`$.reactive_dev` = `$.reactive_dev`

  `[.reactive_dev` = `[.reactive_dev<-` = function(...){
    stop('No `[`, `[<-` methods for reactive values')
  }
  toolbox$`[.reactive_dev` = `[.reactive_dev`
  toolbox$`[.reactive_dev<-` = `[.reactive_dev<-`


  get_inputs = function(){
    inputId = lapply(config$inputs, function(comp){
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

  # `[[<-.reactive_dev` = `$<-.reactive_dev` = function(x, k){
  #   y = x()
  #   y[[k]]
  # }
  class(get_inputs) = c('reactive_output_dev', 'reactive_dev', 'function')
  makeActiveBinding('input', get_inputs, toolbox)

  get_outputs = function(){
    localenv$reactive_outputs
  }
  class(get_outputs) = c('reactive_dev', 'function')
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
  class(getDefaultReactiveInput) = c('helper', 'function')
  toolbox$getDefaultReactiveInput = getDefaultReactiveInput




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
  class(getDefaultReactiveOutput) = c('helper', 'function')
  toolbox$getDefaultReactiveOutput = getDefaultReactiveOutput


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
  toolbox$ns = ns



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
  toolbox$rave_updates = rave_updates

  progress = function(
    title, max = 1,
    session = getDefaultReactiveDomain(),
    quiet = FALSE
  ){
    rave::progress(title, max, session = NULL, quiet)
  }
  toolbox$progress = progress


  ##### Inputs ####
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
  toolbox$selectInput = selectInput

  checkboxGroupInput = function(
    inputId, label, choices = NULL, selected = NULL, inline = FALSE, width = NULL
  ){
    ip = substitute(inputId)
    proty('checkboxGroupInput', TRUE, ip, label, value = selected, type = character)
    return(shiny::checkboxGroupInput(inputId, label, choices, selected , inline, width ))
  }
  toolbox$checkboxGroupInput = checkboxGroupInput

  radioButtons = function (inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                           width = NULL){
    ip = substitute(inputId)
    proty('radioButtons', TRUE, ip, label, value = selected, type = character)
    return(shiny::radioButtons(inputId, label, choices, selected , inline, width ))
  }
  toolbox$radioButtons = radioButtons


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
  toolbox$sliderInput = sliderInput

  textInput = function(
    inputId, label, value = "", width = NULL, placeholder = NULL
  ){
    ip = substitute(inputId)
    proty('textInput', TRUE, ip, label, value = value, type = character, size = 1)
    return(shiny::textInput(inputId, label, value, width , placeholder ))
  }
  toolbox$textInput = textInput

  numericInput = function(
    inputId, label, value, min = NA, max = NA, step = NA, width = NULL
  ){
    ip = substitute(inputId)
    proty('numericInput', TRUE, ip, label, value = value, type = numeric, size = 1)
    return(shiny::numericInput(inputId, label, value, min , max , step , width ))
  }
  toolbox$numericInput = numericInput

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
  toolbox$compoundInput = compoundInput


  uiOutput = function(outputId, inline = FALSE, container = if (inline) span else div,
                      ...){
    ip = substitute(outputId)
    proty('uiOutput', FALSE, ip, label = '', value = 'renderUI')
    return(shiny::uiOutput(outputId, container = container, inline = inline))
  }
  toolbox$uiOutput = uiOutput

  textOutput = function(outputId, container = if (inline) span else div, inline = FALSE){
    ip = substitute(outputId)
    proty('textOutput', FALSE, ip, label = '', value = 'renderText')
    return(shiny::textOutput(outputId, container, inline))
  }
  toolbox$textOutput = textOutput

  verbatimTextOutput = function(outputId, placeholder = FALSE){
    ip = substitute(outputId)
    proty('verbatimTextOutput', FALSE, ip, label = '', value = 'renderPrint')
    return(shiny::verbatimTextOutput(outputId, placeholder))
  }
  toolbox$verbatimTextOutput = verbatimTextOutput

  plotOutput = function(
    outputId, width = "100%", height = "400px", click = NULL,
    dblclick = NULL, hover = NULL, brush = NULL, clickId = NULL
  ){
    ip = substitute(outputId)
    proty('plotOutput', FALSE, ip, label = '', value = 'renderPlot')
    return(shiny::plotOutput(outputId, width, height, inline))
  }
  toolbox$plotOutput = plotOutput

  tableOutput = function(outputId){
    ip = substitute(outputId)
    proty('tableOutput', FALSE, ip, label = '', value = 'renderTable')
    return(shiny::tableOutput(outputId))
  }
  toolbox$tableOutput = tableOutput

  threejsOutput = function(outputId, width = "100%", height = "600px"){
    ip = substitute(outputId)
    proty('threejsOutput', FALSE, ip, label = '', value = 'renderThreejs')
    return(threejsr::threejsOutput(outputId, width, height ))
  }
  toolbox$threejsOutput = threejsOutput

  # Renderers
  renderPlot = function(expr, width = "auto", height = "auto", res = 72, ...,
                        env = parent.frame(), quoted = FALSE, execOnResize = FALSE,
                        outputArgs = list()){
    expr = substitute(expr)
    return(expr)
  }
  toolbox$renderPlot = renderPlot

  renderText = function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()){
    expr = substitute(expr)
    return(expr)
  }
  toolbox$renderText = renderText

  ### Finally, attach ####

  attach_toolbox()

  return(invisible(toolbox))
}
