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
  'shiny::selectInput'
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
