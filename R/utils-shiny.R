
safe_wrap_expr <- function(expr, onFailure = NULL, onError = NULL, finally = {}){
  ...internal_expr... = substitute(expr)
  withRestarts({
    
    tryCatch({
      force(expr)
    }, error = function(e){
      
      if(is.function(onFailure)){
        onFailure(e)
      }
      
      if(inherits(e, 'rave-info')){
        invokeRestart('rave-info', e)
      }else if(inherits(e, 'rave-warning')){
        invokeRestart('rave-warning', e)
      }else if(inherits(e, 'rave-warning')){
        invokeRestart('rave-error', e)
      }else{
        invokeRestart('rave-fatal', e)
      }
    }, finally = finally)
    
  }, `rave-info` = function(e){
    catgl(e$message, level = 'INFO')
    
  }, `rave-warning` = function(e){
    catgl(e$message, level = 'WARNING')
    invokeRestart('rave-notification', e)
    
  }, `rave-error` = function(e){
    catgl(e$message, level = 'ERROR')
    if(is.function(onError)){
      onError(e)
    }
    invokeRestart('rave-notification', e)
    
  }, `rave-fatal` = function(e){
    catgl(e$message, level = 'ERROR')
    if(is.function(onError)){
      onError(e)
    }
    print(...internal_expr...)
    print(traceback(e))
    invokeRestart('rave-notification', e)
    
  }, `rave-notification` = function(e, session = shiny::getDefaultReactiveDomain()){
    
    if(!is.null(session)){
      shiny::showNotification(
        shiny::p(shiny::span(e$message, style = 'font-style:italic;')), 
        type = 'error'
      )
    }
  })
}


observe <- function(x, env = NULL, quoted = FALSE, priority = 0, domain = NULL, ...){
  if(!quoted){
    x = substitute(x)
  }
  
  # Make sure shiny doesn't crash
  x = rlang::quo_squash(rlang::quo(
    safe_wrap_expr(!!x)
  ))
  
  if(!is.environment(env)){
    env = parent.frame()
  }
  if(is.null(domain)){
    domain = getDefaultReactiveDomain()
  }
  shiny::observe(
    x = x,
    env = env,
    quoted = TRUE,
    priority = priority - 1L,
    domain = domain,
    ...
  )
}


observeEvent = function(
  eventExpr, handlerExpr, event.env = NULL,
  event.quoted = FALSE, handler.env = NULL, handler.quoted = FALSE,
  priority = 0, domain = NULL, ...
){
  if(!event.quoted){
    eventExpr = substitute(eventExpr)
  }
  if(!is.environment(event.env)){
    event.env = parent.frame()
  }
  
  if(!handler.quoted){
    handlerExpr = substitute(handlerExpr)
  }
  if(!is.environment(handler.env)){
    handler.env = parent.frame()
  }
  if(is.null(domain)){
    domain = getDefaultReactiveDomain()
  }
  
  # Make sure shiny doesn't crash
  eventExpr = rlang::quo_squash(rlang::quo(
    safe_wrap_expr(!!eventExpr)
  ))
  
  
  handlerExpr = rlang::quo_squash(rlang::quo(
    safe_wrap_expr(!!handlerExpr)
  ))
  
  shiny::observeEvent(
    eventExpr = eventExpr, handlerExpr = handlerExpr, event.env = event.env,
    event.quoted = TRUE, handler.env = handler.env, handler.quoted = TRUE,
    priority = priority - 1L, domain = domain, ...
  )
}



add_to_session <- function(
  session,
  key = 'rave_id',
  val = paste(sample(c(letters, LETTERS, 0:9), 20), collapse = ''),
  override = FALSE
){
  if(!is.null(session)){
    if(override || !exists(key, envir = session$userData)){
      assign(key, val, envir = session$userData)
    }
    return(get(key, envir = session$userData))
  }
  return(NULL)
}

#' Fake 'shiny' Session for Debug Purpose
#' @param rave_id internally used
#' @param id module ID, used to create scope, will passed to \code{\link[shiny]{NS}}
#' @return Fake shiny session for debug purpose
#' @export
fake_session <- function(rave_id = '__fake_session__', id = NULL){
  self_id = id
  fakesession = new.env()
  
  shiny = asNamespace('shiny')
  list2env(as.list(shiny$createMockDomain()), fakesession)
  
  fakesession$sendInputMessage = function(inputId, message){
    return(message)
  }
  fakesession$userData = new.env(parent = emptyenv())
  fakesession$userData$rave_id = rave_id
  fakesession$ns = shiny::NS(id)
  
  fakesession$makeScope = function(id = NULL){
    if( identical(self_id, id) ){
      return(fakesession)
    }else{
      re = fake_session(rave_id = rave_id, id = id)
      re$userData = fakesession$userData
      return(re)
    }
  }
  
  fakesession$rootScope = function(){
    if(is.null(self_id)){
      return(fakesession)
    }else{
      re = fake_session(rave_id = rave_id, id = NULL)
      re$userData = fakesession$userData
      return(re)
    }
  }
  
  fakesession
}

with_fake_session <- function(func, ...){
  fakesession = new.env()
  fakesession$sendInputMessage = function(inputId, message){
    return(message)
  }
  local({
    func(fakesession, ...)
  }, envir = fakesession)
}

#' internally used for debugging functions
#' @param ... see with_fake_session
#' @param .args same as ...
#' @param .func function to pass to with_fake_session
get_fake_updated_message <- function(..., .args = list(), .func = NULL){
  .args = c(
    list(...),
    .args
  )
  .args = dipsaus::drop_nulls(.args)
  if(sum(names(.args) %in% c('choices', 'selected')) > 0){
    .func = shiny::updateSelectInput
  }
  
  if(is.function(.func)){
    .args = c(
      func = .func,
      .args
    )
    return(do.call(with_fake_session, args = .args))
  }else{
    return(.args)
  }
}



#' Customized Shiny Elements
#' @param inputId character, input id
#' @param width integer from 1-12
#' @param ... passed to \code{\link[shiny]{uiOutput}}
#' @export
customizedUI <- function(inputId, width = 12L, ...){
  shiny::uiOutput(inputId, ...)
}



div_elastic <- function(css_selector, any = TRUE){
  div(
    class = 'btn btn-box-tool rave-elastic-btn force-recalculate',
    tag('i', list(class = "fa fa-expand"))
  ) ->
    re
  
  if(!missing(css_selector)){
    re = htmltools::tagAppendAttributes(re, 'data-target' = css_selector)
  }
  re
}


expand_box <- function(
  ..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
  background = NULL, width = 12L, height = NULL, collapsible = TRUE,
  collapsed = FALSE, box_link = NULL
){
  boxClass <- "box"
  boxId = paste0(sample(c(LETTERS, letters, 0:9), 16), collapse = '')
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", htmltools::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status
    buttonStatus %?<-% "default"
    collapseIcon <- if (collapsed)
      "plus"
    else "minus"
    collapseTag <-
      div(class = "box-tools pull-right",
          if (is.null(box_link)) NULL else tags$a(
            class = paste0("btn btn-box-tool"),
            href = box_link,
            target = "_blank",
            shiny::icon('question-circle')
          ),
          tags$button(
            class = "btn btn-box-tool force-recalculate",
            shiny::icon('refresh')
          ),
          tags$button(
            class = "btn btn-box-tool rave-elastic-btn force-recalculate",
            'data-target' = paste0('#', boxId),
            shiny::icon('expand')
          ),
          tags$button(
            class = paste0("btn btn-box-tool"),
            `data-widget` = "collapse",
            shiny::icon(collapseIcon)
          ))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag)
  }
  div(
    class = if (!is.null(width))
      paste0("col-sm-", width),
    div(
      class = boxClass,
      id = boxId,
      style = if (!is.null(style))
        style,
      headerTag,
      div(class = "box-body", ...),
      if (!is.null(footer))
        div(class = "box-footer", footer)
    )
  )
}



parsers = new.env()
parsers[['.default_parser']] = function(expr, env = environment()){
  fun = eval(expr[[1]], envir = env)
  width = eval(expr[['width']])
  if(length(width) != 1 || !is.numeric(width)){
    width = 12L
  }else{
    expr[['width']] = NULL
  }
  
  expr = match.call(
    definition = fun,
    call = expr
  )
  outputId = expr[['outputId']]
  inputId = expr[['inputId']]
  if(length(inputId)){
    # this is an input
    expr[['inputId']] = as.call(list(quote(ns), inputId))
    observers = function(input, output, session, local_data, exec_env){
      observe({
        # rave_context(senv = exec_env, tpos = 1L)
        rave_context(senv = exec_env)
        val = input[[inputId]]
        t = Sys.time()
        
        # Make sure the data has been loaded
        if(isolate(local_data$has_data)){
          
          # Assign variables into param_env and runtime env so that rave_execute can access to the variables
          exec_env$param_env[[inputId]] = val
          exec_env$runtime_env[[inputId]] = val
          
          # Check variable type. By default it triggers rave_execute and refresh the whole process
          # However there are two exceptions
          if( inputId %in% exec_env$rendering_inputIds ){
            # this input only updates renderings, skip main
            local_data$has_results = t
          }else if( !inputId %in% exec_env$manual_inputIds ){
            # need to run rave_execute
            # cat2('Input ', inputId, ' is changed')
            local_data$last_input = t
          }
        }
        # cat2('Assigned input - ', inputId, sprintf(' (%.3f sec)', as.numeric(Sys.time() - t)))
      })
    }
    updates = function(session, ..., .args = list()){
      args = c(list(...), .args)
      if(length(args) == 0){
        catgl('Nothing to update')
        return()
      }
      fun_name = utils::tail(unlist(stringr::str_split(as.character(expr[[1]]), ':')), 1)
      fun_name = stringr::str_c('update', stringr::str_to_upper(stringr::str_sub(fun_name, end = 1L)), stringr::str_sub(fun_name, start = 2L))
      
      args[['inputId']] %?<-% inputId
      args[['session']] = session
      
      if('' %in% names(args)){
        nms = names(args)
        sel = nms == 'value'
        nms[which(nms == '')[1]] = 'value'
        names(args) = nms
        if(sum(sel)){
          args = args[!sel]
        }
      }
      
      do.call(fun_name, args)
    }
  }else{
    # this is an output
    expr[['outputId']] = as.call(list(quote(ns), outputId))
    updates = function(session, ..., .args = list()){}
    observers = function(input, output, session, local_data, exec_env){
      if(length(outputId)){
        fun_name = utils::tail(unlist(stringr::str_split(as.character(expr[[1]]), ':')), 1)
        fun_name = stringr::str_c('render', stringr::str_to_upper(stringr::str_sub(fun_name, end = 1L)), stringr::str_sub(fun_name, start = 2L))
        fun_name = stringr::str_replace(fun_name, 'Output', '')
        
        output[[outputId]] = do.call(fun_name, args = list(quote({
          rave_context(senv = exec_env)
          local_data$show_results
          if(isolate(local_data$has_data)){
            func = get0(outputId, envir = exec_env$param_env, inherits = TRUE)
            if(is.function(func)){
              func()
            }
          }
        })))
      }
    }
  }
  
  args = as.list(expr)[-1]
  
  list(
    expr = expr,
    inputId = inputId,
    outputId = outputId,
    args = args,
    initial_value = args[['value']],
    width = width,
    observers = observers,
    updates = updates,
    margin = NULL
  )
}


comp_parsers = list(
  set = function(fun_name, pkg_name, handler, .env = NULL){
    if(missing(pkg_name) || !length(pkg_name) || is.blank(pkg_name)){
      pkg_name = '.default'
    }
    parsers[[pkg_name]] %?<-% list()
    .env %?<-% parsers
    environment(handler) = .env
    parsers[[pkg_name]][[fun_name]] = handler
  },
  has = function(fun_name, pkg_name){
    if(missing(pkg_name) || !length(pkg_name) || is.blank(pkg_name)){
      pkg_name = '.default'
    }
    parsers[[pkg_name]] %?<-% list()
    if(fun_name %in% names(parsers[[pkg_name]])){
      ps = parsers[[pkg_name]][[fun_name]]
      if(is.function(ps)){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }else{
      FALSE
    }
  },
  get = function(fun_name, pkg_name = '.default'){
    if(comp_parsers$has(fun_name, pkg_name)){
      ps = parsers[[pkg_name]][[fun_name]]
    }else{
      ps = parsers[['.default_parser']]
    }
    
    f = function(...){
      re = ps(...)
      if(pkg_name != '.default'){
        fun = list(as.symbol('::'), as.symbol(pkg_name), as.symbol(fun_name))
        re$expr[[1]] = as.call(fun)
      }
      re
    }
    return(f)
  },
  parse_quo = function(quo){
    expr = rlang::quo_squash(quo)
    env = rlang::quo_get_env(quo)
    stopifnot2(is.call(expr), msg = sprintf(
      'Need a function call but given: "%s"', deparse(expr)
    ))
    
    fun = eval(expr[[1]], envir = env)
    fun_expr = as.character(expr[[1]])
    fun_name = utils::tail(fun_expr, 1)
    if(length(fun_expr) > 1){
      fun_pkg = fun_expr[2]
    }else{
      fun_env = environment(fun)
      if(isNamespace(fun_env)){
        fun_pkg = environmentName(fun_env)
      }else{
        fun_pkg = ''
      }
    }
    
    fun = comp_parsers$get(fun_name = fun_name, pkg_name = fun_pkg)
    fun(expr, env)
  }
)


# register
parsers[['shiny']] = list(
  'selectInput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    inputId = re$inputId
    
    re$initial_value = re$args[['selected']]
    
    re$updates = function(session, ..., .args = list()){
      args = c(list(...), .args)
      if(length(args) == 0){
        return()
      }
      
      args[['inputId']] %?<-% inputId
      args[['session']] = session
      
      sel = '' == names(args)
      if(any(sel)){
        nms = names(args)
        sel1 = nms == 'selected'
        nms[sel] = 'selected'
        names(args) = nms
        if(sum(sel1)){
          args = args[!sel1]
        }
      }
      
      
      do.call(shiny::updateSelectInput, args = args)
    }
    
    return(re)
  },
  'htmlOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = shiny::renderText({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    return(re)
  },
  'verbatimTextOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = shiny::renderPrint({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    return(re)
  },
  'plotOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = shiny::renderPlot({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    re$margin = -10
    return(re)
  }
)

parsers[['DT']] = list(
  'DTOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = DT::renderDT({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    return(re)
  },
  'dataTableOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = DT::renderDataTable({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    return(re)
  }
)

parsers[['dipsaus']] = list(
  'compoundInput2' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    inputId = re$inputId
    re$updates = function(session, ..., .args = list()){
      args = c(list(...), .args)
      
      if(length(args) == 0){
        return()
      }
      args[['session']] = session
      args[['inputId']] %?<-% inputId
      
      do.call(dipsaus::updateCompoundInput2, args = args)
    }
    
    return(re)
  },
  'actionButtonStyled' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    inputId = re$inputId
    re$updates = function(session, ..., .args = list()){
      args = c(list(...), .args)
      
      if(length(args) == 0){
        return()
      }
      args[['session']] = session
      args[['inputId']] %?<-% inputId
      
      do.call(dipsaus::updateActionButtonStyled, args = args)
    }
    
    return(re)
  }
)

parsers[['rave']] = list(
  'customizedUI' = function(expr, env = environment()){
    # expr : customizedUI('id')
    expr = match.call(customizedUI, expr)
    inputId = expr[['inputId']]
    width = eval(expr[['width']])
    if(!is.null(width)){
      expr[['width']] = NULL
    }else{
      width = 12L
    }
    args = as.list(expr)[-1];
    
    expr[['inputId']] = as.call(list(quote(ns), inputId))
    
    expr[[1]] = quote(shiny::uiOutput)
    observers = function(input, output, session, local_data, exec_env){
      output[[inputId]] <- shiny::renderUI({
        rave_context(senv = exec_env)
        if(local_data$has_data){
          func = exec_env$static_env[[inputId]]
          tryCatch({
            if(is.function(func)){
              func()
            }
          }, error = function(e){
            lapply(utils::capture.output(traceback(e)), function(x){
              catgl(x, level = 'ERROR')
            })
          })
        }
      })
    }
    
    list(
      expr = expr,
      inputId = inputId,
      outputId = inputId,
      args = args,
      initial_value = NULL,
      width = width,
      observers = observers,
      updates = do_nothing
    )
  }
)

parsers[['threeBrain']] = list(
  'threejsBrainOutput' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    outputId = re$outputId
    
    re$observers = function(input, output, session, local_data, exec_env){
      output[[outputId]] = threeBrain::renderBrain({
        rave_context(senv = exec_env)
        local_data$show_results
        if (isolate(local_data$has_data)) {
          func = get0(outputId, envir = exec_env$param_env,
                      inherits = TRUE)
          if (is.function(func)) {
            func()
          }
        }
      })
    }
    re$margin = -10
    return(re)
  }
)

comp_parser_template <- function(expr, update_fun, env = environment(), keyword = 'selected'){
  re = parsers[['.default_parser']](expr, env)
  inputId = re$inputId
  
  re$initial_value = re$args[[keyword]]
  
  re$updates = function(session, ..., .args = list()){
    args = c(list(...), .args)
    if(length(args) == 0){
      return()
    }
    
    args[['inputId']] %?<-% inputId
    args[['session']] = session
    
    sel = '' == names(args)
    if(any(sel)){
      nms = names(args)
      sel1 = nms == keyword
      nms[sel] = keyword
      names(args) = nms
      if(sum(sel1)){
        args = args[!sel1]
      }
    }
    
    
    do.call(update_fun, args = args)
  }
}

parsers[['shinyWidgets']] = list(
  'actionBttn' = function(expr, env = environment()){
    re = parsers[['.default_parser']](expr, env)
    inputId = re$inputId
    re$updates = function(session, ..., .args = list()){
      args = c(list(...), .args)
      
      if(length(args) == 0){
        return()
      }
      args[['session']] = session
      args[['inputId']] %?<-% inputId
      
      do.call(shiny::updateActionButton, args = args)
    }
    
    return(re)
  },
  'checkboxGroupButtons' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateCheckboxGroupButtons,
                         env = env, keyword = 'selected')
  },
  'awesomeCheckboxGroup' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateAwesomeCheckboxGroup,
                         env = env, keyword = 'selected')      
  },
  'prettyCheckboxGroup' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updatePrettyCheckboxGroup,
                         env = env, keyword = 'selected')      
  },
  'radioGroupButtons' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateRadioGroupButtons,
                         env = env, keyword = 'selected')      
  },
  'awesomeRadio' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateAwesomeRadio,
                         env = env, keyword = 'selected')      
  },
  'prettyRadioButtons' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updatePrettyRadioButtons,
                         env = env, keyword = 'selected')      
  },
  'pickerInput' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updatePickerInput,
                         env = env, keyword = 'selected')      
  },
  'sliderTextInput' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateSliderTextInput,
                         env = env, keyword = 'selected')      
  },
  'colorSelectorDrop' = function(expr, env = environment()){
    comp_parser_template(expr, do_nothing,
                         env = env, keyword = 'selected')      
  },
  'multiInput' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateMultiInput,
                         env = env, keyword = 'selected')      
  },
  'spectrumInput' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateSpectrumInput,
                         env = env, keyword = 'selected')      
  },
  'verticalTabsetPanel' = function(expr, env = environment()){
    comp_parser_template(expr, shinyWidgets::updateVerticalTabsetPanel,
                         env = env, keyword = 'selected')      
  }
)


comp_parser <- function(){
  comp_parsers
}


