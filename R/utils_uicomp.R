#' Customized UI Element
#' @param inputId character, input id
#' @param width integer from 1-12
#' @param ... passed to shiny::uiOutput
#' @export
customizedUI <- function(inputId, width = 12L, ...){
  shiny::uiOutput(inputId, ...)
}


div_elastic <- function(css_selector, any = T){
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
  background = NULL, width = 12L, height = NULL, collapsible = T,
  collapsed = FALSE
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


comp_parser <- function(){
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
              local_data$last_input = t
            }
          }
          # logger('Assigned input - ', inputId, sprintf(' (%.3f sec)', as.numeric(Sys.time() - t)))
        })
      }
      updates = function(session, ..., .args = list()){
        args = c(list(...), .args)
        if(length(args) == 0){
          logger('Nothing to update')
          return()
        }
        fun_name = tail(unlist(str_split(as.character(expr[[1]]), ':')), 1)
        fun_name = str_c('update', str_to_upper(str_sub(fun_name, end = 1L)), str_sub(fun_name, start = 2L))

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
          fun_name = tail(unlist(str_split(as.character(expr[[1]]), ':')), 1)
          fun_name = str_c('render', str_to_upper(str_sub(fun_name, end = 1L)), str_sub(fun_name, start = 2L))
          fun_name = str_replace(fun_name, 'Output', '')

          output[[outputId]] = do.call(fun_name, args = list(quote({
            local_data$show_results
            if(isolate(local_data$has_data)){
              func = get(outputId, envir = exec_env$param_env, inherits = T)
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


  comp_parser = list(
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
      if(comp_parser$has(fun_name, pkg_name)){
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
      assert_that(is.call(expr), msg = sprintf(
        'Need a function call but given: "%s"', deparse(expr)
      ))

      fun = eval(expr[[1]], envir = env)
      fun_expr = as.character(expr[[1]])
      fun_name = tail(fun_expr, 1)
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

      fun = comp_parser$get(fun_name = fun_name, pkg_name = fun_pkg)
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
        output[[outputId]] = do.call(shiny::renderText, args = list(quote({
          local_data$show_results
          if (isolate(local_data$has_data)) {
            func = get(outputId, envir = exec_env$param_env,
                       inherits = T)
            if (is.function(func)) {
              func()
            }
          }
        })))
      }
      return(re)
    },
    'verbatimTextOutput' = function(expr, env = environment()){
      re = parsers[['.default_parser']](expr, env)
      outputId = re$outputId

      re$observers = function(input, output, session, local_data, exec_env){
        output[[outputId]] = do.call(shiny::renderPrint, args = list(quote({
          local_data$show_results
          if (isolate(local_data$has_data)) {
            func = get(outputId, envir = exec_env$param_env,
                       inherits = T)
            if (is.function(func)) {
              func()
            }
          }
        })))
      }
      return(re)
    },
    'plotOutput' = function(expr, env = environment()){
      re = parsers[['.default_parser']](expr, env)
      outputId = re$outputId
      
      re$observers = function(input, output, session, local_data, exec_env){
        output[[outputId]] = do.call(shiny::renderPlot, args = list(quote({
          local_data$show_results
          if (isolate(local_data$has_data)) {
            func = get(outputId, envir = exec_env$param_env,
                       inherits = T)
            if (is.function(func)) {
              func()
            }
          }
        })))
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
        output[[outputId]] = do.call(do.call('::', list('DT', 'renderDT')), args = list(quote({
          local_data$show_results
          if (isolate(local_data$has_data)) {
            func = get(outputId, envir = exec_env$param_env,
                       inherits = T)
            if (is.function(func)) {
              func()
            }
          }
        })))
      }
      return(re)
    },
    'dataTableOutput' = function(expr, env = environment()){
      re = parsers[['.default_parser']](expr, env)
      outputId = re$outputId
      re$observers = function(input, output, session, local_data, exec_env){
        output[[outputId]] = do.call(do.call('::', list('DT', 'renderDataTable')), args = list(quote({
          local_data$show_results
          if (isolate(local_data$has_data)) {
            func = get(outputId, envir = exec_env$param_env,
                       inherits = T)
            if (is.function(func)) {
              func()
            }
          }
        })))
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
          if(local_data$has_data){
            func = exec_env$static_env[[inputId]]
            tryCatch({
              if(is.function(func)){
                func()
              }
            }, error = function(e){
              lapply(capture.output(traceback(e)), function(x){
                logger(x, level = 'ERROR')
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
    },
    'compoundInput' = function(expr, env = environment()){
      expr = match.call(compoundInput, expr)
      inputId = expr[['inputId']]
      expr[['inputId']] = as.call(list(quote(ns), inputId))
      args = as.list(expr)[-1]
      max_ncomp = eval(expr[['max_ncomp']])
      max_ncomp %?<-% formals(compoundInput)$max_ncomp
      # parse components
      components = expr[['components']]
      if(as.character(components[[1]])[[1]] == '{'){
        components = as.list(components)[-1]
      }else{
        components = list(components)
      }



      lapply(components, function(sub_expr){
        quo = rlang::as_quosure(sub_expr, env = env)
        comp_parser$parse_quo(quo)
      }) ->
        sub_comps

      names(sub_comps) = sapply(sub_comps, function(x){x$inputId})
      sub_names = names(sub_comps)

      observers = function(input, output, session, local_data, exec_env){

        observe({
          val = input[[inputId]]
          if(isolate(local_data$has_data)){

            t = Sys.time()
            exec_env$param_env[[inputId]] = val
            exec_env$runtime_env[[inputId]] = val
            
            if( inputId %in% exec_env$rendering_inputIds ){
              # this input only updates renderings, skip main
              local_data$has_results = t
            }else if( !inputId %in% exec_env$manual_inputIds ){
              # need to run rave_execute
              local_data$last_input = t
            }
          }

        })
      }

      updates = function(session, ..., .args = list()){
        args = c(list(...), .args)
        base_args = args$initialize

        to = args[['to']]
        if(is.null(to)){
          to = length(args$value)
        }

        if(!is.null(to)){
          args[['to']] = NULL
          if(to > 0){
            updateCompoundInput(session, inputId, to = to)
          }
        }

        lapply(seq_len(max_ncomp), function(ii){
          base_args
          if(ii <= length(args$value)){
            more_args = args$value[[ii]]
            for(ma in names(more_args)){
              val = more_args[[ma]]
              if(is.list(val)){
                tmp = c(val[['value']], val[['selected']])
                if(!length(tmp) && length(val)){
                  tmp = val[[1]]
                }
                val = tmp
              }
              base_args[[ma]] = c(base_args[[ma]], list(val))
            }
          }
          lapply(sub_names, function(nm){
            sub_id = sprintf('%s_%s_%d', inputId, nm, ii)
            value = base_args[[nm]]
            sub_comps[[nm]]$updates(session = session, inputId = sub_id, .args = value)
          })
        })

      }

      list(
        expr = expr,
        inputId = inputId,
        args = args,
        observers = observers,
        updates = updates,
        initial_value = list(sapply(sub_comps, function(co) {
          co$initial_value
        }, simplify = F, USE.NAMES = T))
      )
    }
  )

  parsers[['threeBrain']] = list(
    'threejsBrainOutput' = function(expr, env = environment()){
      re = parsers[['.default_parser']](expr, env)
      outputId = re$outputId

      re$observers = function(input, output, session, local_data, exec_env){
        output[[outputId]] = do.call(
          do.call('::', list('threeBrain', 'renderBrain')), 
          args = list(quote({
            local_data$show_results
            if (isolate(local_data$has_data)) {
              func = get(outputId, envir = exec_env$param_env,
                         inherits = T)
              if (is.function(func)) {
                func()
              }
            }
          }))
        )
      }
      re$margin = -10
      return(re)
    }
  )




  comp_parser
}












