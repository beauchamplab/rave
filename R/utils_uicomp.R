comp_parser = function(){
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
          if(local_data$has_data){
            logger('Assign input - ', inputId)
            exec_env$param_env[[inputId]] = val
            exec_env$runtime_env[[inputId]] = val
            local_data$last_input = t
          }

        })
      }
      updates = function(session, ..., .args = list()){
        args = c(list(...), .args)
        if(length(args) == 0){
          logger('Nothing to update')
          return()
        }
        fun_name = tail(unlist(str_split(expr[[1]], ':')), 1)
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
      updates = function(...){}
      observers = function(input, output, session, local_data, exec_env){
        if(length(outputId)){
          fun_name = tail(unlist(str_split(expr[[1]], ':')), 1)
          fun_name = str_c('render', str_to_upper(str_sub(fun_name, end = 1L)), str_sub(fun_name, start = 2L))
          fun_name = str_replace(fun_name, 'Output', '')

          output[[outputId]] = do.call(fun_name, args = list(quote({
            local_data$show_results
            if(local_data$has_data){
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
      width = width,
      observers = observers,
      updates = updates
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
      assertthat::assert_that(is.call(expr), msg = sprintf(
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


        logger('Updating - ', args[['inputId']])
        print(args)
        print('---')

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
          if (local_data$has_data) {
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
    'compoundInput' = function(expr, env = environment()){
      expr = match.call(rave::compoundInput, expr)
      inputId = expr[['inputId']]
      expr[['inputId']] = as.call(list(quote(ns), inputId))
      args = as.list(expr)[-1]
      max_ncomp = eval(expr[['max_ncomp']])
      max_ncomp %?<-% formals(rave::compoundInput)$max_ncomp
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
          # lapply(seq_len(max_ncomp), function(ii){
          #   sub_ids = paste0(inputId, '_', sub_names, '_', ii)
          #   lapply(sub_ids, function(i){
          #     input[[i]]
          #   }) ->
          #     re
          #   names(re) = sub_names
          # }) ->
          #   val

          val = input[[inputId]]
          if(local_data$has_data){

            t = Sys.time()
            exec_env$param_env[[inputId]] = val
            exec_env$runtime_env[[inputId]] = val
            local_data$last_input = t
          }

        })
      }

      updates = function(session, ..., .args = list()){
        args = c(list(...), .args)
        base_args = args$initialize

        lapply(seq_len(max_ncomp), function(ii){
          base_args
          if(ii <= length(args$value)){
            more_args = args$value[[ii]]
            for(ma in names(more_args)){
              base_args[[ma]] = c(base_args[[ma]], more_args[[ma]])
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
        updates = updates
      )
    }
  )




  comp_parser
}
