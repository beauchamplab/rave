# Module exec environment
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

data_repository = new.env()


#' @export
getDefaultCacheEnvironment <- function(
  session = shiny::getDefaultReactiveDomain()
){
  data_env = getDefaultDataRepository(session = session, session_based = T)
  if(!exists('.cache_env', envir = data_env, inherits = F)){
    data_env$.cache_env = new.env(parent = baseenv())
    data_env$.cache_env$.keys = c()
  }
  return(data_env$.cache_env)
}


#' @export
getDefaultDataRepository <- function(
  session = shiny::getDefaultReactiveDomain(),
  session_id,
  session_based = NULL
){
  if(is.null(session_based)){
    session_based = rave_opts$get_options('session_based_datarepo')
  }
  if(missing(session_id)){
    if(!session_based){
      session_id = NULL
    }else{
      session_id = add_to_session(session)
    }
  }
  if(!is.character(session_id)){
    session_id = '.TEMP'
  }
  if(!exists(session_id, envir = data_repository)){
    e = new.env(parent = globalenv())
    e$.clean = function(){
      if(is.null(session)){
        return(invisible())
      }
      rm(list = ls(envir = e, all.names = T), envir = e, inherits = F)
      data_repository$.sessions[[session_id]] = NULL
    }
    assign(session_id, e, envir = data_repository)


    if(!is.null(session)){
      new_l = list(session); names(new_l) = session_id
      data_repository$.sessions = c(
        data_repository$.sessions, new_l
      )
    }
  }
  return(get(session_id, envir = data_repository))
}

#' @export
ModuleEnvir <- R6::R6Class(
  classname = 'ModuleEnvir',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    exec_env = NULL,
    cache_env = NULL
  ),
  public = list(
    module_id = '',
    label_name = '',
    script_path = '',
    script = '',
    author = NULL,
    version = NULL,
    packages = NULL,
    rmd_path = NULL,
    print = function(){
      cat('Module Name:', self$label_name, '\n')
      cat('Version:', self$version, '\n')
      cat('Script Path:', self$script_path, '\n')
      cat('Author(s):\n')
      for(a in self$author){
        cat(' -', a, '\n')
      }
    },
    initialize = function(
      module_id,
      label_name,
      script_path,
      author = NULL,
      version = '0',
      packages = NULL,
      .script_content = NULL,
      rmd_path = NULL
    ){
      self$module_id = module_id
      self$label_name = label_name
      self$author = author
      self$version = version
      self$packages = c('rave', packages)
      self$rmd_path = rmd_path
      private$cache_env = list()

      # validate script_path
      if(missing(script_path)){
        assertthat::assert_that(!is.null(.script_content), msg = 'Script Path not specified')
        script_path = file.path(dirname(rmd_path), '.rave_tmp.R')
        writeLines(.script_content, script_path)
      }

      assertthat::validate_that(file.exists(script_path), msg = sprintf('[File Not Found] %s', script_path))
      script_path = tools::file_path_as_absolute(script_path)
      self$script_path = script_path

    },
    get_or_new_exec_env = function(data_env = rave::getDefaultDataRepository(),
                            session = shiny::getDefaultReactiveDomain()){
      session_id = add_to_session(session)
      if(is.null(session_id)){
        session_id = '.TEMP'
      }

      if(is.null(private$exec_env[[session_id]])){
        private$exec_env[[session_id]] = ExecEnvir$new(data_env = data_env, session = session)
        private$exec_env[[session_id]]$register_module(self)
      }
      if(!session_id %in% names(private$cache)){
        private$cache_env[[session_id]] = new.env()
      }
      return(private$exec_env[[session_id]])
    },
    load_script = function(session = shiny::getDefaultReactiveDomain()){
      # get
      static_env = self$get_or_new_exec_env(session = session)$static_env
      runtime_env = self$get_or_new_exec_env(session = session)$runtime_env
      src = readLines(self$script_path)
      parsed = parse(text = src)
      for(i in 1:length(parsed)){
        comp = lazyeval::as.lazy(str_c(parsed[i]), env = static_env)
        tryCatch({
          lazyeval::lazy_eval(comp)
          # logger('[Parsed]: ', str_c(parsed[i]), level = 'DEBUG')
        }, error = function(e){
          logger('[Ignored]: ', str_c(parsed[i]), level = 'INFO')
          # logger(e, level = 'WARNING')
        })
      }

      # re-direct function environment to runtime-env where rave_execute take place.
      for(nm in ls(static_env, all.names = T)){
        obj = get(nm, envir = static_env)
        if(is.function(obj)){
          environment(obj) <- runtime_env
          assign(nm, obj, envir = static_env)
        }
      }

      # add package information into static_env
      additional_pkgs = str_match(src, '(require|library)\\(([a-zA-Z0-9_]*)')[,3]
      additional_pkgs = additional_pkgs[!is.na(additional_pkgs)]

      static_env$..packages = c(additional_pkgs, self$packages)

    },
    cache = function(key, val, session, replace = FALSE){
      session_id = add_to_session(session)
      if(is.null(session_id)){
        session_id = '.TEMP'
      }
      if(!is.environment(private$cache_env[[session_id]])){
        private$cache_env[[session_id]] = new.env()
      }
      env = private$cache_env[[session_id]]
      if(!replace && exists(key, envir = env)){
        return(get(key, envir = env))
      }else{
        # assume val is evaluated
        if(!missing(val) && !is.null(val)){
          force(val)
          assign(key, val, envir = env)
          return(val)
        }else{
          return(NULL)
        }
      }
    },
    render_ui = function(session = shiny::getDefaultReactiveDomain()){
      e = self$get_or_new_exec_env(session = session)
      shiny::fluidRow(
        e$generate_input_ui(),
        e$generate_output_ui()
      )

    },
    clean = function(session = shiny::getDefaultReactiveDomain(),
                     session_id){

      if(missing(session_id)){
        session_id = add_to_session(session)
      }
      if(is.character(session_id)){
        # clear cache
        cache_env = private$cache_env[[session_id]]
        if(is.environment(cache_env)){
          rm(list = ls(envir = cache_env, all.names = T), envir = cache_env)
        }
        private$cache_env[[session_id]] = NULL

        # Clear runtime_env
        exec_env = private$exec_env[[session_id]]
        if('ExecEnvir' %in% class(exec_env)){
          exec_env$clean()
        }
        private$exec_env[[session_id]] = NULL
      }
    }
  )
)


#' @import magrittr
#' @export
ExecEnvir <- R6::R6Class(
  classname = 'ExecEnvir',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    module_env = NULL,
    data_env = NULL,
    session = NULL,
    cache_env = NULL,
    inputs = NULL,
    outputs = NULL,
    update = NULL,
    execute = NULL,
    tabsets = NULL
  ),
  public = list(
    wrapper_env = NULL,
    static_env = NULL,
    runtime_env = NULL,
    ns = NULL,
    input_update = NULL,
    register_output_events = NULL,
    finalize = function(){
      logger(sprintf('[%s] Runtime Environment Removed.', private$module_env$module_id))
    },
    print = function(...){
      cat('- wrapper environment -\n')
      cat(ls(self$wrapper_env))
      cat('\n- static environment -\n')
      cat(ls(self$static_env))
      cat('\n- runtime environment -\n')
      cat(ls(self$runtime_env))
    },
    clean = function(){
      # WARNING: this is not clean, but should be able to clear most of the large objects
      clear_env(self$runtime_env)
      clear_env(self$static_env)
      clear_env(private$cache_env)
      clear_env(self$wrapper_env)
    },
    initialize = function(data_env = rave::getDefaultDataRepository(),
                          session = shiny::getDefaultReactiveDomain()){
      private$data_env = data_env
      private$session = session

      # wrapper can access data repository, and contains all util functions
      self$wrapper_env = new.env(parent = data_env)

      # static_env contains user self-defined functions. once initialized, they can
      # be read-only (in most of the cases).
      self$static_env = new.env(parent = self$wrapper_env)

      # runtime_env, all variables will be stored within this environment, is the
      # one that real execute take place
      self$runtime_env = new.env(parent = self$static_env)
      private$cache_env = new.env()
      private$cache_env$.keys = c()
      self$ns = base::I

      self$wrapper_env$rave_inputs = self$rave_inputs
      self$wrapper_env$rave_outputs = self$rave_outputs
      self$wrapper_env$rave_updates = self$rave_updates
      self$wrapper_env$rave_execute = self$rave_execute
      self$wrapper_env$cache = self$cache
      self$wrapper_env$cache_input = self$cache_input
      self$wrapper_env$rave_ignore = function(...){}
      self$wrapper_env$rave_prepare = function(...){} # do nothing
      self$wrapper_env$source = function(file, local = T, ...){
        # try to find file, if not, try to use the one under modules's dir
        if(!file.exists(file)){
          logger('File [', file, '] does not exists, try to look for it.', level = 'INFO')
          dir = dirname(private$module_env$script_path)
          file = tail(as.vector(str_split(file, '/', simplify = T)), 1)
          file = file.path(dir, file)
        }
        logger('Trying to source [', file, ']')
        self$static_env$.__tmp_file = file
        eval(quote(base::source(.__tmp_file, local = T)), self$static_env)
      }
    },
    reset = function(inputs){
      if(shiny::is.reactivevalues(inputs)){
        inputs = shiny::reactiveValuesToList(inputs)
      }
      rm(list = ls(self$runtime_env), envir = self$runtime_env)
      for(nm in self$input_ids){
        assign(nm, inputs[[nm]], envir = self$runtime_env)
      }
    },
    names = function(x){
      if(is.list(x)){
        nm = base::names(x)
        if(length(x) != length(nm) && is.null(nm)){
          nm = rep('', length(x))
        }
      }else{
        nm = base::names(x)
      }
      return(nm)
    },
    register_module = function(module_env){
      if(!is.null(private$module_env)){
        logger('Overriding Module Environment.', level = 'WARNING')
      }
      private$module_env = module_env
      self$ns = shiny::NS(module_env$module_id)
    },
    rave_inputs = function(..., .tabsets = list()){
      private$tabsets = .tabsets
      dots = lazyeval::lazy_dots(...)
      re = lapply(dots, function(comp){
        comp = parse_shiny_inputs(comp, env = self$static_env)
        comp$expr = comp$.change_param(inputId = self$ns(comp$.inputId), .lazy = F)
        comp
      })
      nm = lapply(re, function(x){
        assign(x$.inputId, x$.args[[x$.value]], envir = self$static_env)
        x$.inputId
      })
      names(re) = nm
      private$inputs = re
      return(invisible(re))
    },
    rave_outputs = function(...){
      dots = lazyeval::lazy_dots(...)
      titles = names(dots)
      dots = lapply(1:length(dots), function(ii){
        comp = dots[[ii]]
        title = titles[[ii]]
        comp$env = self$static_env
        expr = comp$expr
        ui_func = expr[[1]]
        func_name = str_replace(ui_func, '^[\\w]+::', '')
        func_env = environment(eval(ui_func))
        env_name = environmentName(func_env)
        func = get(func_name, envir = func_env)

        arglist = as.list(expr)[-1]
        func_args = formals(func)
        func_args[names(func_args) %in% names(arglist)] <- NULL
        func_args_names = names(func_args)
        nm = names(arglist)
        unnamed = length(nm[nm == ''])
        if(unnamed > 0){
          names(arglist)[nm == ''] = func_args_names[1:min(length(func_args_names), unnamed)]
        }

        tryCatch({
          min(as.numeric(arglist$width), 12L)
        }, error = function(e){
          return(12L)
        }) ->
          width
        arglist[['width']] <- NULL
        outputId = arglist$outputId

        comp_info = list(
          outputId = outputId,
          title = title,
          width = width,
          arglist = arglist
        )
        init_args = arglist
        init_args$outputId = self$ns(arglist$outputId)

        for(event_name in c('click', 'hover', 'brush', 'dblclick')){
          if(event_name %in% names(arglist)){
            init_args[[event_name]] = self$ns(arglist[[event_name]])
          }
        }

        comp_info$init_expr = lazyeval::call_new(ui_func, .args = init_args)
        comp_info$observers = function(input, output, session, local_data){
          render_func = ui_register_function(str_c(env_name, '::', func_name))
          if(!is.null(render_func)){
            output[[outputId]] <- do.call(render_func$update_func, args = c(alist({
              local_data$show_results
              # get function with same output id name in runtime_env
              func = get(outputId, envir = self$runtime_env)
              eval_fun(func, env = NULL)
            }),
              render_func$default_args
            ))
          }

          events = c(
            'click', 'hover', 'brush', 'dblclick'
          )
          events = events[events %in% names(arglist)]
          if(length(events)){
            lapply(events, function(event_name){
              event_id = arglist[[event_name]]
              observe({
                e = input[[event_id]]
                func = get(event_id, envir = self$runtime_env)
                eval_fun(func, args = list(
                  event = e,
                  env = self$runtime_env
                ), env = self$runtime_env)
                local_data$force_render = Sys.time()
              })
            })
          }

        }
        comp_info
      })

      names = unlist(lapply(dots, function(x){x$outputId}))
      names(dots) = names
      private$outputs = dots
      self$register_output_events = function(input, output, session, local_data){
        if(length(private$outputs)){
          lapply(private$outputs, function(comp){
            comp$observers(input, output, session, local_data)
          })
        }
      }
    },
    rave_updates = function(...){
      dots = lapply(lazyeval::lazy_dots(...), function(x){
        parse_call(x, self$runtime_env)
      })
      private$update = dots

      update = function(input, session = NULL, init = FALSE){
        input = dropNulls(input)
        if(!init){
          # Not yet implemented
          return(invisible())
        }
        if(is.null(session)){
          session = shiny::getDefaultReactiveDomain() #private$session
        }
        for(varname in names(private$update)){
          tryCatch({
            info = private$inputs[[varname]]
            lazy_comp = private$update[[varname]]
            valname = info$.value
            val = lazyeval::lazy_eval(lazy_comp)

            if(!is.list(val)){
              val = list(val)
              names(val) = valname
            }

            if(!is.null(input[[varname]])){
              val[[valname]] = input[[varname]]
            }

            once = TRUE # By default, all inputs only run once
            if(!is.null(val$once)){
              once = val$once
              val[['once']] <- NULL
            }
            val[['inputId']] = varname # info$inputId
            val[['session']] = session

            onced = self$cache_input('..onced', FALSE, read_only = T, sig = 'special')
            if(init){
              local({
                logger('Initializing Input - ', varname)
                tryCatch({
                  do.call(info$.update_func, args = val)
                }, error = function(e){
                  logger('Error in updating - ', varname, level = 'WARNING')
                })
              })
              next;
            }else{

              # if(!once){
              #   val[[valname]] <- self$cache(
              #     key = list(
              #       type = '.rave-inputs-Dipterix',
              #       inputId = varname
              #     ), NULL,
              #     global = self$is_global(varname),
              #     replace = FALSE)
              #   if(length(val)){
              #     val[['inputId']] = varname # info$inputId
              #     val[['session']] = session
              #     local({
              #       tryCatch({
              #         logger('Updating Input - [', varname, '] - ', val[[valname]])
              #         print(val)
              #         do.call(info$.update_func, args = val)
              #       }, error = function(e){})
              #     })
              #   }
              # }
            }
          },error = function(e){
            logger('Error in updating input ', varname, level = 'ERROR')
            s = capture.output(traceback(e))
            lapply(s, logger, level = 'ERROR')
          })

        }
      }
      self$input_update = update
      return(invisible(dots))
    },
    rave_execute = function(...){
      exprs = lapply(lazyeval::lazy_dots(...), function(x){x$env = self$runtime_env; x})
      force(exprs)
      private$execute = function(plan = NULL, async = TRUE){
        local_env = self$runtime_env
        local_env$.is_async = async
        async_future = NULL

        if(length(exprs)){
          for(i in 1:length(exprs)){
            if('async' != names(exprs)[i]){
              lazyeval::lazy_eval(exprs[[i]])
              # tryCatch({
              #   lazyeval::lazy_eval(exprs[[i]])
              # }, error = function(e){
              #   logger('Error in executing ', private$module_env$module_id, level = 'ERROR')
              #   s = capture.output(traceback(e))
              #   lapply(s, logger, level = 'ERROR')
              # })

            }
          }
        }

        if(async && 'async' %in% names(exprs)){
          e = exprs[['async']]
          .env = e$env
          .env$..async.. = e
          async({
            if(is.character(..packages)){
              lapply(..packages, function(p){
                do.call('require', args = list(
                  package = p,
                  character.only = T
                ))
              })
            }
            lazyeval::lazy_eval(..async..)
            # large object won't be returned
            env = ..async..$env
            nms = ls(envir = env)
            sapply(nms, function(nm){
              tryCatch({
                pryr::object_size(get(nm, envir = env))
              }, error = function(e){
                return(0)
              })
            }) -> s
            nms = nms[s > rave_opts$get_options('big_object_size')]
            if(length(nms)){
              rm(list = nms, envir = env)
            }
            return(env)
          }, plan = plan, envir = .env) ->
            async_future
        }

        return(list(
          async_future = async_future,
          local_env = local_env
        ))
      }
    },
    cache = function(key, val, global = FALSE, replace = FALSE,
                     session = shiny::getDefaultReactiveDomain()){
      .key = str_c(unlist(key, recursive = T, use.names = F), collapse = ', ')
      key = as.character(digest::digest(key))
      if(global){
        env = getDefaultCacheEnvironment(session = session)
      }else{
        env = private$cache_env
      }
      if(!replace){
        if(key %in% env$.keys){
          return(get(key, envir = env))
        }
        if(!global && key %in% private$cache_env$.keys){
          # try to get from local
          return(get(key, envir = private$cache_env))
        }
      }
      if(missing(val)){
        return(NULL)
      }

      # save cache
      expr = lazyeval::lazy(val)
      expr$env = self$runtime_env
      val = NULL
      try({
        val = lazyeval::lazy_eval(expr, data = list())
      })
      str_val = safe_str_c(val, collapse = ', ')

      logger('Caching', ifelse(global, ' (global)', ''), ' - [', .key,'] - ', str_val)
      assign(key, val, envir = env)
      env$.keys = unique(c(env$.keys, key))
      return(val)
    },
    cache_input = function(inputId, val = NULL, read_only = T, sig = NULL){
      is_global = self$is_global(inputId)
      if(read_only){
        v = self$cache(
          key = list(
            type = '.rave-inputs-Dipterix',
            inputId = inputId,
            sig = sig
          ),
          global = is_global,
          replace = F)
        if(is.null(v)){
          return(val)
        }
      }else{
        v = self$cache(
          key = list(
            type = '.rave-inputs-Dipterix',
            inputId = inputId,
            sig = sig
          ), val,
          global = is_global,
          replace = !read_only)
      }
      return(v)
    },
    generate_input_ui = function(sidebar_width = 3){
      # generate inputs
      input_names = names(private$inputs)
      tabsets = private$tabsets

      tabnames = names(tabsets)
      varnames = unlist(tabsets)
      localnames = input_names[!input_names %in% varnames]
      if(length(localnames)){
        tabsets$`Local Variables` = localnames
        tabnames = c(tabnames, 'Local Variables')
      }
      column = function(..., width, offset = 0L){
        shiny::column(width = width, ..., offset = offset)
      }


      lapply(tabnames, function(tnm){
        var_names = str_c(tabsets[[tnm]])
        var_names = var_names[var_names %in% input_names]

        lapply(var_names, function(nm){
          expr = private$inputs[[nm]]$expr
        }) %>%
          lazyeval::as.lazy_dots(env = self$static_env) %>%
          lazyeval::lazy_eval() %>%
          tagList() %>%
          shinydashboard::box(
            title = tnm,
            collapsible = T,
            width = 12
          ) %>%
          fluidRow()
        }) %>%
        tagList() %>%
        column(
          fluidRow(
            shinydashboard::box(
              title = 'Others',
              width = 12,
              h4('Export'),
              hr(),
              actionButton(self$ns('.gen_report'), 'Export Report'),
              actionButton(self$ns('.force_run'), 'Force Run')
            )
          ),
          width = sidebar_width
        )
    },
    generate_output_ui = function(sidebar_width = 3){
      column = function(..., width, offset = 0L){
        shiny::column(width = width, ..., offset = offset)
      }

      nms = names(private$outputs)
      lapply(nms, function(nm){
        comp = private$outputs[[nm]]
        expr = comp$init_expr
        shinydashboard::box(
          width = comp$width,
          title = comp$title,
          collapsible = T,
          lazyeval::lazy_eval(lazyeval::as.lazy(expr, env = self$static_env))
        )
      }) %>%
        tagList() %>%
        fluidRow() %>%
        column(width = 12 - sidebar_width)
    },
    generate_results = function(env, async = TRUE){
      env$.rave_future <- private$execute(plan = NULL, async = async)
    },
    is_global = function(inputId){
      tabsets = private$tabsets
      if(length(tabsets) == 0){
        return(FALSE)
      }
      nms = names(tabsets)
      ts = unlist(tabsets[str_detect(str_to_lower(nms), 'global')])
      if(length(ts) == 0){
        return(FALSE)
      }
      return(inputId %in% ts)
    }
  ),
  active = list(
    input_ids = function(){
      names(private$inputs)
    },
    input_labels = function(){
      re = lapply(private$inputs, function(x){x$.args$label})
      names(re) = names(private$inputs)
      return(re)
    },
    output_labels = function(){
      re = lapply(private$outputs, function(x){x$title})
      names(re) = names(private$outputs)
      return(re)
    },
    output_ids = function(){
      names(private$outputs)
    }
  )
)
















# functions for dev use
#' @export
rave_ignore <- function(...){
  dots <- lazyeval::lazy_dots(...)
  globalenv = globalenv()
  for(i in 1:length(dots)){
    dots[[i]]$env <- globalenv
    logger('> ', dots[[i]]$expr, level = 'INFO')
    lazyeval::lazy_eval(dots[[i]])
  }
}

#' @export
rave_inputs <- function(..., .tabsets = list(), .env = globalenv()){
  dots = lazyeval::lazy_dots(...)
  lapply(dots, function(comp){
    expr = comp$expr
    ui_func = expr[[1]]
    func_name = str_replace(ui_func, '^[\\w]+::', '')
    func_env = environment(eval(ui_func))
    env_name = environmentName(func_env)
    func = get(func_name, envir = func_env)
    info = ui_register_function(sprintf('%s::%s', env_name, func_name))
    arglist = as.list(expr)[-1]
    func_args = formals(func)
    func_args[names(func_args) %in% names(arglist)] <- NULL
    func_args_names = names(func_args)
    nm = names(arglist)
    unnamed = length(nm[nm == ''])
    if(unnamed > 0){
      names(arglist)[nm == ''] = func_args_names[1:min(length(func_args_names), unnamed)]
    }

    val_name = info$value

    inputId = arglist[['inputId']]
    value = arglist[[val_name]]
    if(!is.null(inputId) && inputId != ''){
      assign(inputId, value, envir = .env)
    }
    return(list(inputId = inputId, value = value, val_name = val_name))
  }) ->
    init_val

  nms = unlist(lapply(init_val, function(x){x$inputId}))
  names(init_val) = nms
  assign('.tmp_init', init_val, envir = .env)
}

#' @export
rave_outputs <- function(...){
  # do nothing
  return(invisible())
}


#' @export
rave_updates <- function(..., .env = globalenv()){

  res = list(...)
  if(exists('.tmp_init', envir = .env)){
    init_val = get('.tmp_init', envir = .env)
  }
  for(nm in names(res)){
    if(nm != '' && nm %in% names(init_val)){
      val = res[[nm]]
      if(is.list(val)){
        val = val[[init_val[[nm]][['val_name']]]]
      }
      if(!is.null(val)){
        assign(nm, val, envir = .env)
      }
    }
  }
}


#' @export
rave_execute <- function(..., .env = globalenv()){
  assign('.is_async', TRUE, envir = .env)
  dots <- lazyeval::lazy_dots(...)
  for(i in 1:length(dots)){
    dots[[i]]$env <- .env
    logger('> ', dots[[i]]$expr, level = 'INFO')
    lazyeval::lazy_eval(dots[[i]])
  }
}

#' @export
cache_input <- function(key, val, read_only = T){
  return(val)
}
