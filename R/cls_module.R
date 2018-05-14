#' Environment for ECoG data and modules
#' As of rave-Ent, data_repository nolonger succeed from globalenv()
#' Instead, its parent is now baseenv()
#' All packages needed are imported via loadnamespace within modules
#' This will help create a clean environment for modules.
NULL


getDefaultReactiveDomain <- function(){
  session = shiny::getDefaultReactiveDomain()
  session %?<-% get0('session', envir = globalenv())
  return(session)
}

data_repository = new.env(parent = baseenv())


#' @export
getDefaultDataRepository <- function(
  session = getDefaultReactiveDomain(),
  session_id,
  session_based = NULL
){
  if(is.null(session_based)){
    session_based = rave_options('session_based_datarepo')
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
    e = new.env(parent = baseenv())
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
    externalpackage = FALSE,
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
      rmd_path = NULL,
      externalpackage = FALSE
    ){
      self$module_id = module_id
      self$label_name = label_name
      self$author = author
      self$version = version
      self$packages = c('rave', packages)
      self$rmd_path = rmd_path
      private$cache_env = list()
      self$externalpackage = externalpackage

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
    get_or_new_exec_env = function(session = getDefaultReactiveDomain(), ...){
      session_id = add_to_session(session)
      if(is.null(session_id)){
        session_id = '.TEMP'
      }
      if(self$externalpackage){
        parent_env = list(...)[['parent_env']]
        parent_env %?<-% do.call(base::loadNamespace, args = list(self$packages))
      }else{
        parent_env = globalenv()
      }

      if(is.null(private$exec_env[[session_id]])){
        private$exec_env[[session_id]] = ExecEnvir$new(session = session, parent_env = parent_env)
        private$exec_env[[session_id]]$register_module(self)
      }
      if(!session_id %in% names(private$cache)){
        private$cache_env[[session_id]] = new.env()
      }
      return(private$exec_env[[session_id]])
    },
    load_script = function(session = getDefaultReactiveDomain()){
      # read in script, get package info
      src = readLines(self$script_path)
      # add package information into static_env
      # additional_pkgs = str_match(src, '(require|library)\\(([a-zA-Z0-9_]*)')[,3]
      # additional_pkgs = additional_pkgs[!is.na(additional_pkgs)]
      #
      # loaded_pkgs = str_match(search(), '^package:(.+)')[,2]
      # loaded_pkgs = rev(loaded_pkgs[!is.na(loaded_pkgs)])
      #
      # all_packages = c(loaded_pkgs, self$packages, additional_pkgs)



      # get
      static_env = self$get_or_new_exec_env(session = session)$static_env
      runtime_env = self$get_or_new_exec_env(session = session)$runtime_env

      parsed = parse(text = src)
      for(i in 1:length(parsed)){
        comp = lazyeval::as.lazy(str_c(parsed[i]), env = static_env)
        tryCatch({
          lazyeval::lazy_eval(comp)
          # logger('[Parsed]: ', str_c(parsed[i]), level = 'DEBUG')
        }, error = function(e){
          logger('[Ignored]: ', str_c(parsed[i]), level = 'INFO')
          logger(paste(e, sep = '\n'), level = 'WARNING')
        })
      }

      # re-direct function environment to runtime-env where rave_execute take place.
      for(nm in ls(static_env, all.names = T)){
        if(is.function(static_env[[nm]])){
          environment(static_env[[nm]]) <- runtime_env
        }
      }

      # lockEnvironment(static_env)

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
    render_ui = function(session = getDefaultReactiveDomain()){
      e = self$get_or_new_exec_env(session = session)
      shiny::fluidRow(
        e$generate_input_ui(),
        e$generate_output_ui()
      )

    },
    clean = function(session = getDefaultReactiveDomain(),
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
  cloneable = TRUE,
  private = list(
    module_env = NULL,
    data_env = NULL,
    session = NULL,
    cache_env = NULL,
    inputs = NULL,
    outputs = NULL,
    update = NULL,
    tabsets = NULL
  ),
  public = list(
    wrapper_env = NULL,
    static_env = NULL,
    runtime_env = NULL,
    param_env = NULL,
    ns = NULL,
    input_update = NULL,
    register_output_events = NULL,
    register_input_events = NULL,
    execute = NULL,
    async_module = FALSE,
    finalize = function(){
      logger(sprintf('[%s] Runtime Environment Removed.', private$module_env$module_id))
    },
    print = function(...){
      cat('- wrapper environment -\n')
      cat(ls(self$wrapper_env))
      cat('\n- static environment -\n')
      cat(ls(self$static_env))
      cat('\n- param environment -\n')
      cat(ls(self$param_env))
      cat('\n- runtime environment -\n')
      cat(ls(self$runtime_env))
    },
    clean = function(){
      # WARNING: this is not clean, but should be able to clear most of the large objects
      clear_env(self$runtime_env)
      # clear_env(self$static_env)
      clear_env(private$cache_env)
      # clear_env(self$wrapper_env)
    },
    initialize = function(session = getDefaultReactiveDomain(),
                          parent_env = baseenv()){
      private$session = session

      # wrapper has active bindings to data repository which allow us
      # the access to data loaded in data repo. It'll be sealed (locked)
      self$wrapper_env = new.env(parent = parent_env)
      rave::rave_module_tools(self$wrapper_env)

      # static_env contains user self-defined functions. once initialized, they can
      # be read-only (in most of the cases).
      self$static_env = new.env(parent = self$wrapper_env)
      self$param_env = new.env(parent = self$static_env)
      self$param_env$..rave_future_env = new.env()


      # runtime_env, all variables will be stored within this environment, is the
      # one that real execute take place
      self$runtime_env = new.env(parent = self$param_env)
      self$static_env$.env = self$runtime_env
      private$cache_env = new.env()
      private$cache_env$.keys = c()
      self$ns = base::I

      self$wrapper_env$async_var = function(x, default = NULL){
        x_name = deparse(substitute(x))
        val = NULL
        if(is.environment(self$param_env[['..rave_future_env']])){
          val = self$param_env[['..rave_future_env']][[x_name]]
        }
        if(is.null(val)){
          return(default)
        }else{
          return(val)
        }
      }

      self$wrapper_env$rave_inputs = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::rave_inputs(...)
        }else{
          self$rave_inputs(...)
        }
      }
      self$wrapper_env$rave_outputs = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::rave_outputs(...)
        }else{
          self$rave_outputs(...)
        }
      }
      self$wrapper_env$rave_updates = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::rave_updates(...)
        }else{
          self$rave_updates(...)
        }
      }
      self$wrapper_env$rave_execute = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::rave_execute(...)
        }else{
          self$rave_execute(...)
        }
      }
      self$wrapper_env$cache = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::cache(...)
        }else{
          self$cache(...)
        }
      }
      self$wrapper_env$cache_input = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::cache_input(...)
        }else{
          self$cache_input(...)
        }
      }
      self$wrapper_env$rave_ignore = function(...){
        if(is.null(getDefaultReactiveDomain())){
          rave::rave_ignore(...)
        }
      }
      self$wrapper_env$rave_prepare = self$wrapper_env$rave_ignore # do nothing
      self$wrapper_env$source = function(file, local = T, ...){
        if(environmentIsLocked(self$static_env)){
          return()
        }

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

      self$wrapper_env$require = function(package, ..., character.only = TRUE){
        p = as.character(substitute(package))
        if(!p %in% installed.packages()[,1]){
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

      self$wrapper_env$library = self$wrapper_env$require

      lockEnvironment(self$wrapper_env)

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
    copy = function(
      session_id = '__fake_runtime_env__'
    ){
      # deep clone, but sharing the data, module environment
      fakesession = fake_session(rave_id = session_id)
      data_env = private$data_env

      new = private$module_env$get_or_new_exec_env(
        data_env = data_env, session = fakesession
      )

      capture.output(
        private$module_env$load_script(session = fakesession)
      )

      # migrate runtime_env, params only
      lapply(self$input_ids, function(nm){
        new$runtime_env[[nm]] = self$runtime_env[[nm]]
      })

      return(new)
    },
    execute_with = function(param, async = FALSE, plan = NULL){
      lapply(names(param), function(nm){
        assign(nm, param[[nm]], envir = self$runtime_env)
      })
      res = self$execute(plan = plan, async = async)
      if(async){
        logger('Execute_with async not implemented.')
      }
      return(invisible(self$runtime_env))
    },
    calculate_niml = function(inputId = 'electrode', func_name = 'niml_default'){
      param = sapply(self$input_ids, get, envir = self$runtime_env, simplify = F, USE.NAMES = T)
      new = self$copy()
      elecs = private$data_env$electrodes
      progress = rave:::progress('Export to NIML for SUMA.', max = length(elecs))
      on.exit({progress$close()})

      tryCatch({
        lapply_async(elecs, function(e){
          pm = param
          pm[[inputId]] = e
          new$execute_with(pm, async = F, plan = NULL)
          func = get(func_name, envir = new$runtime_env, inherits = T)
          return(func())
        }, .ncores = rave_options('max_worker'), .call_back = function(i){
          progress$inc(sprintf('Calculating %d (%d of %d)', elecs[i], i, length(elecs)))
        }) ->
          fs

        nelem = length(as.vector(fs[[1]]))
        nm = names(fs[[1]])
        dat = t(vapply(fs, function(x){as.numeric(x)}, numeric(nelem)))
        if(length(nm)){
          nm = stringr::str_trim(nm)
          nm[nm == ''] = paste0('Unamed_', which(nm == ''))
          nm = stringr::str_replace_all(nm, '[^a-zA-Z0-9]', '_')
          colnames(dat) = nm
        }
        return(dat)
      }, error = function(e){
        logger(str_c(capture.output({traceback()}), collapse = '\n'), level = 'ERROR')
        return(NULL)
      })




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
    rave_inputs = function(..., .tabsets = list(), .env = NULL){
      quos = rlang::quos(...)
      parsers = comp_parser()

      x = lapply(quos, parsers$parse_quo)
      names(x) = ids = sapply(x, function(comp){comp$inputId})

      if(!'Local Variables' %in% names(.tabsets)){
        rest_inputs = ids[!ids %in% unlist(.tabsets)]
        if(length(rest_inputs)){
          .tabsets[['Local Variables']] = c(rest_inputs)
        }
      }


      lapply(seq_along(.tabsets), function(ii){
        tabName = names(.tabsets)[ii]

        rlang::quo({
          do.call(shinydashboard::box, args = c(
            list(width = 12, title = !!tabName, collapsible = T),
            !!lapply(.tabsets[[ii]], function(inputIds){
              if(length(inputIds) == 1){
                comp = x[[inputIds]]
                return(comp$expr)
              }else{
                n = length(inputIds)
                mod2 = n %% 2
                mod3 = n %% 3
                if(mod3 == 0){
                  flex_basis = rep('flex-basis: 33%;', n)
                }else if(mod2 == 0){
                  flex_basis = rep('flex-basis: 50%;', n)
                }else if(mod3 == 1){
                  flex_basis = rep('flex-basis: 50%;', n)
                  flex_basis[length(flex_basis)] = 'flex-basis: 100%;'
                }else{
                  flex_basis = rep('flex-basis: 33%;', n)
                  flex_basis[length(flex_basis) - c(0:1)] = 'flex-basis: 50%;'
                }

                return(rlang::quo_squash(rlang::quo(
                  do.call(div, args = c(
                    list(class = 'rave-grid-inputs'),
                    !!lapply(seq_len(n), function(jj){
                      inputId = inputIds[[jj]]
                      rlang::quo_squash(rlang::quo(
                        do.call(div, args = c(
                          list(style = !!flex_basis[[jj]], !!x[[inputId]]$expr)
                        ))
                      ))
                    })
                  ))
                )))
              }
            })
          ))

        })

      }) ->
        ui_inputs

      rlang::quo({
        do.call(shiny::fluidRow, args = !!ui_inputs)
      }) -> ui_inputs

      private$inputs = list(
        quos = ui_inputs,
        comp = x
      )

      self$register_input_events = function(input, output, session, local_data){
        lapply(x, function(comp){
          comp$observers(input, output, session, local_data, self)
        })
      }

      invisible()
    },
    rave_outputs = function(..., .tabsets = list(), .env = NULL){
      quos = rlang::quos(...)
      assertthat::assert_that(length(quos) > 0, msg = 'No output defined!')
      parsers = rave:::comp_parser()
      x = lapply(names(quos), function(nm){
        re = parsers$parse_quo(quos[[nm]])
        re$label = nm
        re
      })

      ids = sapply(x, function(comp){comp$outputId})
      names(x) = ids

      #### Generate UIs for output

      # 1. tabsets in .tabsets
      widths = .tabsets[['width']]
      .tabsets[['width']] = NULL

      if(length(.tabsets)){
        names = names(.tabsets)
        ntabs = length(names)
        if(length(widths) < ntabs){
          widths = rep(widths, ntabs)
        }

        lapply(seq_len(ntabs), function(ii){
          nm = names[[ii]]
          ids = .tabsets[[nm]]

          quo_panels = lapply(seq_along(ids), function(ii){
            comp = ids[ii]
            title = names(comp)
            rlang::quo({
              shiny::tabPanel(
                title = !!title,
                do.call(shiny::fluidRow, args = !!{lapply(comp[[1]], function(output_id){
                  comp = x[[output_id]]
                  width = comp[['width']]; width %?<-% 12L
                  expr = quote(shiny::column())
                  expr = c(as.list(expr), list(
                    shiny::h4(comp$label),
                    comp$expr,
                    width = width
                  ))
                  as.call(expr)
                })}
                )
              )
            })
          })

          quo_box = rlang::quo({
            do.call(shinydashboard::tabBox,
                    args = c(
                      list(title = !!nm, width = !!widths[[ii]]),
                      !!quo_panels
                    )
            )
          })

          quo_box
        }) ->
          tab_boxes
      }else{
        tab_boxes = NULL
      }

      left_ids = ids[!ids %in% unlist(.tabsets)]

      lapply(left_ids, function(nm){
        comp = x[[nm]]
        width = comp$width; width %?<-% 12L
        rlang::quo({
          shinydashboard::box(
            width = width,
            title = comp$label,
            collapsible = T,
            !!comp$expr
          )
        })
      }) ->
        single_boxes

      ui_comps = rlang::quo(do.call(shiny::fluidRow, args = c(!!tab_boxes, !!single_boxes)))

      #### Reactive functions
      private$outputs = list(
        quos = ui_comps,
        comp = x
      )
      self$register_output_events = function(input, output, session, local_data){
        lapply(x, function(comp){
          comp$observers(input, output, session, local_data, self)
        })
      }
    },
    rave_updates = function(..., .env = NULL){
      quos = rlang::quos(..., .named = T)
      private$update = quos

      update = function(input, session = NULL, init = FALSE){
        input = dropNulls(input)
        if(!init){
          # Not yet implemented
          return(invisible())
        }
        if(is.null(session)){
          session = getDefaultReactiveDomain() #private$session
        }
        for(varname in names(private$update)){
          tryCatch({
            comp = private$inputs$comp[[varname]]
            new_args = rlang::eval_tidy(private$update[[varname]], data = input, env = self$param_env)

            comp$updates(session = session, .args = new_args)
          },error = function(e){
            logger('Error in updating input ', varname, level = 'ERROR')
            s = capture.output(traceback(e))
            lapply(s, logger, level = 'ERROR')
          })

        }
      }
      self$input_update = update
      invisible()
    },
    rave_execute = function(..., .env = NULL){
      quos = rlang::quos_auto_name(rlang::quos(...))

      normal_quos = quos[!names(quos) %in% 'async']
      async_quo = quos[['async']]
      self$async_module = !is.null(async_quo)

      self$execute = function(async = FALSE){
        self$runtime_env$.is_async = async
        async_future = NULL

        if(async){
          if(self$async_module){
            async_env = new.env(parent = self$runtime_env)
            packages = str_match(search(), '^package:(.+)$')[,2]; packages = packages[!is.na(packages)]
            packages = unique(packages, private$module_env$packages)
            self$param_env$..rave_future_obj =
              future::future({
                rave::eval_dirty(async_quo, env = async_env)
                async_env
              }, packages = packages, evaluator = future::multiprocess,
              gc = T, workers = rave_options('max_worker'))
          }
        }else{
          if(length(normal_quos)){
            lapply(normal_quos, rave::eval_dirty, env = self$runtime_env)
          }
        }

        return(self$param_env[['..rave_future_obj']])
      }
    },
    cache = function(key, val, global = FALSE, replace = FALSE,
                     session = getDefaultReactiveDomain()){
      # .key = str_c(unlist(key, recursive = T, use.names = F), collapse = ', ')
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
      str_val = ''#safe_str_c(val, collapse = ', ')
      # if(str_length(str_val) > 10){
      #   str_val = str_sub(str_val, end = 10L)
      # }

      # logger('Caching', ifelse(global, ' (global)', ''), ' - [', .key,'] - ', str_val)
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
    generate_input_ui = function(sidebar_width = 3L){
      ns = self$ns
      env = environment()

      more_btns = list(
        vignette = tags$li(actionLink(self$ns('..vignette'), 'Show Module Description')),
        niml = tags$li(actionLink(self$ns('.gen_niml'), 'Generate NIML File for SUMA')),
        async = tags$li(actionLink(self$ns('..async_run'), 'Run Algorithm (Async)'))
      )

      # TODO Vignette

      # NIML
      niml_func = names(as.list(self$static_env))
      is_niml_func = vapply(niml_func, function(x){
        is.function(self$static_env[[x]]) && str_detect(x, 'niml_')
      }, FUN.VALUE = logical(1))
      if(length(is_niml_func) == 0 || sum(is_niml_func) == 0){
        more_btns[['niml']] = NULL
      }

      # Async
      if(!self$async_module){
        more_btns[['async']] = NULL
      }


      names(more_btns) = NULL
      shiny::column(
        sidebar_width,
        rlang::eval_tidy(private$inputs$quos, env = env),
        fluidRow(
          shinydashboard::box(
            title = 'More...',
            tags$ul(
              tagList(more_btns)
            ),
            width = 12,
            collapsible = T
          )
        )
      )
    },
    generate_output_ui = function(sidebar_width = 3L){
      ns = self$ns
      env = environment()
      shiny::column(
        12L - sidebar_width,
        rlang::eval_tidy(private$outputs$quos, env = env)
      )

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
      names(private$inputs$comp)
    },
    input_labels = function(){
      re = lapply(private$inputs$comp, function(x){x$args$label})
      names(re) = names(private$inputs$comp)
      return(re)
    },
    output_labels = function(){
      re = lapply(private$outputs$comp, function(x){x$label})
      names(re) = names(private$outputs$comp)
      return(re)
    },
    output_ids = function(){
      names(private$outputs$comp)
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
    try({
      value = eval(value)
    })
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


#' @export
async_var <- function(x, default = NULL){
  if(is.null(x)){
    return(default)
  }else{
    return(x)
  }
}







