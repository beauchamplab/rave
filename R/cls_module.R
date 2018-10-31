# Environment for ECoG data and modules
# As of rave-Ent, data_repository nolonger succeed from globalenv()
# Instead, its parent is now baseenv()
# All packages needed are imported via loadnamespace within modules
# This will help create a clean environment for modules.
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
  session_based = F
  if(missing(session_id) || !is.character(session_id)){
    session_id %?<-% '.TEMP'
  }
  if(!exists(session_id, envir = data_repository)){
    e = new.env(parent = do.call('loadNamespace', list('rave')))
    e$.clean = function(){}
    data_repository[[session_id]] = e
  }
  return(data_repository[[session_id]])
}
# getDefaultDataRepository <- function(
#   session = getDefaultReactiveDomain(),
#   session_id,
#   session_based = NULL
# ){
#   if(is.null(session_based)){
#     session_based = rave_options('session_based_datarepo')
#   }
#   if(missing(session_id)){
#     if(!session_based){
#       session_id = NULL
#     }else{
#       session_id = add_to_session(session)
#     }
#   }
#   if(!is.character(session_id)){
#     session_id = '.TEMP'
#   }
#   if(!exists(session_id, envir = data_repository)){
#     e = new.env(parent = do.call('loadNamespace', list('rave')))
#     e$.clean = function(){
#       if(is.null(session)){
#         return(invisible())
#       }
#       rm(list = ls(envir = e, all.names = T), envir = e, inherits = F)
#       data_repository$.sessions[[session_id]] = NULL
#     }
#     assign(session_id, e, envir = data_repository)
#
#
#     if(!is.null(session)){
#       new_l = list(session); names(new_l) = session_id
#       data_repository$.sessions = c(
#         data_repository$.sessions, new_l
#       )
#     }
#   }
#   return(get(session_id, envir = data_repository))
# }

#' @export
attachDefaultDataRepository <- function(unload = F){
  if(unload){
    try({detach(rave_data)}, silent = T)
  }else{
    rave_data = getDefaultDataRepository()
    attach(rave_data)
  }
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
    info = function(){
      cat('Module Name:', self$label_name, '\n')
      cat('Version:', self$version, '\n')
      cat('Script Path:', self$script_path, '\n')
      cat('Author(s):\n')
      for(a in self$author){
        cat(' -', a, '\n')
      }
    },
    print = function(){
      pryr::address(self)
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
      script_path = base::normalizePath(script_path)
      self$script_path = script_path

    },
    get_or_new_exec_env = function(session = getDefaultReactiveDomain(), ..., new = FALSE){
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

      if(new || is.null(private$exec_env[[session_id]])){
        private$exec_env[[session_id]] = ExecEnvir$new(session = session, parent_env = parent_env)
        private$exec_env[[session_id]]$register_module(self)
      }
      if(!session_id %in% names(private$cache)){
        private$cache_env[[session_id]] = new.env()
      }
      return(private$exec_env[[session_id]])
    },
    load_script = function(session = getDefaultReactiveDomain()){
      # load default script
      default_src = readLines(system.file('default_module.R', package = 'rave'))
      # read in script, get package info
      src = readLines(self$script_path)
      src = c(default_src, src)

      # get
      static_env = self$get_or_new_exec_env(session = session)$static_env
      parse_env = self$get_or_new_exec_env(session = session)$parse_env
      runtime_env = self$get_or_new_exec_env(session = session)$runtime_env
      clear_env(parse_env)


      parsed = parse(text = src)
      for(i in 1:length(parsed)){

        # Use eval_dirty
        # Do not use str_c, use as.character instead to avoid warnings
        tryCatch({
          eval_dirty(parsed[i], env = runtime_env)
        }, error = function(e){
          logger('[Ignored]: ', as.character(parsed[i]), level = 'INFO')
          logger(paste(e, sep = '\n'), level = 'WARNING')
        })

        # comp = lazyeval::as.lazy(str_c(parsed[i]), env = static_env)
        # tryCatch({
        #   lazyeval::lazy_eval(comp)
        #   # logger('[Parsed]: ', str_c(parsed[i]), level = 'DEBUG')
        # }, error = function(e){
        #   logger('[Ignored]: ', str_c(parsed[i]), level = 'INFO')
        #   logger(paste(e, sep = '\n'), level = 'WARNING')
        # })
      }

      # Move everything to statis env
      list2env(as.list(runtime_env, all.names = T), envir = static_env)
      clear_env(runtime_env)

      # re-direct function environment to runtime-env where rave_execute take place.
      # for(nm in ls(static_env, all.names = T)){
      #   if(is.function(static_env[[nm]])){
      #     environment(static_env[[nm]]) <- runtime_env
      #   }
      # }

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
        uiOutput(e$ns('.__rave_modal__.')),
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
rave_inputs <- function(..., .input_panels = list(), .env = globalenv()){
  quos = rlang::quos(...)
  parser = comp_parser()
  lapply(quos, function(quo){
    comp = parser$parse_quo(quo)
    value = eval(comp$initial_value, envir = .env)
    inputId = comp$inputId
    .env[[inputId]] = value

    return(list(inputId = inputId, value = value))
  }) ->
    re
  nms = lapply(re, function(x){x$inputId})
  vals = lapply(re, function(x){x$value})
  names(vals) = nms
  .env[['.tmp_init']] = vals
  invisible(vals)
}

#' @export
rave_outputs <- function(..., .output_tabsets = list()){
  # do nothing
  return(invisible())
}


#' @export
rave_updates <- function(..., .env = globalenv()){

  res = rlang::quos(...)
  nms = names(res)
  if(length(nms) == 0){
    return()
  }
  lapply(res[nms == ''], function(quo){
    rave::eval_dirty(quo, env = .env)
  })

  nms = nms[nms != '']

  parser = comp_parser()
  for(nm in names(nms)){
    val = rave::eval_dirty(res[[nm]], env = .env)
    try({
      re = val$value
      re %?<-% val$selected
      .env[[nm]] = re
    })
  }

  invisible(res)

}


#' @export
rave_execute <- function(..., auto = TRUE, .env = globalenv()){
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
  tryCatch({
    if(is.null(x)){
      re = default
    }else{
      re = x
    }
    re
  }, error = function(e){
    default
  }) ->
    re
  re
}


#' @export
export_report <- function(expr, inputId){

}





