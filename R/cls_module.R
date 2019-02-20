# Environment for ECoG data and modules
# As of rave-Ent, data_repository nolonger succeed from globalenv()
# Instead, its parent is now baseenv()
# All packages needed are imported via loadnamespace within modules
# This will help create a clean environment for modules.
NULL

#' Wrapper for shiny::getDefaultReactiveDomain
getDefaultReactiveDomain <- function(){
  session = shiny::getDefaultReactiveDomain()
  session %?<-% get0('session', envir = globalenv())
  return(session)
}


data_repository = new.env(parent = baseenv())

#' Get environment where subject data is loaded
#' @param session shiny session, default is NULL
#' @param session_id internal use
#' @param session_based internal use
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

#' Attach subject data
#' @param unload TRUE if you want to detach
#' @export
attachDefaultDataRepository <- function(unload = T){
  if(unload){
    try({detach('rave_data')}, silent = T)
  }

  rave_data = getDefaultDataRepository()

  rave_idx = which(search() == "package:rave")

  if(length(rave_idx)){
    do.call('attach', list(rave_data, name = 'rave_data', pos = rave_idx))
  }else{
    do.call('attach', list(rave_data, name = 'rave_data'))
  }
}

#' Module class
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
    parent_env = NULL,
    from_package = FALSE,
    sidebar_width = 3L,
    info = function(){
      cat('Module Name:', self$label_name, '\n')
      cat('Version:', self$version, '\n')
      cat('Script Path:', self$script_path, '\n')
      cat('Author(s):\n')
      for(a in self$author){
        cat(' -', a, '\n')
      }
    },
    print = function(...){
      self$info()
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
      parent_env = globalenv()
    ){
      self$module_id = module_id
      self$label_name = label_name
      self$author = author
      self$version = version
      self$packages = c('rave', packages)
      self$rmd_path = rmd_path
      private$cache_env = list()

      # Note as of 11/11/2018:
      # The structure of module is parent_env -> wrapper -> static -> param -> runtime -> (parser)
      # Before today, all variables are stored in runtime environment. However if parent_env is
      # a package environment, there is no way for functions within that package to access those
      # variables within runtime environment.
      #
      # We can't set parent environment of package environment since `parent.env<-` is not recommended
      # However, we can redirect some variables or to the package and evaluate some functions in
      # different context
      #
      ### Solution: (here is mainly for package parent_env)
      #
      # Case 1:
      # If parent_env is globalenv(), we replace with a new one under globalenv as parent_env
      # In this case, we load module via scripts (not some packages). There is no scoping issue
      # as scripts are loaded from runtime environment and then migrated to static environment.
      # All functions are executed in runtime environment and thus this env is the root and all
      # its parent envs are accessible
      #
      # Case 2:
      # If parent_env is a some environment, we here need it to be either a package environment
      # or some unlocked environment.
      #
      # Either scenarios will need
      # 1. parent_env is unlocked.
      #    If parent_env is locked package environment. RAVE will try to unload this environment
      #    and load it with partial - loadNamespace(..., partial = T)
      # 2. RAVE needs to be one of the parent envs (search path). If parent_env is created via
      #    loadNamespace(..., partial = T), this is automatically true as the search path will be
      #    package << base << globalenv << ... << rave. For non-package environment, easiest case
      #    would be using new.env(parent = globalenv()). However, non-package environment is not
      #    recommended unless you know what I'm doing. Best practice would be using rave built-in
      #    function to create template R module and modify.
      #
      ### Scoping
      # For package based modules, scoping issue is a big problem as package loaded via loadNamespace
      # can't control their parents and therefore hard to access runtime env
      #
      # To solve this problem, I use the following method:
      #
      # 1. Active binding rave module toolbox to package environment (this needs the package env
      #    unlocked)
      # 2. Make environment inside of `__init__` visible within package environment. This requires
      #    a dirty-eval (evaluation with side-effects) within package environment for rave_updates (TODO)
      # 3. rave_execute will be evaluated within runtime env, and return a variable "result", which will
      #    be use used as inputs of output functions.
      #
      # structure will be
      #
      # + parent_env: package environment or  (top environment)
      # | <--- binded module toolbox
      # | <--- active bind other util functions (redirect to wrappers)
      # | <--- contains __init__ environment, or anonymous rave_updates variables
      # | <--- cannot access to all other runtime environment
      # |
      # |---+ wrapper (locked)
      #     | <--- provide util functions
      #     | <--- rewrite default functions
      #     |
      #     |---+ static (locked, not very useful for package-based modules)
      #         |
      #         |---+ param environment
      #             | <--- stores all the inputs parameters
      #             | <--- stores all anonymous rave_updates variables (will be removed)
      #             | <--- stores async futures
      #             |
      #             |---+ runtime environment
      #                 | <--- rave_updates runs here
      #
      # Modified on Nov 12, 2018
      # It turns out even if we active bind variables to package environment, functions within
      # package cannot access those active bindings. Reasons are simple, when packages are loaded
      # via loadNamespace, there seems to be a prototype environment created internally from inside
      # of R. After that, all "loadNamespace" will just copy from that internal environment and all
      # functions in the package are redirected to the locked environment and thus we can change
      # nothing to that environment.
      #
      # package env (prototype, may be locked)
      #    ^
      #    |
      # package namespaces via loadNamespace (reference env, with active binding)
      #
      # Package functions may not look at the namespace they stay in. Instead, they are referenced
      # to prototype environment sometimes.
      #
      # This is sad. Because solution 1 seems not working if the namespace is loaded in other places
      # and we cannot control users' action. Therefore, I change the implementation back and only make
      # __init__ and __main__ special
      #
      # __init__ is evaluated at param_env
      # __main__ is evaluated at runtime_env
      #
      # hence all variables in __init__ can be accessed by __main__
      #
      # These two functions can access all variables. However, other functions can only access
      # "result" returned by __main__
      #
      # This is sad, but very robust as we don't need strict assumptions
      #


      if(!is.environment(parent_env) || identical(parent_env, globalenv())){
        parent_env = new.env(parent = globalenv(), hash = T)
      }
      self$parent_env = parent_env

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

      if(new || is.null(private$exec_env[[session_id]])){
        private$exec_env[[session_id]] = ExecEnvir$new(session = session, parent_env = self$parent_env)
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
        e$generate_input_ui(sidebar_width = self$sidebar_width),
        e$generate_output_ui(sidebar_width = self$sidebar_width)
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



















#' Functions for dev use
#' @param ... Expressions
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

rave_outputs <- function(..., .output_tabsets = list()){
  # do nothing
  return(invisible())
}


rave_updates <- function(..., .env = globalenv()){

  res = rlang::quos(...)
  nms = names(res)
  if(length(nms) == 0){
    return()
  }
  lapply(res[nms == ''], function(quo){
    eval_dirty(quo, env = .env)
  })

  nms = nms[nms != '']

  # parser = comp_parser()
  for(nm in nms){
    val = eval_dirty(res[[nm]], env = .env)
    try({
      re = val$value
      re %?<-% val$selected
      .env[[nm]] = re
    })
  }

  invisible(res)

}


rave_execute <- function(..., auto = TRUE, .env = globalenv()){
  assign('.is_async', TRUE, envir = .env)
  dots <- lazyeval::lazy_dots(...)
  for(i in 1:length(dots)){
    dots[[i]]$env <- .env
    logger('> ', dots[[i]]$expr, level = 'INFO')
    lazyeval::lazy_eval(dots[[i]])
  }
}



#' Cache input values
#' @param inputId input ID
#' @param val value if not cached
#' @param read_only logical, if FALSE, replace cache
#' @export
cache_input <- function(inputId, val, read_only = T){
  return(val)
}


# Get x or default
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


export_report <- function(expr, inputId){

}





