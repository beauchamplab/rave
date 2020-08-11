# Documented 2019-11-21


# Function to bind functions to exec_env's wrappers
# e is the execenv and w is its wrapper
bind_wrapper_env <- function(self, w, shiny_mode = TRUE){
  w$async_var = function(x, default = NULL){
    x_name = deparse(substitute(x))
    val = NULL
    future_env = self$param_env[['..rave_future_env']]
    if(is.environment(future_env) || is.list(future_env)){
      val = future_env[[x_name]]
      
      if(!is.null(val)){
        return(val)
      }
    }
    return(default)
  }
  
  w$reloadUI = function(){
    self$reload()
  }
  
  w$launch_selector = function(){
    self$global_reactives$launch_selector = Sys.time()
  }
  
  w$monitor_subject_change = function(){
    
    if(shiny::is.reactivevalues(self$local_reactives)){
      if( self$local_reactives$initialized && self$local_reactives$has_data && self$local_reactives$focused ){
        # base::print('subject changed?')
        return(TRUE)
      }
      return(FALSE)
    }else{
      return(FALSE)
    }
  }
  w$get_execenv_local_reactive = function(){
    self$local_reactives
  }
  
  
  w$get_client_size = function(){
    if(is.reactivevalues(self$global_reactives)){
      return(shiny::isolate(self$global_reactives$client_size))
    }
    return(NULL)
  }
  
  w$switch_to = function(module_id, varriable_name = NULL, value = NULL, quiet = F, ...){
    if(is.reactivevalues(self$global_reactives)){
      # if missing module_id, jump to last activated module
      # This is a hidden feature if not specifying module_id
      # 1. in the dev mode, I'll raise error if module_id is not string
      # 2. Be careful when using this hidden feature since it might cause infinite loop
      if(missing(module_id)){
        module_id = NULL
        hist = isolate(self$global_reactives$view_history)
        if(length(hist) > 1){
          ind = which(vapply(hist, '[[', logical(1L), 'activated'))
          if(length(ind)){
            ind = ind[length(ind)]
            module_id = hist[[ind]]$module_id
          }
        }
      }
      if(length(module_id)){
        self$global_reactives$switch_module = c(
          list(
            module_id = module_id,
            varriable_name = varriable_name,
            value = value,
            timestamp = Sys.time()
          ),
          list(...)
        )
      }else{
        # showNotification(p('Cannot switch back. You have not opened any modules yet.'), type = 'warning')
        w$launch_selector()
      }
    }
  }
  
  w$reload_module = function(){
    clear_cache(levels = 1)
    self$input_update(list(), init = TRUE)
  }
  w$get_input_ids = function(render_inputs = FALSE, manual_inputs = FALSE){
    input_ids = self$input_ids
    
    if( !render_inputs ){
      render_ids = self$rendering_inputIds
      input_ids = input_ids[!input_ids %in% render_ids]
    }
    if( !manual_inputs ){
      manual_ids = self$manual_inputIds
      input_ids = input_ids[!input_ids %in% manual_ids]
    }
    input_ids
  }
  
  persist_widget = local({
    auto = TRUE
    temporary_on = FALSE
    is_auto = function( on, include_temporary = FALSE, cancel_temporary = FALSE ){
      if(!missing(on)){
        temporary_on <<- FALSE
        auto <<- !isFALSE(on)
        catgl('Auto Re-calculate is set to ', auto)
      }
      if( include_temporary ){
        re = auto || temporary_on
      }else{
        re = auto
      }
      if(cancel_temporary){
        temporary_on <<- FALSE
      }
      
      re
    }
    
    trigger = function(force = TRUE){
      temporary_on <<- isTRUE(force)
      if(shiny::is.reactivevalues(self$local_reactives)){
        self$local_reactives$last_input = Sys.time()
      }
    }
    
    list(
      is_auto = is_auto,
      trigger = trigger
    )
    
  })
  w$auto_recalculate = persist_widget$is_auto
  w$trigger_recalculate = persist_widget$trigger
  

  w$current_module = function(){
    if(is.reactivevalues(self$global_reactives)){
      return(isolate(get_val(self$global_reactives, 'execute_module', default = '')))
    }
    return('')
  }
  
  w$get_brain = function(surfaces = 'pial', multiple_subject = FALSE,
                         data_repo = rave::getDefaultDataRepository()){
    subject = get0('subject', envir = data_repo, ifnotfound = NULL)
    brain = NULL
    if( !is.null(subject) ){
      brain = rave_brain2(subject = subject, surfaces = surfaces, compute_template = FALSE)
      if( multiple_subject ){
        brain = threeBrain::merge_brain(brain)
      }
    }
    brain
  }
  
  w$require = function(package, ..., character.only = TRUE){
    p = as.character(substitute(package))
    if(!dipsaus::package_installed(p)){
      try({
        catgl("Installing Package ", p, level = 'WARNING')
        utils::install.packages(p, type = 'binary')
      })
    }
    do.call('require', args = c(list(
      package = p,
      character.only = TRUE
    ),
    list(...)))
  }
  w$library = w$require
  
  w$observe = function(x, env = NULL, quoted = FALSE, priority = 0, domain = NULL, ...){
    if(!quoted){
      x = substitute(x)
    }
    
    # Make sure shiny doesn't crash
    x = rlang::quo_squash(rlang::quo(
      safe_wrap_expr(!!x)
    ))
    
    
    if(!is.environment(env)){
      env = self$runtime_env
    }
    if(is.null(domain)){
      domain = self$wrapper_env$getDefaultReactiveDomain()
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
  
  w$safe_wrap_expr = safe_wrap_expr
  
  w$observeEvent = function(
    eventExpr, handlerExpr, event.env = NULL,
    event.quoted = FALSE, handler.env = NULL, handler.quoted = FALSE,
    priority = 0, domain = NULL, ...
  ){
    if(!event.quoted){
      eventExpr = substitute(eventExpr)
    }
    if(!is.environment(event.env)){
      event.env = self$runtime_env
    }
    
    if(!handler.quoted){
      handlerExpr = substitute(handlerExpr)
    }
    if(!is.environment(handler.env)){
      handler.env = self$runtime_env
    }
    if(is.null(domain)){
      domain = self$wrapper_env$getDefaultReactiveDomain()
    }
    
    # Make sure shiny doesn't crash
    handlerExpr = rlang::quo_squash(rlang::quo(
      safe_wrap_expr(!!handlerExpr)
    ))
    
    shiny::observeEvent(
      eventExpr = eventExpr, handlerExpr = handlerExpr, event.env = event.env,
      event.quoted = TRUE, handler.env = handler.env, handler.quoted = TRUE,
      priority = priority - 1L, domain = domain, ...
    )
  }
  
  if(shiny_mode){
    w$rave_inputs = self$rave_inputs
    w$rave_outputs = self$rave_outputs
    w$rave_updates = self$rave_updates
    w$rave_execute = self$rave_execute
    w$rave_ignore = do_nothing
  }else{
    w$rave_inputs = rave_inputs
    w$rave_outputs = rave_outputs
    w$rave_updates = rave_updates
    w$rave_execute = rave_execute
    w$rave_ignore = rave_ignore
  }
  
  w$export_report = self$export_report
  w$rave_prepare = do_nothing
  w$ns = function(id){
    # ns will be changed during shinirize process
    self$ns(id)
  }
  
}


#' @title R6 'RAVE' Module Class
#' @description contains module data, functions, etc.
#' 
#' @examples 
#' \dontrun{
#' 
#' module <- get_module('ravebuiltins', 'power_explorer')
#' module
#' #> Module Name: Power Explorer 
#' #> Version: 0 
#' #> Script Path: ... 
#' #> Author(s):
#' 
#' }
#' @export
ModuleEnvir <- R6::R6Class(
  classname = 'ModuleEnvir',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    exec_env = NULL
  ),
  public = list(
    #' @field cache_env cache environment for module
    cache_env = NULL,
    
    #' @field module_id module ID, unique
    module_id = '',
    
    #' @field label_name corresponding module name
    label_name = '',
    
    #' @field script_path compiled module scripts
    script_path = '',
    
    #' @field script if \code{script_path} not exists, alternative script
    script = '',
    
    #' @field author who wrote the module not often used
    author = NULL,
    
    #' @field version module version
    version = NULL,
    
    #' @field packages the packages to be loaded for the module
    packages = NULL,
    
    #' @field rmd_path deprecated
    rmd_path = NULL,
    
    #' @field parent_env parent environment of the module, usually global 
    #' environment or package environment
    parent_env = NULL,
    
    #' @field from_package whether the module is compiled from another R 
    #' package. This value is required to be true since \code{"rave-0.1.9"}.
    from_package = FALSE,
    
    #' @field package_name which package does the module belong to?
    package_name = '',
    
    #' @field sidebar_width input panel width, from 1 to 11
    sidebar_width = 3L,
    
    #' @description print module information
    #' @return none
    info = function(){
      cat('Module Name:', self$label_name, '\n')
      cat('Version:', self$version, '\n')
      cat('Script Path:', self$script_path, '\n')
      cat('Author(s):\n')
      for(a in self$author){
        cat(' -', a, '\n')
      }
    },
    
    #' @description print module information and returns memory address
    #' @param ... ignored
    print = function(...){
      self$info()
      env_address(self)
    },
    
    #' @description constructor
    #' @param module_id,label_name,script_path,author,version see fields
    #' @param packages,parent_env,rmd_path see fields
    #' @param .script_content internal use
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
      
      # Persist light version of module settings
      setting_file <- file.path('~/rave_modules/settings/', module_id)
      
      if(dir.exists(setting_file)){
        fs <- list.files(setting_file, all.files = TRUE, recursive = TRUE, full.names = TRUE)
        # only allow max of 1MB settings
        if(sum(file.size(fs)) > 4*1024^2){
          warning("Module settings `", setting_file, "` is larger than 4MB. Reset settings as it will impact performance")
          unlink(setting_file, recursive = TRUE, force = TRUE)
        }
      }
      self$cache_env = dipsaus::rds_map(path = setting_file)
      
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
      #    and load it with partial - loadNamespace(..., partial = TRUE)
      # 2. RAVE needs to be one of the parent envs (search path). If parent_env is created via
      #    loadNamespace(..., partial = TRUE), this is automatically true as the search path will be
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
        parent_env = new.env(parent = globalenv(), hash = TRUE)
      }
      self$parent_env = parent_env
      
      # validate script_path
      if(missing(script_path)){
        stopifnot2(!is.null(.script_content), msg = 'Script Path not specified')
        script_path = file.path(dirname(rmd_path), '.rave_tmp.R')
        writeLines(.script_content, script_path)
      }
      
      stopifnot2(file.exists(script_path), msg = sprintf('[File Not Found] %s', script_path))
      script_path = base::normalizePath(script_path)
      self$script_path = script_path
      
    },
    
    
    #' @description get the corresponding \code{\link[rave]{ExecEnvir}} with 
    #' shiny session
    #' @param session shiny session; see shiny \code{\link[shiny]{domains}}
    #' @param new whether to force creating a new runtime environment if 
    #' previous one already exists
    #' @param ... ignored
    #' @return an \code{\link[rave]{ExecEnvir}} instance associated with 
    #' current module and given session
    get_or_new_exec_env = function(session = getDefaultReactiveDomain(), 
                                   ..., new = FALSE){
      rave_context()
      session_id = add_to_session(session)
      if(is.null(session_id)){
        session_id = '.TEMP'
      }
      
      if(new || is.null(private$exec_env[[session_id]])){
        private$exec_env[[session_id]] = ExecEnvir$new(session = session, parent_env = self$parent_env)
        private$exec_env[[session_id]]$register_module(self)
      }
      return(private$exec_env[[session_id]])
    },
    
    #' @description load and compile script into registered 
    #' \code{\link[rave]{ExecEnvir}}
    #' @param session shiny session; see shiny \code{\link[shiny]{domains}}
    #' @return none
    load_script = function(session = getDefaultReactiveDomain()){
      
      rave_context()
      # load default script
      default_src = readLines(system.file('default_module.R', package = 'rave'))
      # read in script, get package info
      src = readLines(self$script_path)
      src = c(default_src, src)
      
      execenv = self$get_or_new_exec_env(session = session)
      
      on.exit({
        rm(execenv)
      }, add = TRUE)
      
      # get
      static_env = execenv$static_env
      parse_env = execenv$parse_env
      runtime_env = execenv$runtime_env
      clear_env(parse_env)
      
      
      parsed = parse(text = src)
      for(i in 1:length(parsed)){
        
        # Use eval_dirty
        # Do not use str_c, use as.character instead to avoid warnings
        tryCatch({
          dipsaus::eval_dirty(parsed[i], env = runtime_env)
        }, error = function(e){
          catgl('[Ignored]: ', as.character(parsed[i]), level = 'INFO')
          catgl(paste(e, sep = '\n'), level = 'WARNING')
        })
        
      }
      
      # Move everything to statis env
      list2env(as.list(runtime_env, all.names = TRUE), envir = static_env)
      clear_env(runtime_env)
      
      # re-direct function environment to runtime-env where rave_execute take place.
      # for(nm in ls(static_env, all.names = TRUE)){
      #   if(is.function(static_env[[nm]])){
      #     environment(static_env[[nm]]) <- runtime_env
      #   }
      # }
      
      # lockEnvironment(static_env)
      
    },
    
    
    #' @description generate 'HTML' tags
    #' @param session shiny session; see shiny \code{\link[shiny]{domains}}
    #' @return 'HTML' tags
    render_ui = function(session = getDefaultReactiveDomain()){
      e = self$get_or_new_exec_env(session = session)
      if(length(e$input_ids)){
        sidebar_width = self$sidebar_width
      }else{
        sidebar_width = 0
      }
      shiny::fluidRow(
        uiOutput(e$ns('.__rave_modal__.')),
        e$generate_input_ui(sidebar_width = sidebar_width),
        e$generate_output_ui(sidebar_width = sidebar_width)
      )
      
    },
    
    
    #' @description clean the module environment
    #' @param session shiny session; see shiny \code{\link[shiny]{domains}}
    #' @param session_id shiny 'RAVE' ID, default is auto-generated
    clean = function(session = getDefaultReactiveDomain(),
                     session_id){
      
      if(missing(session_id)){
        session_id = add_to_session(session)
      }
      if(is.character(session_id)){
        # Clear runtime_env
        if( inherits(private$exec_env[[session_id]], 'ExecEnvir') ){
          private$exec_env[[session_id]]$clean()
        }
        private$exec_env[[session_id]] = NULL
      }
    }
  )
)

