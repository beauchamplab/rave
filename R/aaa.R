#' @import shiny
#' @importFrom dipsaus %?<-%
#' @importFrom dipsaus collapse
NULL



tags = htmltools::tags
div = htmltools::div

### For dev use only:
gl <- function(..., .envir = parent.frame()){
  glue::glue(..., .envir = .envir)
}

catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal){
  if(missing(.pal)){
    dipsaus::cat2(gl(..., .envir = .envir), level = level)
  }else{
    dipsaus::cat2(gl(..., .envir = .envir), level = level, pal = .pal)
  }
}

cat2 <- dipsaus::cat2

soft_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'WARNING')
}

hard_deprecated <- function(){
  env = parent.frame()
  call = do.call(match.call, envir = env, args = list())
  catgl('Function {call[[1]]} is soft-Deprecated. Details: \n{deparse(call)}', level = 'FATAL')
}

do_nothing <- function(...){}

### Stores internal settings (session-based)
..setup_env <- new.env(parent = baseenv())
..setup_env$..setup_env = ..setup_env
..setup_env$finalizers = list()

get_conf <- function(key, default = NULL){
  soft_deprecated()
  if(exists(key, envir = ..setup_env)){
    return(..setup_env[[key]])
  }else{
    default
  }
}

set_conf <- function(key, val, remove_if_null = TRUE){
  soft_deprecated()
  if(remove_if_null && (missing(val) || is.null(val))){
    rm(list = key, envir = ..setup_env, inherits = FALSE)
  }else{
    ..setup_env[[key]] = val
  }
}


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}


`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}


# ---------------- Deprecated functions ------------------

py_console <- function(...){
  hard_deprecated()
}

register_compoundInput <- function(...){
  soft_deprecated()
}



rave_context_list <- list(
  default = 'default',                        # Normal when running rave locally   no package, no module
  debug = 'rave_module_debug',                # When debugging rave                has package
  running = 'rave_running',                   # RAVE is running in shiny           has package and module
  run_local = 'rave_running_local'            # RAVE is running locally            has package and module
)
rave_context_c = unlist(rave_context_list)

rave_debug <- function(local=TRUE){
  genv = globalenv()
  genv$.__rave_context__. = 'rave_module_debug'
  genv$.__rave_package__. = 'ravebuiltins'
  genv$.__rave_module__. = 'power_explorer'
  if(!local){
    
    m = to_module('power_explorer')
    genv$.__rave_context__. = 'rave_running'
    genv$.__rave_module_instance__. = m$get_or_new_exec_env()
  }
  
  
  genv$module_id = 'power_explorer'
  genv$package = 'ravebuiltins'
}

get_running_instance <- function(senv, test = TRUE){
  has = exists('.__rave_module_instance__.', envir = senv, inherits = TRUE)
  if(test){
    return(has)
  }
  get0('.__rave_module_instance__.', envir = senv, inherits = TRUE, ifnotfound = NULL)
}

#' 'RAVE' Context: Read and Set Context of Environments
#' @param context context string for target environment, optional, see `Details'
#' @param require_contexts characters, (optional): required context for current 
#' function. If any context is missing, the function will raise errors
#' @param disallowed_context characters, (optional): defines the contexts that 
#' don't work for the function. If running within such contexts, the function 
#' will raise errors
#' @param error_msg characters, (optional): if running in improper contexts, the
#' message to display, will passed to \code{\link{stop}}
#' @param senv environment to read 'RAVE' contexts
#' @param spos levels to go up to search for \code{senv}, passed to 
#' \code{\link{parent.frame}}
#' @param tenv environment to set 'RAVE' contexts
#' @param tpos levels to go up to search for \code{tenv}, passed to 
#' \code{\link{parent.frame}}
#' @return A list of current context, including the package name, module ID, 
#' and current \code{ExecEnvir} instance if running under \code{"rave_running"} 
#' context.
#' @details Context strings tells the function which context it's running, 
#' and it will affect the behaviors of functions within its environment. 
#' Because 'RAVE' modules are usually R packages, the context strings help 
#' the module writters determine where the function is running. For example, 
#' running locally, or in 'RAVE' container, or debug mode. A typical example 
#' would be \code{\link[rave]{get_path}} function. All external scripts used 
#' in R packages require to be obtained using \code{\link[base]{system.file}}. 
#' However, because the files are subject to change, using system file function 
#' requires re-compile the package, which is time-consuming. Function 
#' \code{\link[rave]{get_path}} returns the file path relative to current 
#' working directory during the development (in "default" context), and it 
#' calls \code{\link[base]{system.file}} when 'RAVE' instance is running.
#' 
#' There are four contexts: \code{"default"}, \code{"rave_module_debug"}, 
#' \code{"rave_running"}, and \code{"rave_running_local"}. 
#' \describe{
#' \item{\code{default}}{Default context: this means the function is running 
#' without any additional information.}
#' \item{\code{rave_module_debug}}{Debug mode: used to develop and debug 
#' modules locally. Under the context, the function will be aware of the 
#' package that module belongs to}
#' \item{\code{rave_running}}{If the function is running under this context, 
#' this means it's running inside of shiny application (usually within 
#' \code{\link[rave]{start_rave}}). The function will be able to get more 
#' contexts such as module ID, and current runtime environment 
#' (\code{\link[rave]{ExecEnvir}})}
#' \item{\code{rave_running_local}}{Similar to \code{"rave_running"}, but 
#' without run-time environment. Under this context, the module is 
#' running locally without shiny. All reactive observers are disabled, and 
#' the modules will be compiled into a function with all the inputs defined 
#' by \code{\link[rave]{define_input}} as arguments, and code within 
#' \code{"main.R"} as the main body of the function.}
#' }
#' 
#' Function \code{rave_context} uses reserved variables in the environment:
#' \code{.__rave_context__.}, \code{.__rave_package__.}, 
#' \code{.__rave_module__.}, and \code{.__rave_module_instance__.}. Please 
#' don't use these variables for other purposes. See `Examples' for how to 
#' set and read the context.
#' 
#' @examples 
#' # ------- 1. Read/Set Context ---------
#' 
#' library(dipsaus)
#' library(rave)
#' # Reset context for current environment
#' rave_context('default')
#' 
#' # Read from current caller's environment
#' fun <- function(...){
#'   ctx <- rave_context()
#'   cat2('The function is running under context - ', ctx$context)
#'   cat2('The package under the context - ', ctx$package)
#'   cat2('Module ID is - ', ctx$module_id)
#'   cat2('Running instance is - ', ctx$instance)
#' }
#' fun()
#' ## The function is running under context - default
#' ## The package under the context - 
#' ## ...
#' 
#' # Set debug context 
#' debug_env <- new.env()
#' rave_context('rave_module_debug', tenv = debug_env)
#' debug_env$.__rave_package__. <- 'ravebuiltins'
#' 
#' # With debug_env, the function is aware of the package it's under
#' with(debug_env, { fun() })
#' ## The function is running under context - rave_module_debug
#' ## The package under the context - ravebuiltins
#' ## ...
#' 
#' # To set context within the function and affect the functions inide
#' fun2 <- function(module_id){
#'   # Run rave_context and then set module ID
#'   rave_context('rave_running_local')
#'   .__rave_module__. <- module_id
#'   fun()
#' }
#' with(debug_env, { fun2('power_explorer') })
#' ## The function is running under context - rave_running_local
#' ## The package under the context - ravebuiltins
#' ## Module ID is - power_explorer
#' ## ...
#' 
#' # Let's see what we can do with rave_module_debug
#' with(debug_env, { get_path('inst/rave.yaml') })
#' # When I develop the package, it returns:
#' ## "/Users/beauchamplab/.../ravebuiltins/inst/settings.yaml"
#' # When I run in other places, it returns
#' ## "/Users/beauchamplab/Library/R/3.6/library/ravebuiltins/rave.yaml"
#' 
#' 
#' # ------- 2. Setting behaviors for context ---------
#' # One way to set different behaviors is to using `ctx`
#' \dontrun{
#' fun <- function(){
#'   ctx <- rave_context()
#'   switch(ctx$context, ...)
#' }
#' }
#' 
#' # The other way is to use S3 generics provided by R syntax
#' fun <- rave_context_generics('fun', function(module_id, ...){})
#' 
#' # action for default
#' fun.default <- function(...){
#'   cat2('Function is not supposed to run under default context...',
#'        level = 'ERROR')
#' }
#' 
#' # for debug, set module ID and run with rave_running_local
#' fun.rave_module_debug <- function(module_id, ...){
#'   cat2('Debug mode... loading a test subject')
#'   # Do something ... like automatically mount_demo_subject
#'   # by running mount_demo_subject()
#'   
#'   rave_context('rave_running_local')
#'   .__rave_module__. <- module_id
#'   # Recall the function under rave_running_local context
#'   fun(module_id, ...)
#' }
#' 
#' # When running within RAVE container, local and with shiny
#' fun.rave_running_local <- function(...){
#'   ctx <- rave_context()
#'   cat2('Yay, running ', ctx$module_id, ' under context ',
#'        ctx$context, level='INFO')
#' }
#' fun.rave_running <- fun.rave_running_local
#' 
#' # Run in default mode, expect error message
#' fun('power_explorer')
#' 
#' # Run in debug mode
#' debug_env <- new.env()
#' rave_context('rave_module_debug', tenv = debug_env)
#' debug_env$.__rave_package__. <- 'ravebuiltins'
#' 
#' # The function will run in debug mode, then rave_running_local
#' with(debug_env, { fun('power_explorer') })
#' 
#' 
#' 
#' @export
rave_context <- function(context, require_contexts, disallowed_context, 
                        error_msg, senv = parent.frame(2L), spos,
                        tenv = parent.frame(), tpos){
  if(!missing(spos)){
    senv = parent.frame(spos)
  }
  if(!missing(tpos)){
    tenv = parent.frame(tpos)
  }
  if(missing(context)){
    context = get0('.__rave_context__.', envir = senv, ifnotfound = 'default', inherits = TRUE)
  }
  
  if(!missing(require_contexts)){
    if(!all(require_contexts %in% context)){
      call = paste(deparse(sys.call(2L)[[1]]), collapse = '')
      if(!missing(error_msg)){
        do.call(rave_failure, list(
          message = paste0('Context error: in ', sQuote(call), ': ', error_msg),
          level = 'ERROR',
          .stop = TRUE
        ), envir = parent.frame())
        # stop('Context error: in ', sQuote(call), ': ', error_msg)
      }
      
      do.call(rave_failure, list(
        message = paste0(
          'Context error: in ', sQuote(call), ': required context: ', paste(require_contexts, ', ')
        ),
        level = 'ERROR',
        .stop = TRUE
      ), envir = parent.frame())
      # stop('Context error: in ', sQuote(call), ': required context: ', paste(require_contexts, ', '))
    }
  }
  
  if(!missing(disallowed_context)){
    sel = context %in% disallowed_context
    if(any(sel)){
      call = paste(deparse(sys.call(2L)[[1]]), collapse = '')
      if(!missing(error_msg)){
        do.call(rave_failure, list(
          message = paste0('Context error: in ', sQuote(call), ': ', error_msg),
          level = 'ERROR',
          .stop = TRUE
        ), envir = parent.frame())
        # stop('Context error: in ', sQuote(call), ': ', error_msg)
      }
      do.call(rave_failure, list(
        message = paste0(
          'Context error: in ', sQuote(call), ': disallow context: ', paste(context[sel], ', ')
        ),
        level = 'ERROR',
        .stop = TRUE
      ), envir = parent.frame())
      # stop('Context error: in ', sQuote(call), ': disallow context: ', paste(context[sel], ', '))
    }
  }
  
  if(!all(context %in% rave_context_c)){
    cat2("Context doesn't exists: ", paste(context[!context %in% rave_context_c], collapse = ', '), level = 'FATAL')
  }
  
  package = get0('.__rave_package__.', envir = senv, inherits = TRUE, ifnotfound = '')
  moduleid = get0('.__rave_module__.', envir = senv, inherits = TRUE, ifnotfound = '')
  instance = NULL
  
  context = context[context %in% rave_context_c]
  if(!length(context)){
    context = 'default'
  }
  tenv$.__rave_context__. = context
  
  if('rave_running' %in% context){
    tenv$.__rave_package__. = package
    tenv$.__rave_module__. = moduleid
    if(!get_running_instance(senv)){
      cat2('RAVE is running but no instance is found', level = 'INFO')
    }
    instance = get_running_instance(senv, test = FALSE)
    tenv$.__rave_module_instance__. = instance
  }else if('rave_running_local' %in% context){
    tenv$.__rave_package__. = package
    tenv$.__rave_module__. = moduleid
  }else if('rave_module_debug' %in% context){
    tenv$.__rave_package__. = package
  }
  invisible(list(
    context = context,
    package = package,
    module_id = moduleid,
    instance = instance
  ))
}


#' @title Create S3 Generics that Respects 'RAVE' Context
#' @param fun_name generic function name
#' @param fun function that set the arguments of the generic
#' @return A generic function
#' @export
rave_context_generics <- function(fun_name, fun = function(){}){
  
  stopifnot2(is.character(fun_name), msg = 'fun_name must be characters')
  
  body(fun) <- rlang::quo_squash(rlang::quo({
    .__rave_temp__. = rave_context()
    # cat2(!!fun_name, ' - ', paste(unlist(.__rave_temp__.), collapse = ','))
    UseMethod(!!fun_name, structure(list(), class = .__rave_temp__.$context))
  }))
  
  fun
  
}


shiny_is_running <- function(){
  !is.null(shiny::getDefaultReactiveDomain())
}

