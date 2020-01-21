#' @title Defines 'RAVE' Module Initialization
#' Defines the global variables for the module. Called along with 
#' \code{\link[rave]{define_input}} to define UI initialization
#' actions once a subject is loaded.
#' @name define_initialization
#' @param expr R expression to run after subject is loaded
#' @return None
#' @examples 
#' 
#' \dontrun{
#' # Requires to install R package beauchamplab/ravebuiltins
#' 
#' # Enable debug mode
#' ravebuiltins::dev_ravebuiltins(reload = FALSE)
#' # Check data
#' define_initialization({
#'   rave_checks('power')
#'   power <- module_tools$get_power(referenced = TRUE)
#' })
#' 
#' 
#' # Initialize global variables for modules
#' ravebuiltins::dev_ravebuiltins(reload = FALSE)
#' define_initialization({
#'   print(subject$info())
#'   time_points = preload_info$time_points
#' })
#' define_input(
#'   shiny::sliderInput('time_range', 'Time-Range', min=0, 
#'                      max=1, value = c(0,1)),
#'   init_args = c('min', 'max', 'value'),
#'   init_expr = {
#'     min = min(time_points)
#'     max = max(time_points)
#'     value = c(0, max)
#'   }
#' )
#' }
NULL

.define_initialization <- function(expr){
  init_env = get('...init_env', envir = parent.frame(), inherits = TRUE)
  if(isFALSE(init_env[['init']])){
    init_env[['init']] = list()
  }
  expr = substitute(expr)
  init_env[['init']][[length(init_env[['init']]) + 1]] = expr
}

#' @rdname define_initialization
#' @export
define_initialization <- rave_context_generics('define_initialization', .define_initialization)

#' @rdname define_initialization
#' @export
define_initialization.default <- function(expr){
  stop('Function ', sQuote('define_initialization'), ' is not designed to run ',
       'in normal context. Please enable debugging mode to run locally')
}

#' @rdname define_initialization
#' @export
`define_initialization.rave_module_debug` <- function(expr){
  rave_context('rave_module_debug')
  mount_demo_subject()
  expr = substitute(expr)
  eval(expr, envir = parent.frame())
}

#' @rdname define_initialization
#' @export
`define_initialization.rave_running` <- .define_initialization

#' @rdname define_initialization
#' @export
`define_initialization.rave_running_local` <- .define_initialization

