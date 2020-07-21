#' @title Defines 'RAVE' Module Inputs
#' @name define_input
#' @param definition R expression to define UI elements without \code{ns()}, 
#' for example, \code{textInput('varname', 'Label', ...)}
#' @param init_args arguments to change once a subject is loaded
#' @param init_expr expression to evaluate with subject loaded
#' @param keyword what identifies the input element
#' @param update_level update action code: see details.
#' @param ... ignored or passed to other methods.
#' 
#' @details 
#' 
#' This function behaves differently in different contexts. By default, 
#' it returns the result of \code{definition}. When debugging modules (
#' \code{"rave_module_debug"}), it assigns a variable to the global environment 
#' with the variable name defined as input ID. In other contexts it parse
#' the definition and returns a list for 'RAVE' to use internally to compile
#' the module.
#' 
#' If \code{update_level} is `0` then the input is defined as  manual inputs, 
#' which will not trigger re-calculate if changed. If `1` is set, then the 
#' input is a render's input, and only update render functions. If `2` is 
#' used, then once user change an input, then the whole module is re-calculated.
#' 
#' \code{init_args} must be argument names of the definition. Once subject is 
#' loaded, \code{init_expr} will be evaluated in a local environment, then 
#' variables in \code{init_args} will be used to update the input widgets.
#' 
#' @return See details
NULL

.define_input <- function(definition, init_args, init_expr, keyword = 'inputId',
                          update_level = 2, ...){
  # When run normally in shiny session
  
  ctx = rave_context()
  if(ctx$module_id != ''){ns = shiny::NS(ctx$module_id)}else{ns = shiny::NS(NULL)}
  
  definition = substitute(definition)
  definition = dipsaus::match_calls(definition, quoted = TRUE, envir = parent.frame())
  
  
  definition = dipsaus::match_calls(
    definition, quoted = TRUE, envir = parent.frame(), 
    replace_args = structure(list(prepend_ns), names = keyword)
  )
  invisible(eval(definition, envir = list(ns = ns), enclos = parent.frame()))
}

#' @rdname define_input
#' @export
define_input <- rave_context_generics('define_input', .define_input)

#' @rdname define_input
#' @export
define_input.default <- .define_input

#' @rdname define_input
#' @export
define_input.rave_module_debug <- function(definition, init_args, init_expr, ...){
  rave_context()
  
  definition = substitute(definition)
  init_expr = substitute(init_expr)
  
  mount_demo_subject()
  
  parser = comp_parser()
  
  comp = parser$parse_quo(rlang::quo(!!definition))
  
  def_text = deparse(comp$expr)
  def_text = paste(def_text, collapse = '\n  ')
  input_id = comp$inputId
  
  f = eval(definition[[1]])
  env_name = environmentName(environment(f))
  if(env_name == ''){env_name = '<No Name>'}
  
  catgl('Input Definition - ', level = 'INFO')
  catgl(' ', def_text, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  catgl('Package/Environment - \t', level = 'INFO', end = '')
  catgl(env_name, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  val = comp$initial_value
  
  # Update info
  if(!missing(init_args)){
    catgl('Updating Input Parameter(s) - ', level = 'INFO')
    
    env = new.env(parent = parent.frame())
    eval(init_expr, envir = env)
    for(arg in init_args){
      v = env[[arg]]
      v = paste(deparse(v), collapse = '\n  ')
      
      catgl(' ', arg, '- ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
      catgl(v, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
    }
    
    if('value' %in% init_args){
      val = env[['value']]
    }else if('selected' %in% init_args){
      val = env[['selected']]
    }
    
  }
  
  v = paste(deparse(val), collapse = '\n  ')
  
  catgl('Input Value - \t', level = 'INFO', end = '')
  catgl(input_id, '= ', level = 'INFO', pal = list('INFO' = 'orangered'), end = '')
  catgl(v, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  assign(input_id, val, envir = parent.frame())
  invisible(val)
}

#' @rdname define_input
#' @export
define_input.rave_running_local <- function(
  definition, init_args, init_expr, keyword = 'inputId',
  update_level = 2, ...
){
  rave_context()
  # Special environment, this is inside of compiler
  input_env = get('...input_env', envir = parent.frame(), inherits = TRUE)
  shared_env = get('...shared_env', envir = parent.frame(), inherits = TRUE)
  
  definition = substitute(definition)
  definition = dipsaus::match_calls(definition, quoted = TRUE, envir = parent.frame())
  inputId = eval(definition[[keyword]])
  re = list(
    inputId = inputId,
    definition = definition
  )
  class(re) = c('comp_input', 'list')
  if(missing(init_args) || missing(init_expr)){
    input_env[[inputId]] = re
    return(invisible(re))
  }
  init_expr = substitute(init_expr)
  initialization = rlang::quo(local({
    force(!!init_expr)
    sapply(!!init_args, get, envir = environment(), 
           simplify = FALSE, USE.NAMES = TRUE)
  }))
  re[['initialization']] = initialization
  
  input_env[[inputId]] = re
  
  
  if( update_level == 1 ){
    # Render output only
    shared_env$render_inputs = c(shared_env$render_inputs, inputId)
  }else if(update_level == 0){
    shared_env$manual_inputs = c(shared_env$manual_inputs, inputId)
  }
  
  invisible(re)
}

#' @rdname define_input
#' @export
define_input.rave_running <- .define_input






