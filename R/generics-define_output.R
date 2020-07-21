#' @title Define 'RAVE' Module Output
#' @name define_output
#' @param definition R expression of output, such as \code{plotOutput('out')}
#' @param title Title to show 
#' @param width integer from 1 to 12, similar to "width" in 
#' \code{\link[shiny]{column}}
#' @param order the order of output, smaller order will be displayed first
#' @param keyword keyword for the output ID
#' @param ... ignored or passed to other methods
#' @return In default or debug context, it returns HTML tags, but when 
#' 'RAVE' is running, the result will be parse list for internal use.
NULL

.define_output <- function(definition, title = '', width = 12L, order = Inf, 
                           keyword = 'outputId', ...){
  # When run normally or in shiny session
  stopifnot2(width %in% 1:12, msg = 'Width must be from 1 to 12')
  
  ctx = rave_context()
  if(ctx$module_id != ''){ns = shiny::NS(ctx$module_id)}else{ns = shiny::NS(NULL)}
  
  definition = substitute(definition)
  definition = dipsaus::match_calls(definition, quoted = TRUE, envir = parent.frame())
  
  if(!shiny_is_running()){
    catgl(deparse(definition[[1]]), ' is defined as an output with title ', sQuote(title))
  }
  
  definition = dipsaus::match_calls(
    definition, quoted = TRUE, envir = parent.frame(), 
    replace_args = structure(list(prepend_ns), names = keyword)
  )
  
  invisible(eval(definition, envir = list(ns = ns), enclos = parent.frame()))
}

#' @rdname define_output
#' @export
define_output <- rave_context_generics('define_output', .define_output)

#' @rdname define_output
#' @export
define_output.default <- .define_output

#' @export
define_output.rave_module_debug <- function(
  definition, title = '', width = 12L, order = Inf, keyword = 'outputId', ...
){
  ctx = rave_context()
  stopifnot2(width %in% 1:12, msg = 'Width must be from 1 to 12')
  
  parser = comp_parser()
  definition = substitute(definition)
  
  mount_demo_subject()
  
  comp = parser$parse_quo(rlang::quo(!!definition))
  
  f = eval(definition[[1]])
  env_name = environmentName(environment(f))
  if(env_name == ''){env_name = '<No Name>'}
  
  catgl('Title - \t\t', level = 'INFO', end = '')
  catgl(title, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  catgl('Definition - \t\t', level = 'INFO', end = '')
  catgl(paste(deparse(comp$expr), collapse = '\n  '), level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  catgl('Package/Environment - \t', level = 'INFO', end = '')
  catgl(env_name, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  catgl('Width - \t\t', level = 'INFO', end = '')
  catgl(sprintf('%d (%.1f%% of output panel width)', width, width/12*100), level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  catgl('Order - \t\t', level = 'INFO', end = '')
  catgl(order, level = 'INFO', pal = list('INFO' = 'dodgerblue3'))
  
  # try to locate function
  
  output_id = comp[[keyword]]
  
  pname = ctx$package
  penv  = asNamespace(pname)
  f = get0(output_id, envir = penv, ifnotfound = NULL, inherits = FALSE)
  
  
  if(is.function(f)){
    if(length(formals(f))){
      catgl('Output function `', output_id, '` found in package ', pname, '.', level = 'INFO', sep = '')
    }else{
      catgl('Output function `', output_id, '` MUST take in at least one argument(s)!', level = 'ERROR', sep = '')
    }
  }else{
    fn_found = FALSE
    if(stringr::str_detect(deparse(definition[[1]]), '(customizedUI)|(uiOutput)|(htmlOutput)')){
      f = get0(output_id, envir = globalenv(), ifnotfound = NULL, inherits = FALSE)
      if(is.function(f) && length(formals(f))){
        catgl('Output function `', output_id, '` found in global environment. (Shiny-RAVE Customized UI)', level = 'INFO', sep = '')
        fn_found = TRUE
      }
    }
    if(!fn_found){
      catgl('Cannot find output function `', output_id, '` in package ', pname, '!', level = 'ERROR', sep = '')
    }
  }
  
}


#' @export
define_output.rave_running_local <- function(
  definition, title = '', width = 12L, order = Inf, keyword = 'outputId', ...
){
  rave_context()
  definition = substitute(definition)
  
  pkg_env = get('...pkg_env', envir = parent.frame(), inherits = TRUE)
  output_env = get('...output_env', envir = parent.frame(), inherits = TRUE)
  
  definition = match.call(definition = eval(definition[[1]], envir = pkg_env), definition)
  outputId = definition[[keyword]]
  has_output_id = !is.null(outputId)
  outputId %?<-% definition[['inputId']]
  mod_id = outputId
  # try to get function `outputId` from the package
  has_function = exists(outputId, envir = pkg_env, inherits = FALSE) && is.function(pkg_env[[outputId]])
  if(has_function){
    mod_id = paste0('..', outputId)
  }
  
  definition[[ifelse(has_output_id, keyword, 'inputId')]] = mod_id
  
  # output width
  
  width %?<-% 12
  stopifnot2(width %in% 1:12, msg = 'Output width Must be integer from 1 to 12.')
  definition[['width']] = width
  
  re = list(
    outputId = outputId,
    title = title,
    definition = definition,
    order = order
  )
  class(re) = c('comp_output', 'list')
  
  
  output_env[[outputId]] = re
  invisible(re)
}

#' @export
define_output.rave_running <- .define_output



