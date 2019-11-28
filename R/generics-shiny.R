# define_ui <- rave_context_generics('define_ui', function(definition, keyword, global_scope, env){})
# 
# define_ui.rave_module_debug <- function(definition, keyword, global_scope, env){
#   ctx = rave_context('rave_module_debug')
#   
#   
#   definition = dipsaus::match_calls(definition, quoted = TRUE)
#   eval(definition, envir = list(ns = ns), enclos = env)
# }
# 
# define_ui <- function(definition, keyword, global_scope, env){
#   ns = NULL
#   if(global_scope){
#     execenv = getCurrentExecEnvir()
#     if(!is.null(execenv)){
#       ns = execenv$ns
#     }
#     if(!is.function(ns)){
#       stop('You cannot call ', sQuote('define_input'), ' in shiny context without RAVE module')
#     }
#   }else{
#     session = shiny::getDefaultReactiveDomain()
#     ns = session$ns
#   }
#   
#   definition = dipsaus::match_calls(
#     definition, quoted = TRUE, 
#     replace_args = structure(list(prepend_ns), names = keyword), envir = env)
#   eval(definition, envir = list(ns = ns), enclos = env)
# }
# 
# 
# 
# define_output2 <- rave_context_generics('define_output2', alist(
#   definition = , title = '', width = 12L, order = Inf, listen_to = , 
#   keyword = 'outputId', ... = 
# ))

# 
# 
# 
# 
# `define_output2.rave-session_proxy` <- function(
#   definition, title = '', width = 12L, order = Inf, 
#   listen_to, keyword = 'outputId', ...){
#   env = parent.frame()
#   definition = substitute(definition)
#   define_ui(definition, keyword, FALSE, env)
# }
# 
# `define_output2.rave-ShinySession` <- function(
#   definition, title = '', width = 12L, order = Inf, 
#   listen_to, keyword = 'outputId', ...){
#   env = parent.frame()
#   definition = substitute(definition)
#   define_ui(definition, keyword, TRUE, env)
# }
# 
# `define_output2.rave_compile` <- function(
#   definition, title = '', width = 12L, order = Inf, 
#   listen_to, keyword = 'outputId', ...){
#   stopifnot2(width %in% seq_len(12), msg = 'width must be integer from 1 to 12')
#   
#   definition = substitute(definition)
#   definition = dipsaus::match_calls(definition, quoted = TRUE, envir = parent.frame())
#   outputId = eval(definition[[keyword]], list(), enclos = baseenv())
#   fun_name = paste0('...___', outputId)
#   definition[[keyword]] = fun_name
#   definition[['width']] = width
#   
#   
#   # Unlike inputs, outputs might exists in package namespace
#   package_name = get('.__rave_package__.', envir = parent.frame(), inherits = TRUE)
#   pkg_env = get('...pkg_env', envir = parent.frame(), inherits = TRUE)
#   shared_env = get('...shared_env', envir = parent.frame(), inherits = TRUE)
#   output_env = get('...output_env', envir = parent.frame(), inherits = TRUE)
#   
#   # try to see if exists function `outputId` from the package
#   has_function = is.function(pkg_env[[outputId]])
#   
#   # Defines function, to be evaluated within the module
#   if(has_function){
#     assign_quo = rlang::quo({
#       assign(!!fun_name, function(...){
#         ._current_env = environment()
#         ._env = new.env()
#         ._env$get_value = function(key, ifNotFound = NULL){
#           get0(key, envir = ._current_env, ifnotfound = ifNotFound)
#         }
#         
#         ._env$async_value = function(key){
#           ..param_env = get0('..param_env', envir = ._current_env)
#           if(is.environment(..param_env)){
#             async_var = get0('async_var', envir = ..param_env)
#             if(is.function(async_var)){
#               return(async_var(key))
#             }
#           }
#           
#           return(NULL)
#         }
#         do.call(
#           asNamespace(package_name)[[!!outputId]],
#           c(list(._env), list(...))
#         )
#         
#       })
#     })
#   }else{
#     assign_quo = rlang::quo({
#       assign(!!fun_name, function(...){
#         
#         # Directly call the function 
#         do.call(!!outputId, list())
#       })
#     })
#   }
#   
#   
#   re = list(
#     outputId = outputId,
#     title = title,
#     definition = definition,
#     order = order
#   )
#   class(re) = c('comp_output', 'list')
#   
#   
#   output_env[[outputId]] = re
#   invisible(re)
# }
# 
# 
# 
# 
# 
