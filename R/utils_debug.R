
# Function for module writers to debug modules
# @param m module object see ModuleEnvir
# @param private_id session rave_id
# @details To use this function, you must use test.mode=TRUE, i.e. call init_app(m, test.mode=TRUE)
# otherwise objects will be removed. Once imported data, close rave session and run debug_module(m)
# to see variables within the module
# debug_module <- function(m, private_id){
#   if(missing(private_id)){
#     private_id = ls(m$private$exec_env)[1]
#   }
#
#   e = m$private$exec_env[[private_id]]
#   assertthat::assert_that(is(e, 'ExecEnvir'))
#
#   # attach data repo
#   attachDefaultDataRepository()
#
#   # get static env
#   vars = ls(e$static_env, all.names = T)
#   genv = globalenv()
#   for(k in vars){
#     v = e$static_env[[k]]
#     if(is.reactivevalues(v)){
#       v = isolate(shiny::reactiveValuesToList(v))
#     }
#
#     if(is.function(v)){
#       environment(v) = new.env(parent = genv)
#     }
#     genv[[k]] = v
#   }
#
#   # get param_env, runtime_env
#   copy_env(e$param_env, genv)
#   copy_env(e$runtime_env, genv)
#
# }
