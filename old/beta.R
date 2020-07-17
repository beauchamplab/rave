# if(FALSE){
# # load RAVE
# if(!'rave_dev' %in% search()){
#   rave_dev = loadNamespace('rave')
#   attach(rave_dev)
# }
# 
# # This file contains all experimental functions 
# 
# 
# # FIXME
# # this function is for dev purpose-only!!!
# get_module_map <- local({
#   map <- dipsaus::fastmap2()
#   function(){
#     map
#   }
# })
# 
# 
# 
# rave_needs <- function(expr, label, env = parent.frame()){
#   expr <- substitute(expr)
#   if(missing(label)){
#     label <- deparse(expr)[[1]]
#   }
#   tryCatch({
#     stopifnot(eval(expr, envir = env))
#   }, error = function(e){
#     e <- simpleError(label, call = expr)
#     class(e) <- c('rave_check_error', 'condition')
#     signalCondition(e)
#   })
#   invisible()
# }
# 
# 
# 
# 
# 
# rave_validate <- rave_context_generics(
#   fun_name = 'rave_validate', 
#   fun = function(checks, on_failing, on_import, ...){})
# 
# rave_validate.default <- function(checks, on_failure, on_import, ...){
#   stopifnot2(is.function(checks), is.function(on_failure), is.function(on_import),
#              msg = 'All arguments should be functions')
#   
#   rave_context()
#   
#   # Stores error list label -> error condition
#   err_list <- list()
#   
#   # stores observer
#   observer_list <- list()
#   remove_observers <- function(){
#     for(obs in observer_list){
#       obs$suspend()
#       obs$destroy()
#     }
#   }
#   
#   
#   # Run checks, signal all failed checks as "rave_check_error" 
#   # and store them in err_list
#   # the results will be stored in check_opt
#   withCallingHandlers({
#     check_opt <- checks(get_module_map())
#   }, rave_check_error = function(e){
#     # only captures rave_check_error
#     err_list[[e$message]] <<- e
#   })
#   
#   res <- check_opt
#   
#   # Case 1: err_list is not null, need more data
#   if(length(err_list)){
#     
#     # print out error message
#     cat2('RAVE might require more data...')
#     for(e in err_list){
#       cat2('  ', e$message, ' - [DEBUG INFO]: ', deparse(e$call)[[1]])
#     }
#     
#     # calling on_failure, but need to store observers that will be 
#     # destroyed later
#     
#     # Since R will copy this function, original function won't be affected
#     f_env <- environment(on_failure)
#     sneaky_env <- new.env(parent = f_env)
#     environment(on_failure) <- sneaky_env
#     sneaky_env$observe <- function(...){
#       fargs <- as.list(match.call()[-1])
#       observer_list[[length(observer_list) + 1]] <<- do.call(
#         shiny::observe, fargs, envir = parent.frame()
#       )
#     }
#     sneaky_env$observeEvent <- function(...){
#       fargs <- as.list(match.call()[-1])
#       fargs[['ignoreInit']] <- TRUE
#       observer_list[[length(observer_list) + 1]] <<- do.call(
#         shiny::observeEvent, fargs, envir = parent.frame()
#       )
#     }
#     # Don't use other reactive functions... observe and observeEvent are enough!
#     # To explain what I did here:
#     # I override observeEvent, making it like the following structure
#     # ------------------------------------------
#     # local({
#     #   observeEvent <- ...
#     #   on_failure <- function(...){...}
#     #   on_failure
#     # })
#     # ------------------------------------------
#     # However, observeEvent has argument 'expr' that requires to run
#     # in the run-time environment 
#     # when on_failure gets called, it generates runtime-env
#     # which is observeEvent's caller-env (observeEvent() it gets called 
#     # while on_failure is running). 
#     # In sneaky_env$observeEvent, `parent.frame()` is the caller's env
#     # hence on_failure's runtime-env, we evaluate `observe` with no 
#     # side effect there
#     
#     modal_info <- on_failure(check_opt, get_module_map(), getDefaultDataRepository())
#     
#     # ------ 
#     # From now on, we need to make the function into rave_running context
#     
#     session <- getDefaultReactiveDomain()
#     input <- session$input
#     ns <- session$ns
#     print(ns('asdasd'))
#     
#     # If map is returned, that means data has been loaded
#     # and no need to pop up modal
#     # TODO: change is.list to is-fastmap2
#     if(!is.list(modal_info)){
#       cat2('No need to display data select panel. Pass!')
#       return(get_module_map())
#     }
#     
#     # TODO: wrap this into a function and add default UI
#     title <- modal_info$title
#     if(length(title) != 1){ title = 'Loading more data' }
#     shiny::showModal(shiny::modalDialog(
#       title = title, size = 'l', easyClose = FALSE, 
#       
#       # this is a lazy call as I assume nobody uses this ID
#       # Maybe we should as people DO NOT use ID starting with ..
#       # as they are reserved for RAVE
#       footer = div(
#         shiny::actionButton(
#           ns('..rave_import_data_btn_cancel..'), "Previous Module"),
#         dipsaus::actionButtonStyled(
#           ns('..rave_import_data_btn..'), "Import Data")
#       ), 
#       shiny::div(
#         modal_info$ui,
#         shiny::hr(),
#         shiny::tags$small(
#           'Expected load: ', modal_info$expectedloadingtime, ' seconds',
#           sprintf(', %.2f GB.', modal_info$expectedloadingsize / 1024^3))
#       )
#     ))
#     
#     # add observer ONLY WHEN rave_running mode
#     observer_list[[length(observer_list) + 1]] <- shiny::observeEvent(
#       { 
#         input$..rave_import_data_btn..
#         }, {
#           
#         # Don't click the button twice
#         dipsaus::updateActionButtonStyled(session, '..rave_import_data_btn..', disabled = TRUE)
#         
#           cat2('Loading start')
#           
#         tryCatch({
#           res <- on_import(check_opt, get_module_map())
#           
#           # REMOVE all observers!!!
#           remove_observers()
#           
#           removeModal()
#           
#         }, error = function(e){
#           dipsaus::updateActionButtonStyled(session, '..rave_import_data_btn..', disabled = FALSE)
#           # TODO: signal e further up
#         })
#         
#         
#       }, 
#       # Important!!!
#       # When button is initialized, its value is NULL or 0?
#       # When we click on the btn, the value ++, and won't reset even
#       # we destroy the observer, hence next time when the screen load up
#       # if ignoreInit is FALSE, this observer automatically start
#       # but user hasn't chosen data yet
#       ignoreInit = TRUE
#     )
#     
#   } else{
#     return(res)
#   }
# }
# 
# 
# 
# rave_debug()
# ctx <- rave_context()
# .id = ctx$module_id
# ns <- shiny::NS(.id)
# library(shiny)
# ui <- fluidPage(
#   actionButton(ns('input'), 'Input 1')
# )
# server <- function(input, output, session) {
#   observeEvent(input$input, {
#     
#     rave_validate.default(
#       function(map){
#         rave_needs(stop(123))
#         rave_needs(stop('123asad'))
#         
#         return(345)
#       },
#       function(opt, map, dataenv){
#         signiture = sample(letters, 1)
#         
#         ui <- div('UI of choices',
#                   selectInput(ns('project'), 'Project', choices = get_projects(), 
#                               selected = dataenv$subject$project_name))
#         
#         # Observers MUST explicitly defined here they will be marked and destroyed once user hit "import"
#         # See `Test` section below
#         observeEvent(input$project, {
#           cat(signiture, ' ', input$project, '\n')
#         })
#         
#         # I'm not sure whether we need to return a list or just UI
#         # we can return a list that might be useful to tell RAVE that
#         # you could hide some components. For example, if
#         # expectedloadingtime is NULL, then hide `Expected loading time` 
#         # Need to think more on what UI can be customized
#         return(list(
#           # return UI, either function or HTML tags { }
#           ui = ui,
#           title = '',
#           expectedloadingtime = 10,
#           expectedloadingsize = 1024^2
#         ))
#       },
#       function(opt, map){
#         print('I am loading I am loading...')
#         # stop('loading still failed :/')
#         return(map)
#       }
#     )
#     
#   })
# }
# shinyApp(ui, function(input, output, session){
#   shiny::callModule(server, id = .id, session = session)
# })
# 
# 
# }
