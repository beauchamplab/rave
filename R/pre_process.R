#' RAVE Preprocess Function
#' @usage rave_pre_process(sidebar_width = 3L, launch.browser = T, host = '127.0.0.1', ...)
#' @import shiny
#' @import rhdf5
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @export
rave_preprocess <- function(
  sidebar_width = 3,
  launch.browser = T,
  host = '127.0.0.1',
  quiet = T,
  test.mode = F,
  modules,
  ver = '3',
  ...
){
  # Steps 0 Variables
  default_project_name = ''
  default_subject_code = ''
  model_instances = NULL

  future::plan(future::multiprocess, workers = rave::rave_options('max_worker'))


  modules = list(
    list(
      ID = 'OVERVIEW',
      name = 'Overview',
      checklevel = 0,
      ..func = 'rave_pre_overview3'
    ),
    list(
      ID = 'NOTCH',
      name = 'Notch Filter',
      checklevel = 1,
      ..func = 'rave_pre_notch3'
    ),
    list(
      ID = 'WAVELET',
      name = 'Wavelet',
      checklevel = 2,
      ..func = 'rave_pre_wavelet3'
    ),
    list(
      ID = 'EPOCH',
      name = 'Trial Epoch',
      checklevel = 1,
      ..func = 'pre_epoch3'
    )
  )

  # Step 2: initialize models

  model_instances <- lapply(modules, function(x){
    with(x, {
      ..func %?<-% str_c('rave_pre_', str_to_lower(ID), ver)
      instance = do.call(..func, list(
        module_id = ID %&% '_M',
        sidebar_width = sidebar_width
      ), envir = loadNamespace('rave'))

      list(
        ID = ID,
        name = name,
        call = ..func,
        UI = instance$body,
        server = instance$server,
        checklevel = checklevel
      )
    })
  })


  ui = rave:::dashboardPage(
    control = div(),
    header = shinydashboard::dashboardHeader(
      title = 'RAVE Preprocess'
    ),
    sidebar = shinydashboard::dashboardSidebar(
      tagList(shinydashboard::sidebarMenu(
        id = 'sidebar',
        lapply(model_instances, function(x){
          shinydashboard::menuItem(
            x$name,
            tabName = x$ID
          )
        }))
      )
    ),
    body = shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      singleton(
        tags$head(tags$script(str_c(
          'Shiny.addCustomMessageHandler("alertmessage",',
          'function(message) {',
          'alert(message);',
          '});'
        )))
      ),

      do.call(shinydashboard::tabItems, lapply(model_instances, function(x){
        shinydashboard::tabItem(x$ID, x$UI)
      }))
    )
  )




  server = function(input, output, session){
    user_data <- reactiveValues(
      checklevel = 0,
      reset = NULL
    )

    env <- environment()
    utils <- rave_preprocess_tools(env = env)

    # Reset modules
    utils$reset = function(){
      user_data$reset = Sys.time()
    }

    utils$last_inputs = function(name){
      last_project_name = rave_hist$get_or_save('.rave_pre_project_name', '', save = FALSE)
      last_subject_code = rave_hist$get_or_save('.rave_pre_subject_code', '', save = FALSE)
      last_notch_freq = rave_hist$get_or_save('.rave_pre_notch_freq', 60, save = FALSE)

      return(list(
        last_project_name = last_project_name,
        last_subject_code = last_subject_code
      ))
    }

    # Load subject
    utils$load_subject = function(subject_code, project_name){
      logger('Loading Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      assert_that(dir.exists(dirs$preprocess_dir), msg = 'Subject ' %&% subject_code %&% ' has no project folder ' %&% project_name)
      s = SubjectInfo2$new(project_name = project_name, subject_code = subject_code)
      id = s$project_name %&% '/' %&% s$subject_code

      rave_hist$save(
        .rave_pre_project_name = project_name,
        .rave_pre_subject_code = subject_code
      )

      if(id != utils$get_subject_id()){
        env$subject = s
      }
      utils$reset()
      logger('Loaded Subject')
    }




    # # Save subject
    # save_subject = function(subject_code, project_name){
    #   logger('Saving Subject')
    #   dirs = get_dir(subject_code = subject_code, project_name = project_name)
    #   is_created = FALSE
    #   if(!dir.exists(dirs$preprocess_dir)){
    #     dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = c('preprocess_dir', 'cache_dir', 'meta_dir'))
    #     is_created = TRUE
    #   }else{
    #     # save informations if subject exists
    #     if(!is.null(env$subject) && (
    #       env$subject$subject_code == subject_code &&
    #       env$subject$project_name == project_name
    #     )){
    #       env$subject$save(action = 'Manual Save', message = '')
    #     }
    #   }
    #
    #   # call to load subject
    #   utils$load_subject(subject_code, project_name)
    #
    #   logger('saved Subject')
    #   return(is_created)
    # },

    # save_reference_table = function(tbl){
    #   if(nrow(tbl)){
    #     tbl$ChlType = sapply(tbl$Channel, utils$get_channel_type)
    #   }
    #   utils$save_to_subject(reference_table = tbl)
    # },


    # save_meta_electrodes = function(){
    #   Channel = env$subject$channels
    #   rave:::save_meta(
    #     data = meta_elec,
    #     meta_type = 'electrodes',
    #     project_name = env$subject$project_name,
    #     subject_code = env$subject$subject_code
    #   )
    # },

    lapply(model_instances, function(x){
      callModule(x$server, id = x$ID %&% '_M', user_data = user_data, utils = utils)
    })

  }


  shinyApp(ui = ui, server = server, options = list(
    host = host, launch.browser = launch.browser
  ))
}

#' @export
rave_pre_process = rave_preprocess

