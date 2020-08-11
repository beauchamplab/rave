#' RAVE Preprocess Function
#' @param sidebar_width sidebar width from 1 to 11.
#' @param launch.browser whether to launch browser, default is on
#' @param host default is \code{"localhost"}
#' @param port integer port of the app
#' @param quiet soft deprecated
#' @param beta whether to load experimental modules, default is false
#' @param test.mode passed to \code{\link[shiny]{shinyApp}}
#' @param ver internally used please don't change
#' @param theme color theme
#' @param modules preprocess modules to load, reserved
#' @param ... used for other functions for configuration and debug only
#' @export
rave_preprocess <- function(
  sidebar_width = 3,
  launch.browser = TRUE,
  host = '127.0.0.1',
  port = NULL,
  quiet = TRUE,
  beta = FALSE,
  test.mode = FALSE,
  modules,
  ver = '3',
  theme = 'purple',
  ...
){
  # Steps 0 Variables
  default_project_name = ''
  default_subject_code = ''
  model_instances = NULL

  rave_setup_workers()


  modules = list(
    list(
      ID = 'OVERVIEW',
      name = 'Overview',
      checklevel = 0,
      doc_prefix = 'ravepreprocessoverview',
      ..func = 'rave_pre_overview3'
    ),
    list(
      ID = 'NOTCH',
      name = 'Step 1. Notch Filter',
      checklevel = 1,
      doc_prefix = 'ravepreprocessnotch',
      ..func = 'rave_pre_notch3'
    ),
    list(
      ID = 'WAVELET',
      name = 'Step 2. Wavelet',
      checklevel = 2,
      doc_prefix = 'ravepreprocesswavelet',
      ..func = 'rave_pre_wavelet3'
    )
  )
  
  if( beta ){
    modules <- c(modules, list(
      list(
        ID = 'EPOCH',
        name = 'Step 3. Trial Epoch',
        checklevel = 1,
        doc_prefix = 'ravepreprocessepoch',
        ..func = 'pre_epoch3'
      ),
      list(
        ID = 'ELECLOCAL',
        name = 'E-Localization (Beta)',
        checklevel = 1,
        doc_prefix = 'ravepreprocesseleclocalization',
        ..func = 'rave_pre_eleclocal3'
      )
      ,
      list(
        ID = 'ELECLOCALCT',
        name = 'E-Localization w/ CT (Beta)',
        checklevel = 1,
        doc_prefix = 'ravepreprocesseleclocalizationct',
        ..func = 'rave_pre_eleclocalct3'
      )
    ))
  }

  # Step 2: initialize models

  model_instances <- lapply(modules, function(x){
    with(x, {
      ..func %?<-% stringr::str_c('rave_pre_', stringr::str_to_lower(ID), ver)
      instance = do.call(..func, list(
        module_id = paste0(ID , '_M'),
        sidebar_width = sidebar_width
      ))#, envir = loadNamespace('rave'))

      list(
        ID = ID,
        name = name,
        call = ..func,
        UI = instance$body,
        server = instance$server,
        checklevel = checklevel,
        doc_prefix = doc_prefix
      )
    })
  })


  ui = dashboardPage(
    skin = theme,
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
        tags$head(tags$script(stringr::str_c(
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
      last_project_name = rave_hist()$get_or_save('.rave_pre_project_name', '', save = FALSE)
      last_subject_code = rave_hist()$get_or_save('.rave_pre_subject_code', '', save = FALSE)
      last_notch_freq = rave_hist()$get_or_save('.rave_pre_notch_freq', 60, save = FALSE)

      return(list(
        last_project_name = last_project_name,
        last_subject_code = last_subject_code
      ))
    }

    # Load subject
    utils$load_subject = function(subject_code, project_name){
      catgl('Loading Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      stopifnot2(dir.exists(dirs$preprocess_dir), msg = paste0(
        'Subject ' , subject_code , ' has no project folder ' , project_name
      ))
      s = SubjectInfo2$new(project_name = project_name, subject_code = subject_code)
      id = paste0(s$project_name , '/' , s$subject_code)

      rave_hist()$save(
        .rave_pre_project_name = project_name,
        .rave_pre_subject_code = subject_code
      )

      if(id != utils$get_subject_id()){
        env$subject = s
      }
      utils$reset()
      catgl('Loaded Subject')
    }

    lapply(model_instances, function(x){
      catgl(x$ID)
      shiny::callModule(x$server, id = paste0(x$ID , '_M'), user_data = user_data, utils = utils, doc_prefix = x$doc_prefix, ...)
    })

  }


  shinyApp(ui = ui, server = server, options = list(
    host = host, port = port, launch.browser = launch.browser
  ))
}

