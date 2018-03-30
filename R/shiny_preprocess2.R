# 1 UI for all
#' @import shiny
#' @import rhdf5
#' @import stringr
#' @export
rave_pre_process <- function(
  sidebar_width = 2,
  launch.browser = T,
  host = '127.0.0.1',
  quiet = T,
  test.mode = F,
  ...
){
  rave_setup()
  default_project_name = rave_hist$get_or_save('.rave_pre_project_name', '', save = FALSE)
  default_subject_code = rave_hist$get_or_save('.rave_pre_subject_code', '', save = FALSE)
  # 10s timer
  longtimer = shiny::reactiveTimer(10000)
  longtimer_env = new.env()


  # Init Modules
  OVERVIEW_MODUDLE <- rave_pre_overview(sidebar_width = sidebar_width)
  NOTCH_MODULE <- rave_pre_notch(sidebar_width = sidebar_width, longtimer_env = longtimer_env)
  CAR_MODULE <- rave_pre_car(sidebar_width = sidebar_width)
  WAVELET_MODULE <- rave_pre_wavelet(sidebar_width = sidebar_width, longtimer_env = longtimer_env)
  EPOCH_MODULE <- rave_pre_epoch(sidebar_width = sidebar_width)

  # UI
  {
    ui = shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = 'RAVE Preprocess'
      ),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = 'sidebar',
          shinydashboard::menuItem('Overview',
                                   tabName = 'OVERVIEW'),
          shinydashboard::menuItem('Notch Filter',
                                   tabName = 'NOTCH'),
          shinydashboard::menuItem('CAR',
                                   tabName = 'CAR'),
          shinydashboard::menuItem('Wavelet',
                                   tabName = 'WAVELET'),
          shinydashboard::menuItem('Epoch (Under construction)',
                                   tabName = 'EPOCH')
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
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            'OVERVIEW',
            OVERVIEW_MODUDLE$body
          ),
          shinydashboard::tabItem(
            'NOTCH',
            NOTCH_MODULE$body
          ),
          shinydashboard::tabItem(
            'CAR',
            CAR_MODULE$body
          ),
          shinydashboard::tabItem(
            'WAVELET',
            WAVELET_MODULE$body
          ),
          shinydashboard::tabItem(
            'EPOCH',
            EPOCH_MODULE$body
          )
        )
      )
    )
  }

  server = function(input, output, session){

    user_data = reactiveValues(
      reset = NULL,

      # globals
      subject = NULL,
      project_name = default_project_name,
      subject_code = default_subject_code,
      blocks = NULL,
      channels = NULL,
      srate = 2000,
      exclchan = NULL,
      badchan = NULL,
      epichan = NULL,
      all_blocks = '',

      has_notch = FALSE,
      force_save = NULL,
      valid_channels = NULL,
      long_refresh = NULL,
      doing_wavelet = FALSE
    )
    callModule(OVERVIEW_MODUDLE$server, id = 'OVERVIEW_M', user_data = user_data)
    callModule(NOTCH_MODULE$server, id = 'NOTCH_M', user_data = user_data)
    callModule(CAR_MODULE$server, id = 'CAR_M', user_data = user_data)
    callModule(WAVELET_MODULE$server, id = 'WAVELET_M', user_data = user_data)
    callModule(EPOCH_MODULE$server, id = 'EPOCH_M', user_data = user_data)



    observeEvent(user_data$reset, {
      user_data$has_notch = FALSE
      # user_data$blocks = NULL
      # user_data$channels = NULL
      # user_data$srate = 2000
      # user_data$exclchan = NULL
      # user_data$badchan = NULL
      # user_data$epichan = NULL
      # user_data$all_blocks = ''
      if(!is.null(user_data$subject)){
        subject = user_data$subject

        # load subject info
        # update block info
        user_data$all_blocks = subject$available_blocks
        user_data$blocks = subject$blocks
        # update channel info if exists
        user_data$channels = subject$channels
        # update sample rate if exists
        srate = subject$srate
        if(!is.null(srate) && is.numeric(srate)){
          user_data$srate = srate
        }
        # update bad, epi, excluded channel info if exists
        user_data$badchan = unlist(subject$badchan)
        user_data$epichan = unlist(subject$epichan)
        user_data$exclchan = unlist(subject$exclchan)

        # load_tmp_data(force = TRUE)

        showNotification(sprintf(
          'Subject %s loaded from project %s', subject$subject_code, subject$project_name
        ), type = 'message')

        if(!is.null(subject$logger$get_or_save('notch_filtered'))){
          user_data$has_notch = TRUE
        }else{
          user_data$has_notch = FALSE
        }

        rave_hist$save(
          .rave_pre_project_name = subject$project_name,
          .rave_pre_subject_code = subject$subject_code
        )

        return(NULL)


      }
    }, priority = 1000L)

    observeEvent(user_data$force_save, {
      if(user_data$has_notch && !is.null(user_data$subject) &&
         !setequal(user_data$subject$badchan, user_data$badchan)){
        user_data$subject$set_channels(user_data$badchan, name = 'badchan')
        user_data$subject$save(action = 'Bad Channel Changed',
                               message = rave:::deparse_selections(user_data$badchan))
      }
      if(user_data$has_notch && !is.null(user_data$subject) &&
         !setequal(user_data$subject$exclchan, user_data$exclchan)){
        user_data$subject$set_channels(user_data$exclchan, name = 'exclchan')
        user_data$subject$save(action = 'Excluded Channel Changed',
                               message = rave:::deparse_selections(user_data$exclchan))
      }
      if(user_data$has_notch && !is.null(user_data$subject) &&
         !setequal(user_data$subject$epichan, user_data$epichan)){
        user_data$subject$set_channels(user_data$epichan, name = 'epichan')
        user_data$subject$save(action = 'Epilepsy Channel Changed',
                               message = rave:::deparse_selections(user_data$epichan))
      }
    }, priority = -1000L)


    longtimer_env$default = function(){
      user_data$long_refresh = Sys.time()
    }
    observeEvent(longtimer(),{
      names = ls(envir = longtimer_env)
      for(nm in names){
        func = get(nm, envir = longtimer_env)
        if(is.function(func)){
          func()
        }
      }
    })



  }
  shinyApp(ui = ui, server = server, options = list(
    launch.browser = launch.browser,
    host = host,
    quiet = quiet,
    test.mode = test.mode,
    ...
  ))
}

# rave:::rave_pre_process()





# Global variable panel
global_panel = function(ns, show_all = FALSE){
  if(show_all){
    ui = function(){
      column(
        width = 12,
        textInput(ns('project_name'), 'Project Name'),
        textInput(ns('subject_code'), 'Subject Code'),
        selectInput(ns('blocks'), 'Blocks', selected = NULL, choices = '', multiple = T),
        textInput(ns('channels'), 'Channels', placeholder = 'E.g. 1-84'),
        numericInput(ns('srate'), 'Sample Rate', value = 2000, step = 1L, min = 1L),
        textInput(ns('exclchan'), 'Excluded Channels', placeholder = 'E.g. 51,44-45'),
        textInput(ns('badchan'), 'Bad Channels', placeholder = 'E.g. 3,4,5,11-20'),
        textInput(ns('epichan'), 'Epilepsy Channels', placeholder = 'E.g. 1-10'),

        actionButton(ns('save'), 'Update Changes'),
        actionButton(ns('load'), 'Load')
      )
    }
    reactive_func = function(input, user_data, local_data, session = shiny::getDefaultReactiveDomain()){
      observe({
        updateTextInput(session, 'project_name', value = user_data$project_name)
        updateTextInput(session, 'subject_code', value = user_data$subject_code)
        updateSelectInput(session, 'blocks', choices = user_data$all_blocks, selected = user_data$blocks)
        updateTextInput(session, 'channels', value = rave:::deparse_selections(user_data$channels))
        updateNumericInput(session, 'srate', value = user_data$srate)
        updateTextInput(session, 'exclchan', value = rave:::deparse_selections(user_data$exclchan))
        updateTextInput(session, 'badchan', value = rave:::deparse_selections(user_data$badchan))
        updateTextInput(session, 'epichan', value = rave:::deparse_selections(user_data$epichan))
      }, priority = -1L)
      observeEvent(input$save, {
        if(is.null(user_data$subject)){
          return(NULL)
        }
        modal = NULL
        if(!user_data$has_notch){
          user_data$blocks = input$blocks
          user_data$channels = rave:::parse_selections(input$channels)
          user_data$srate = input$srate
        }else{
          user_data$srate = user_data$subject$srate
          user_data$channels = user_data$subject$channels
          user_data$blocks = user_data$subject$blocks
        }

        if(
          !setequal(user_data$blocks, input$blocks) ||
          !setequal(user_data$channels, rave:::parse_selections(input$channels)) ||
          user_data$srate != input$srate
        ){
          modal = modalDialog(
            title = 'Warning',
            p('You have already notch filtered this subject, the following changes will be ignored: (however the other changes will proceed)'),
            tags$ul(
              tags$li('Blocks'),
              tags$li('Channels'),
              tags$li('Sample Rate')
            ),
            p('If you want to make these changes anyway, please go to ',
              tags$blockquote(tools::file_path_as_absolute(user_data$subject$dirs$subject_dir)), br(),
              'and remove "preprocess" folder as well as "rave.yaml"'),
            easyClose = T
          )
        }
        user_data$exclchan = rave:::parse_selections(input$exclchan)
        user_data$badchan = rave:::parse_selections(input$badchan)
        user_data$epichan = rave:::parse_selections(input$epichan)
        # sync with subject
        if(
          !is.null(user_data$subject) &&
          isolate(user_data$project_name) == input$project_name &&
          isolate(user_data$subject_code) == input$subject_code
        ){
          if(!user_data$has_notch){
            user_data$subject$set_blocks(user_data$blocks)
            user_data$subject$set_channels(user_data$channels, name = 'channels')
            user_data$subject$srate = user_data$srate
          }
          user_data$subject$set_channels(user_data$badchan, name = 'badchan')
          user_data$subject$set_channels(user_data$exclchan, name = 'exclchan')
          user_data$subject$set_channels(user_data$epichan, name = 'epichan')
          user_data$subject$save(message = 'Manual Save')

          user_data$channels = user_data$subject$channels
          user_data$exclchan = user_data$subject$exclchan
          user_data$badchan = user_data$subject$badchan
          user_data$epichan = user_data$subject$epichan

        }
        if(user_data$has_notch && !is.null(modal)){
          logger('Some changes not saved.')
          updateSelectInput(session, 'blocks', choices = user_data$all_blocks, selected = user_data$blocks)
          updateTextInput(session, 'channels', value = rave:::deparse_selections(user_data$channels))
          updateNumericInput(session, 'srate', value = user_data$srate)
          showModal(modal)
        }
      })
      observeEvent(input$load, {
        logger('Loading subject')
        user_data$project_name = input$project_name
        user_data$subject_code = input$subject_code
        user_data$reset = Sys.time()
        local_data$save = Sys.time()
      })
    }
  }else{
    ui = function(){
      column(
        width = 12,
        textInput(ns('exclchan'), 'Excluded Channels', placeholder = 'E.g. 51,44-45'),
        textInput(ns('badchan'), 'Bad Channels', placeholder = 'E.g. 3,4,5,11-20'),
        textInput(ns('epichan'), 'Epilepsy Channels', placeholder = 'E.g. 1-10'),
        actionButton(ns('save'), 'Update Changes')
      )
    }
    reactive_func = function(input, user_data, local_data, session = shiny::getDefaultReactiveDomain()){
      observe({
        updateTextInput(session, 'exclchan', value = rave:::deparse_selections(user_data$exclchan))
        updateTextInput(session, 'badchan', value = rave:::deparse_selections(user_data$badchan))
        updateTextInput(session, 'epichan', value = rave:::deparse_selections(user_data$epichan))
      })
      observeEvent(input$save, {
        logger('Updating changes')
        user_data$exclchan = rave:::parse_selections(input$exclchan)
        user_data$badchan = rave:::parse_selections(input$badchan)
        user_data$epichan = rave:::parse_selections(input$epichan)
        # sync with subject
        user_data$subject$set_channels(user_data$badchan, name = 'badchan')
        user_data$subject$set_channels(user_data$exclchan, name = 'exclchan')
        user_data$subject$set_channels(user_data$epichan, name = 'epichan')
        user_data$subject$save(message = 'Manual Save')

        user_data$exclchan = user_data$subject$exclchan
        user_data$badchan = user_data$subject$badchan
        user_data$epichan = user_data$subject$epichan
        local_data$save = Sys.time()
      })
    }
  }

  return(list(
    ui = ui,
    reactive_func = reactive_func
  ))
}
