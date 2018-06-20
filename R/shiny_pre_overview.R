rave_pre_overview <- function(module_id = 'OVERVIEW_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  gp = global_panel(ns, show_all = T)

  body = fluidRow(
    shinydashboard::tabBox(
      id = ns('sidebar'),
      width = sidebar_width,
      shiny::tabPanel(
        title = 'Global',
        fluidRow(
          gp$ui()
        )
      )
    ),
    shinydashboard::box(
      width = 12 - sidebar_width,
      title = 'Overview',
      fluidRow(
        column(
          width = 6,
          h2('Basic Information'),
          tableOutput(ns('overview_table'))
        ),
        column(
          width = 6,
          h2('Channel Settings'),
          tableOutput(ns('channel_table'))
        ),
        column(
          width = 12,
          h2('Log'),
          tableOutput(ns('subject_log_table'))
        )
      )
    )
  )

  server = function(input, output, session = getDefaultReactiveDomain(), user_data){
    local_data = reactiveValues(
      save = NULL
    )
    gp$reactive_func(input = input, user_data = user_data, local_data = local_data, session = session)
    observeEvent(local_data$save, {
      # Overview is set, look for subject
      subject_code = user_data$subject_code
      project_name = user_data$project_name
      dirs = get_dir(subject_code = subject_code, project_name = project_name)

      if(project_name == '' || subject_code == ''){
      }else if(!dir.exists(dirs$pre_subject_dir)){
        showNotification(p('This subject does not exist!'), type = 'error')
      }else{
        tmp_subject = isolate(user_data$subject)
        if(!is.null(tmp_subject)){
          if(assertthat::are_equal(
            c(tmp_subject$project_name, tmp_subject$subject_code),
            c(input$project_name, input$subject_code)
          )){
            return(NULL)
          }
        }
        user_data$subject = rave:::SubjectInfo2$new(
          project_name = input$project_name,
          subject_code = input$subject_code
        )
        showNotification('Subject Found! Trying to retrieve information...', type = 'message')

      }

    }, priority = 1001L)

    output$overview_table <- renderTable({
      validate(
        need(!is.null(user_data$subject), 'No Entry.')
      )
      list(
        `Project Name` = user_data$project_name,
        `Subject Code` = user_data$subject_code,
        `Blocks` = paste(user_data$blocks, collapse = ', '),
        `Channels` = rave:::deparse_selections(user_data$channels)
      ) ->
        tbl
      data.frame(
        Entry = names(tbl),
        Value = unlist(tbl)
      )
    })

    output$channel_table <- renderTable({
      validate(
        need(!is.null(user_data$subject), 'No Entry.')
      )
      vc = user_data$valid_channels
      user_data$long_refresh
      last_car = user_data$subject$logger$get_or_save('CAR_plan')
      last_wavelet = user_data$subject$logger$get_or_save('last_wavelet')
      if(is.null(last_car) || !is.character(last_car)){
        last_car = ''
      }
      wave_channels = 'Not Started'
      wave_car_plan = 'NA'
      wave_succeed = 'NA'
      if(length(last_wavelet) > 0){
        wave_channels = rave:::deparse_selections(last_wavelet[['channels']])
        wave_car_plan = last_wavelet[['car_plan']]
        wave_succeed = c('Not Finished', 'Yes!')[last_wavelet[['succeed']] + 1]
      }
      list(
        `Excluded Channels` = rave:::deparse_selections(user_data$exclchan),
        `Bach Channels` = rave:::deparse_selections(user_data$badchan),
        `Epilepsy Channel` = rave:::deparse_selections(user_data$epichan),
        `Current CAR Strategy` = last_car,
        `Last Wavelet Channels` = wave_channels,
        `Last Wavelet CAR Channels` = wave_car_plan,
        `Last Wavelet Finished` = wave_succeed
      ) ->
        tbl
      data.frame(
        Entry = names(tbl),
        Value = unlist(tbl)
      )
    })

    output$subject_log_table <- renderTable({
      validate(
        need(!is.null(user_data$subject), 'Please enter subject information.')
      )
      force(local_data$save)
      force(user_data$force_save)
      log = as.data.frame(user_data$subject$logger$get_or_save('log'))
      log
    })
  }

  return(list(
    body = body,
    server = server
  ))
}
