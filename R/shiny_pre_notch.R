rave_pre_notch <- function(module_id = 'NOTCH_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  gp = global_panel(ns)

  body = fluidRow(
    shinydashboard::tabBox(
      id = ns('sidebar'),
      width = sidebar_width,
      shiny::tabPanel(
        title = 'Notch Filter',
        fluidRow(
          column(
            12,
            uiOutput(ns('notch_run'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Global',
        fluidRow(
          gp$ui()
        )
      )
    ),
    shinydashboard::box(
      width = 12 - sidebar_width,
      title = 'Notch - Inspect Signals',
      plotOutput(ns('plot'), height = '80vh')
    )
  )

  server = function(input, output, session, user_data){
    local_data = reactiveValues(
      save = Sys.Date(),
      is_notch = FALSE
    )
    gp$reactive_func(input = input, user_data = user_data, local_data = local_data, session = session)

    output$notch_run <- renderUI({
      if(user_data$has_notch){
        return(tagList(
          selectInput(ns('current_block'), 'Current Block', choices = user_data$blocks),
          selectInput(ns('current_channel'), 'Current Channel', choices = user_data$channels),
          div(
            actionButton(ns('notch_prev'), 'Previous'),
            actionButton(ns('notch_next'), 'Next')
          ),
          checkboxInput(ns('isepi'), 'Mark as Epilepsy Channel'),
          checkboxInput(ns('isbad'), 'Mark as Bad Channel'),
          checkboxInput(ns('isexc'), 'Exclude Channel from CAR [*]'),
          hr(),
          p(tags$small("[*] These channels will be excluded from CAR calculation, but will be counted as good channels."),
            br(), tags$small('IMPORTANT: Click ', strong('Save Changes'), ' to apply (save) any changes')),
          div(
            actionButton(ns('save_after'), 'Save Changes'),
            actionButton(ns('save_plots'), 'Export Plots')
          )
        ))
      }else{
        return(tagList(
          p("You haven't run Notch filter on this subject yet."),
          actionButton(ns('notch_run'), 'Apply Notch Filter')
        ))
      }
    })
    observeEvent(input$save_after, {
      user_data$force_save = Sys.time()
    })
    observeEvent(input$save_plots, {
      showNotification(p('Plots are being saved to ', user_data$subject$dirs$preprocess_dir), type = 'message')
      logger('Plots are being saved to ', user_data$subject$dirs$preprocess_dir, level = 'INFO')
      save_notch_plots(
        project_name = user_data$project_name,
        subject_code = user_data$subject_code,
        blocks = user_data$blocks,
        chls = user_data$channels,
        srate = user_data$srate,
        cex = 2
      )

    })
    observeEvent(input$current_channel, {
      channel = as.integer(input$current_channel)
      if(channel %in% user_data$badchan){
        updateCheckboxInput(session, 'isbad', value = TRUE)
      }else{
        updateCheckboxInput(session, 'isbad', value = FALSE)
      }
      if(channel %in% user_data$exclchan){
        updateCheckboxInput(session, 'isexc', value = TRUE)
      }else{
        updateCheckboxInput(session, 'isexc', value = FALSE)
      }
      if(channel %in% user_data$epichan){
        updateCheckboxInput(session, 'isepi', value = TRUE)
      }else{
        updateCheckboxInput(session, 'isepi', value = FALSE)
      }
    })
    output$plot <- renderPlot({
      block = input$current_block
      channel = as.integer(input$current_channel)
      validate(
        need(user_data$has_notch, 'Please apply notch filter first.'),
        need(length(block) == 1 || length(channel) == 1, '')
      )

      pre_plot_notch(
        project_name = user_data$project_name,
        subject_code = user_data$subject_code,
        block_num = block,
        chl = channel,
        srate = user_data$srate,
        cex = 2
      )
    })
    observeEvent(input$notch_prev, {
      all_c = as.integer(user_data$channels)
      c_c = as.integer(input$current_channel)
      c_n = all_c[all_c < c_c]
      if(length(c_n) > 0){
        updateSelectInput(session, 'current_channel', selected = max(c_n))
      }
    })
    observeEvent(input$notch_next, {
      all_c = as.integer(user_data$channels)
      c_c = as.integer(input$current_channel)
      c_n = all_c[all_c > c_c]
      if(length(c_n) > 0){
        updateSelectInput(session, 'current_channel', selected = min(c_n))
      }
    })
    observe({
      if(user_data$has_notch && is.logical(input$isbad)){
        c_c = as.integer(input$current_channel)
        bc = isolate(user_data$badchan)
        if(input$isbad && !c_c %in% bc){
          user_data$badchan = c(bc, c_c)
        }else if(!input$isbad && c_c %in% bc){
          bc = bc[bc != c_c]
          user_data$badchan = bc
        }
      }
    }, priority = -2L)
    observe({
      if(user_data$has_notch && is.logical(input$isepi)){
        c_c = as.integer(input$current_channel)
        bc = isolate(user_data$epichan)
        if(input$isepi && !c_c %in% bc){
          user_data$epichan = c(bc, c_c)
        }else if(!input$isepi && c_c %in% bc){
          bc = bc[bc != c_c]
          user_data$epichan = bc
        }
      }
    }, priority = -2L)
    observe({
      if(user_data$has_notch && is.logical(input$isexc)){
        c_c = as.integer(input$current_channel)
        bc = isolate(user_data$exclchan)
        if(input$isexc && !c_c %in% bc){
          user_data$exclchan = c(bc, c_c)
        }else if(!input$isexc && c_c %in% bc){
          bc = bc[bc != c_c]
          user_data$exclchan = bc
        }
      }
    }, priority = -2L)


    observeEvent(input$notch_run, {
      # check whether channel settings are proper, show modal
      chls = user_data$channels
      blocks = user_data$blocks
      if(length(chls) == 0 || length(blocks) == 0){
        modal = modalDialog(
          title = 'Error',
          p('Please select blocks or channels in ', strong("Overview"), ' section.'),
          easyClose = T
        )
      }else{
        modal = modalDialog(
          title = 'Confirmation',
          p('You are going to apply notch filter. Upon finished, ',
            tags$label('blocks'), ', ',tags$label('channels'), ' and ',
            tags$label('sample rate'),
            ' will freeze and no longer editable unless you manually remove the preprocess folder.'),
          p(strong('Please confirm the channels:')),
          tags$blockquote(deparse_selections(chls)),
          footer = tagList(
            actionButton(ns('cancel'), "Cancel"),
            actionButton(ns("ok"), "OK")
          )
        )
      }

      showModal(modal)
    })

    observeEvent(input$cancel, {
      if(local_data$is_notch){
        session$sendCustomMessage(
          type = 'alertmessage', message = str_c(
            "Notch filter is still in the background. Please wait for 1-2 minutes ",
            "before it could finalize writing. The modal will disapear automatically ",
            "once ready."
          ))
      }else{
        removeModal()
      }
    })
    observeEvent(input$ok, {
      if(local_data$is_notch){
        return(NULL)
      }
      # run notch
      progress = shiny::Progress$new(session = session, min = 0, max = 1)
      on.exit(progress$close())
      progress$set(message = 'Calculation in progress',
                   detail = 'This may take a while...')

      logger('Notch filter begins.')
      local_data$is_notch = T
      bulk_notch2(
        project_name = user_data$project_name,
        subject_code = user_data$subject_code,
        blocks = user_data$blocks,
        channels = user_data$channels,
        srate = user_data$srate,
        progress = progress
      )

      showNotification(p('Notch Finished!'), duration = NULL, type = 'message')
      # save subject
      user_data$subject$logger$save(notch_filtered = user_data$channels)
      user_data$has_notch = T
      local_data$is_notch = F
      user_data$valid_channels = user_data$channels
      user_data$subject$logger$save(CAR_plan = sprintf('Exc[%s]Bad[]Epi[]', deparse_selections(user_data$channels)))
      # dismiss
      shiny::removeModal()
    })
  }

  return(list(body = body, server = server))
}
