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
          numericInput(ns('srate'), 'Sample Rate', value = 2000, step = 1, min = 1),
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

  rave_pre_overview <- function(module_id = 'OVERVIEW_M'){
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

    server = function(input, output, session = shiny::getDefaultReactiveDomain(), user_data){
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

  rave_pre_notch <- function(module_id = 'NOTCH_M'){
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
        rave:::save_notch_plots(
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

        rave:::pre_plot_notch(
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
            tags$blockquote(rave:::deparse_selections(chls)),
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
        rave:::bulk_notch2(
          project_name = user_data$project_name,
          subject_code = user_data$subject_code,
          blocks = user_data$blocks,
          channels = user_data$channels,
          srate = user_data$srate,
          progress = progress
        ) ->
          check

        longtimer_env$notch_check = function(){
          res = check()
          r = (unlist(res) != FALSE)
          if(sum(r) == length(r)){
            showNotification(p('Notch Finished!'), duration = NULL, type = 'message')
            # save subject
            user_data$subject$logger$save(notch_filtered = user_data$channels)
            user_data$has_notch = T
            local_data$is_notch = F
            # dismiss
            shiny::removeModal()
            rave_setup()
            rm('notch_check', envir = longtimer_env)
          }
        }
      })
    }

    return(list(body = body, server = server))
  }

  rave_pre_car <- function(module_id = 'CAR_M'){
    ns = shiny::NS(module_id)
    gp = global_panel(ns)
    jscode = paste(
      "shinyjs.rave_car_sidebar = function(){",
      sprintf("$('#%s [%s]').click();", ns('sidebar'), 'data-value="Post-CAR"'),
      "};",
      "shinyjs.rave_car_main = function(){",
      sprintf("$('#%s [%s]').click();", ns('main'), 'data-value="Details (Post CAR)"'),
      "};"
    )
    # sprintf("shinyjs.customized_click = function(selector){$(selector).click()}", ns('sidebar'), 'data-value="Post-CAR"')

    body = fluidRow(
      shinydashboard::tabBox(
        id = ns('sidebar'),
        width = sidebar_width,
        shiny::tabPanel(
          title = 'CAR',
          fluidRow(
            column(
              12,
              uiOutput(ns('load_ui')),
              uiOutput(ns('inner_ui')),
              uiOutput(ns('inner_ui2')),
              uiOutput(ns('inner_ui3'))
            )
          )
        ),
        shiny::tabPanel(
          title = 'Post-CAR',
          fluidRow(
            column(
              12,
              uiOutput(ns('inner_ui4'))
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
      shinydashboard::tabBox(
        id = ns('main'),
        width = 12 - sidebar_width,
        shiny::tabPanel(
          title = 'All Channels',
          fluidRow(
            column(
              12,
              plotOutput(ns('parallel_plot'), height = '95vh', click = ns('pp_clicked'))
            )
          )
        ),
        shiny::tabPanel(
          title = 'Details (Post CAR)',
          fluidRow(
            column(
              12,
              plotOutput(ns('post_car_plot'), height = '80vh')
            )
          )
        )
      ),
      shinyjs::extendShinyjs(text = jscode)
    )

    server = function(input, output, session, user_data){
      local_data = reactiveValues(
        save = Sys.Date(),
        loaded = FALSE,
        car_5 = list(),
        car_20 = list(),
        s_5 = list(),
        s_20 = list(),
        space = 0,
        click_msg = 'Click on the plot.',
        start_time = 0,
        car_plan = NULL,
        an = NULL,
        plan = NULL,
        exc = NULL,
        bad = NULL,
        epi = NULL,
        refresh_post = NULL
      )
      gp$reactive_func(input = input, user_data = user_data, local_data = local_data, session = session)

      output$load_ui <- renderUI({
        if(!local_data$loaded){
         tagList(
           actionButton(ns('car_load'), 'Load Data')
         )
        }else{
          div()
        }
      })


      observeEvent(input$main, {
        if(input$main == "Details (Post CAR)" && input$sidebar != "Post-CAR"){
          shinyjs::js$rave_car_sidebar()
        }
      })

      observeEvent(input$sidebar, {
        if(input$main != "Details (Post CAR)" && input$sidebar == "Post-CAR"){
          shinyjs::js$rave_car_main()
        }
      })



      output$inner_ui <- renderUI({
        validate(need(local_data$loaded, 'Please load data first.'))
        max_time = dim(local_data$s_20[[1]])[2] / isolate(user_data$srate) * 20
        tagList(
          selectInput(ns('current_block'), 'Block:', choices = names(local_data$s_20)),
          numericInput(ns('start_time'), 'Start Time:', value = 0, min = 0, max = max_time, step = 1),
          numericInput(ns('duration'), 'Duration:', value = 25, min = 1, max = 40),
          numericInput(ns('space'), 'Voltage Scale', value = 0, step = 1, min = 0),
          div(
            actionButton(ns('prev'), 'Previous'),
            actionButton(ns('nxt'), 'Next')
          )
        )
      })
      observeEvent(input$nxt, {
        max_time = dim(local_data$s_20[[1]])[2] / isolate(user_data$srate) * 20
        c_t = min(max_time - input$duration, input$start_time + 0.9 * input$duration)
        updateNumericInput(session, 'start_time', value = c_t)
      })
      observeEvent(input$prev, {
        c_t = max(0, input$start_time - 0.9 * input$duration)
        updateNumericInput(session, 'start_time', value = c_t)
      })
      output$inner_ui2 <- renderUI({
        validate(need(local_data$loaded, ''))
        tagList(
          tags$small(local_data$click_msg),
          hr(),
          textInput(ns('car_epi'), 'Epilepsy Channels', value = rave:::deparse_selections(user_data$epichan)),
          textInput(ns('car_bad'), 'Bad Channels', value = rave:::deparse_selections(user_data$badchan)),
          textInput(ns('car_exc'), 'Excluded Channels', value = rave:::deparse_selections(user_data$exclchan)),
          div(
            actionButton(ns('update'), 'Update Plot'),
            actionButton(ns('calc'), 'Calculate Average')
          )
        )
      })
      output$inner_ui3 <- renderUI({
        all_choices = local_data$car_plan
        validate(need(length(all_choices) > 0, 'Please Calculate Average Signals First.'))
        tagList(
          hr(),
          selectInput(ns('car_plan_choice'), 'Select an average:', choices = all_choices, selected = local_data$new_plan),
          actionButton(ns('car_execute'), 'Apply CAR')
        )
      })
      observeEvent(user_data$reset, {
        local_data$loaded = FALSE
        local_data$s_5 = list()
        local_data$s_20 = list()
        local_data$click_msg = 'Click on the plot.'
        local_data$start_time = 0
        gc()
      })
      observe({
        if(!is.null(user_data$subject)){
          car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
          gnames = names(car_plans)
          if(length(gnames)){
            local_data$car_plan = gnames
            local_data$new_plan = gnames[1]
            return(NULL)
          }

        }
        local_data$car_plan = NULL
        local_data$new_plan = NULL
      })
      observe({
        local_data$refresh_post
        if(!is.null(user_data$subject)){
          plan = user_data$subject$logger$get_or_save('CAR_plan')
          if(length(plan)){
            plan = plan[1]
            local_data$plan = plan
            parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
            parsed = as.vector(parsed)
            name = parsed[1]
            exc = rave:::parse_selections(parsed[2])
            bad = rave:::parse_selections(parsed[3])
            epi = rave:::parse_selections(parsed[4])
            car = user_data$channels
            car = car[!car %in% c(bad, epi)]
            local_data$exc = exc
            local_data$bad = bad
            local_data$epi = epi
            local_data$car = car
            user_data$valid_channels = car
            return(NULL)
          }
        }
        local_data$plan = NULL
        local_data$exc = NULL
        local_data$bad = NULL
        local_data$epi = NULL
        local_data$car = NULL
        user_data$valid_channels = NULL
      })
      observeEvent(input$car_load, {
        nchls = length(user_data$channels)
        nblocks = length(user_data$blocks)
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = 'Loading in progress',
                     detail = 'This may take a while...')
        # load signals
        if(!is.null(user_data$subject)){
          pre_dir = user_data$subject$dirs$preprocess_dir
          cache_file = file.path(pre_dir, 'compressed_signals.h5')
          if(file.exists(cache_file)){
            for(b in user_data$blocks){
              progress$inc(1/nblocks, detail = sprintf('Block - %s', b))
              local_data$s_5[[b]] = h5read(cache_file, sprintf('/notch/compress5/%s', b))
              local_data$s_20[[b]] = h5read(cache_file, sprintf('/notch/compress20/%s', b))
            }
          }else{
            showNotification(p('You are running CAR for the first time. Caching data will take about 3 minutes.'), type = 'message')
            for(ii in 1:nchls){
              chl = user_data$channels[ii]
              progress$inc(1/nchls, detail = sprintf('Channel - %d', chl))
              for(b in user_data$blocks){
                fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
                # original signal
                s = h5read(fname, name = sprintf('/notch/compress5/%s', b))
                if(is.null(local_data$s_5[[b]])){
                  local_data$s_5[[b]] = matrix(rep(NA, nchls * length(s)), nrow = nchls)
                }
                local_data$s_5[[b]][ii, ] = s
              }
              for(b in user_data$blocks){
                fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
                # original signal
                s = h5read(fname, name = sprintf('/notch/compress20/%s', b))
                if(is.null(local_data$s_20[[b]])){
                  local_data$s_20[[b]] = matrix(rep(NA, nchls * length(s)), nrow = nchls)
                }
                local_data$s_20[[b]][ii, ] = s
              }
            }
            # save
            for(b in user_data$blocks){
              rave:::save_h5(
                x = local_data$s_5[[b]],
                file = cache_file,
                name = sprintf('/notch/compress5/%s', b),
                chunk = c(1, 1024)
              )
              rave:::save_h5(
                x = local_data$s_20[[b]],
                file = cache_file,
                name = sprintf('/notch/compress20/%s', b),
                chunk = c(1, 1024)
              )
            }
          }


          local_data$loaded = TRUE

        }

      })

      update_pressed <- observeEvent(input$update, {
        save = FALSE
        epi = rave:::parse_selections(input$car_epi)
        if(!setequal(user_data$epichan, epi)){
          user_data$epichan = epi
          save = T
        }
        bad = rave:::parse_selections(input$car_bad)
        if(!setequal(user_data$badchan, bad)){
          user_data$badchan = bad
          save = T
        }
        exc = rave:::parse_selections(input$car_exc)
        if(!setequal(user_data$exclchan, exc)){
          user_data$exclchan = exc
          save = T
        }
        if(save){
          user_data$force_save = Sys.time()
        }
      })
      output$parallel_plot <- renderPlot({
        validate(need(length(local_data$s_20) > 0, ''))
        start = input$start_time
        local_data$start_time = start
        duration = input$duration
        block = input$current_block
        plan = input$car_plan_choice

        if(!is.null(plan)){
          car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
          if(plan %in% names(car_plans)){
            if(duration < 5){
              m = car_plans[[plan]][['m_5']][[block]]
            }else{
              m = car_plans[[plan]][['m_20']][[block]]
            }
          } else{
            plan = NULL
          }
        }

        if(duration < 5){
          srate = isolate(user_data$srate) / 5
          ind = seq(ceiling(start * srate), floor(srate * (start + duration)))
          signals = local_data$s_5[[block]][, ind]

        }else{
          srate = isolate(user_data$srate) / 20
          ind = seq(ceiling(start * srate), floor(srate * (start + duration)))
          signals = local_data$s_20[[block]][, ind]
        }

        col = rep(1, nrow(signals))
        col[user_data$badchan] = 2
        col[user_data$epichan] = 3
        col[user_data$exclchan] = 4
        channel_names = paste(1:nrow(signals))

        if(!is.null(plan)){
          m = m[ind]
          zeros = rep(0, dim(signals)[2])
          signals = rbind(m, zeros, signals)
          col = c(5, 0, col)
          channel_names = c('Avg', '', channel_names)
        }
        if(input$space == 0){
          space = rave:::plot_signals(
            signals, srate, start_time = start, space = 0.99999, col = col, channel_names = channel_names
          )
          local_data$space = space
        }else{
          rave:::plot_signals(
            signals, srate, start_time = start, space = input$space, col = col, channel_names = channel_names
          )
          local_data$space = input$space
        }
      })
      observeEvent(input$pp_clicked, {
        if(local_data$space <= 0){
          local_data$click_msg = 'Click on the plot.'
        }else{
          e = input$pp_clicked
          ind = ceiling(e$y / local_data$space - 0.5)
          if(!is.null(input$car_plan_choice) && length(local_data$car_plan) > 0){
            ind = ind - 2
            if(ind <= 0){
              ind = 'Average Signal'
            }
          }
          time = e$x
          local_data$click_msg = HTML(sprintf(
            'You have clicked <br />Channel: %s <br />Time: %.2f sec', ind, time))
        }
      })

      observeEvent(input$calc, {
        # copied from "observeEvent(input$update, {..."
        save = FALSE
        epi = rave:::parse_selections(input$car_epi)
        if(!setequal(user_data$epichan, epi)){
          user_data$epichan = epi
          save = T
        }
        bad = rave:::parse_selections(input$car_bad)
        if(!setequal(user_data$badchan, bad)){
          user_data$badchan = bad
          save = T
        }
        exc = rave:::parse_selections(input$car_exc)
        if(!setequal(user_data$exclchan, exc)){
          user_data$exclchan = exc
          save = T
        }
        if(save){
          user_data$force_save = Sys.time()
        }
        # calculate
        pre_dir = user_data$subject$dirs$preprocess_dir
        name = sprintf('Exc[%s]Bad[%s]Epi[%s]',
                       rave:::deparse_selections(user_data$exclchan),
                       rave:::deparse_selections(user_data$badchan),
                       rave:::deparse_selections(user_data$epichan))
        invalidchans = c(user_data$badchan, user_data$exclchan, user_data$epichan)
        goodchan = user_data$channels[!user_data$channels %in% invalidchans]
        m_5 = list()
        m_20 = list()
        for(b in names(local_data$s_5)){
          m_5[[b]] = rbind(colMeans(local_data$s_5[[b]][goodchan, , drop = FALSE]))
          m_20[[b]] = rbind(colMeans(local_data$s_20[[b]][goodchan, , drop = FALSE]))
        }
        # save cache
        car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
        car_plans[[name]] = list(
          m_5 = m_5,
          m_20 = m_20
        )
        user_data$subject$cacher$save(
          car_plans = car_plans
        )

        local_data$new_plan = name
        local_data$car_plan = unique(c(isolate(local_data$car_plan), name))
        # updateSelectInput(session,)
        showNotification(p('Ready.'), type = 'message')
      })

      observeEvent(input$ok, {
        plan = input$car_plan_choice
        parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
        parsed = as.vector(parsed)
        name = parsed[1]
        exc = rave:::parse_selections(parsed[2])
        bad = rave:::parse_selections(parsed[3])
        epi = rave:::parse_selections(parsed[4])

        validchans = user_data$channels
        validchans = validchans[!validchans %in% c(exc, bad, epi)]
        progress = Progress$new()
        on.exit(progress$close())
        progress$set(message = 'Common Average Reference',
                     detail = 'This may take a while...')
        rave:::pre_car2(
          project_name = user_data$project_name,
          subject_code = user_data$subject_code,
          blocks = user_data$blocks,
          validchans = validchans,
          exclchan = exc, progress = progress
        )
        showNotification(p('CAR Completed.'), type = 'message')
        shiny::removeModal()
        user_data$subject$logger$save(CAR_plan = plan)
        local_data$refresh_post = Sys.time()
      })

      observeEvent(input$car_execute, {
        # apply CAR
        error = FALSE
        modal = NULL
        # step 1: parse selection
        plan = input$car_plan_choice
        if(is.null(plan)){
          error = TRUE
          modal = shiny::modalDialog(
            title = 'Error',
            p("You must choose a strategy to apply CAR"),
            easyClose = T
          )
        }else{
          parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
          if(sum(is.na(parsed)) > 0){
            error = TRUE
            modal = shiny::modalDialog(
              title = 'Fatal',
              p("Invalid Strategy! This shouldn't happen. Please remove the following file manually and try again. ",
                " If this error still occurs, contact author."),
              tags$blockquote(
                tools::file_path_as_absolute(file.path(user_data$subject$dirs$preprocess_dir,
                                                       'rave.RData'))),
              easyClose = T
            )
          }else{
            c_plan = user_data$subject$logger$get_or_save('CAR_plan')
            if(length(c_plan) > 0 && plan == c_plan){
              modal = shiny::modalDialog(
                title = 'Message',
                p("Your current CAR strategy is:"),
                tags$blockquote(plan),
                p("There's no need to re-run CAR"),
                easyClose = T
              )
            }else{
              parsed = as.vector(parsed)
              name = parsed[1]
              exc = rave:::parse_selections(parsed[2])
              bad = rave:::parse_selections(parsed[3])
              epi = rave:::parse_selections(parsed[4])
              modal = shiny::modalDialog(
                title = 'Confirmation',
                p('Please confirm the following information: '),
                tags$ul(
                  tags$li(strong('Excluded Channels: ', parsed[2])),
                  tags$li(strong('Bad Channels: ', parsed[3])),
                  tags$li(strong('Epilepsy Channels: ', parsed[4]))
                ),
                p(tags$small('* Note: All channels not listed above will be included into CAR calculation. ',
                             'Excluded channels will be excluded from CAR but will be counted as "valid" channels.')),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(ns("ok"), "Run CAR")
                )
              )
            }

          }
        }
        if(!is.null(modal)){
          shiny::showModal(modal)
        }
      })

      output$inner_ui4 <- renderUI({
        validate(need(!is.null(local_data$plan), "Please apply CAR first"))
        tagList(
          h4('Current Strategy'),
          tags$small(local_data$plan),
          selectInput(ns('current_block2'), 'Block:', choices = user_data$blocks),
          selectInput(ns('current_channel2'), 'Channel:', choices = local_data$car),
          div(
            actionButton(ns('prev2'), 'Previous'),
            actionButton(ns('nxt2'), 'Next')
          ),
          hr(),
          actionButton(ns('export'), 'Export Plots')
        )
      })
      observeEvent(input$prev2, {
        c_c = as.integer(input$current_channel2)
        a_c = as.integer(local_data$car)
        r_c = a_c[a_c < c_c]
        if(length(r_c)){
          updateSelectInput(session, 'current_channel2', selected = max(r_c))
        }
      })
      observeEvent(input$nxt2, {
        c_c = as.integer(input$current_channel2)
        a_c = as.integer(local_data$car)
        r_c = a_c[a_c > c_c]
        if(length(r_c)){
          updateSelectInput(session, 'current_channel2', selected = min(r_c))
        }
      })

      output$post_car_plot <- renderPlot({
        c_b = input$current_block2
        c_c = as.integer(input$current_channel2)

        validate(
          need(!is.null(local_data$plan), "Please apply CAR first"),
          need(length(c_b) == 1 && length(c_c) == 1, '')
        )

        if(c_c %in% local_data$exc){
          col = 'blue'
        }else{
          col = 'black'
        }
        rave:::pre_plot_car(
          user_data$project_name, user_data$subject_code, c_b, c_c, cex = 2, col = col
        )
      })

      observeEvent(input$export, {
        showNotification(p('Plots are being saved to ', user_data$subject$dirs$preprocess_dir), type = 'message')
        logger('Plots are being saved to ', user_data$subject$dirs$preprocess_dir, level = 'INFO')

        rave:::save_car_plots(
          project_name = user_data$project_name,
          subject_code = user_data$subject_code,
          blocks = user_data$blocks,
          chls = local_data$car,
          srate = user_data$srate,
          cex = 2,
          plan = local_data$plan
        )
      })

    }

    return(list(body = body, server = server))
  }

  rave_pre_wavelet <- function(module_id = 'WAVELET_M'){
    ns = shiny::NS(module_id)
    gp = global_panel(ns)

    body = fluidRow(
      shinydashboard::tabBox(
        id = ns('sidebar'),
        width = sidebar_width,
        shiny::tabPanel(
          title = 'Wavelet',
          uiOutput(ns('wavelet_panel'))
        ),
        shiny::tabPanel(
          title = 'Post-Wavelet',
          uiOutput(ns('pw_panel'))
        )
      ),
      shinydashboard::box(
        width = 12 - sidebar_width,
        title = 'Post-Wavelet Inspection (Power & Phase)',
        h5('Power Plot'),
        plotOutput(ns('power_plot'), height = '35vh'),
        hr(),
        h5('Phase Plot (only display several frequencies)'),
        plotOutput(ns('phase_plot'), height = '70vh')
      )
    )

    server = function(input, output, session = shiny::getDefaultReactiveDomain(), user_data){
      local_data = reactiveValues(
        save = NULL,
        refresh = NULL,
        wave_finished = FALSE,
        last_wavelet = NULL,
        start = 0,
        phase = NULL,
        power = NULL,
        default_step = 6
      )
      gp$reactive_func(input = input, user_data = user_data, local_data = local_data, session = session)

      output$wavelet_panel <- renderUI({
        vc = user_data$valid_channels
        validate(need(length(vc) > 0, 'Please calculate CAR first.'))
        vc_txt = rave:::deparse_selections(vc)
        tagList(
          p('Here are the channels to be transformed:'),
          tags$blockquote(vc_txt),
          hr(),
          sliderInput(ns('freq_range'), 'Frequency Range (Hz):', value = c(2,200), step = 1, round = TRUE, min = 1, max = 300),
          numericInput(ns('freq_step'), 'Frequency Step Size (Hz): ', value = 2, step = 1, min = 1),
          numericInput(ns('wave_num'), 'Number of Wavelet Cycles: ', value = 7, step = 1, min = 1),
          numericInput(ns('target_srate'), 'Target Sample Rate', value = 100, min = 10, max = isolate(user_data$srate), step = 1),
          checkboxInput(ns('save_original'), 'Save Original (Not Recommended)', value = FALSE),
          numericInput(ns('ncores'), 'Parallel, Number of Cores:', value = future::availableCores(), min = 1, max = rave_options('max_worker'), step = 1),
          actionButton(ns('do_wavelet'), 'Run Wavelet'),
          p(tags$small('Wavelet will run several hours.'))
        )
      })

      observe({
        local_data$refresh
        if(!is.null(user_data$subject)){
          last_wavelet = user_data$subject$logger$get_or_save('last_wavelet')
          if(!is.null(last_wavelet)){
            wave_succeed = last_wavelet[['succeed']]
            if(!is.null(wave_succeed) && wave_succeed){
              local_data$wave_finished = TRUE
              local_data$last_wavelet = last_wavelet
              local_data$start = 0
              return(NULL)
            }
          }
        }
        local_data$wave_finished = FALSE
        local_data$last_wavelet = NULL
        local_data$start = 0
      })

      output$pw_panel <- renderUI({

        validate(need(!is.null(user_data$subject), "Subject not Loaded"),
                 need(
                   !is.null(local_data$last_wavelet) &&
                   local_data$wave_finished, "Wavelet not started/finished."))
        # user_data$subject$logger$save(last_wavelet = list(
        #   channels = user_data$valid_channels,
        #   car_plan = user_data$subject$logger$get_or_save('CAR_plan'),
        #   succeed = FALSE,
        #   target_srate = target_srate,
        #   frequencies = frequencies,
        #   wave_num = wave_num,
        #   save_original = save_original
        # ))

        l = local_data$last_wavelet
        tagList(
          p('Your current wavelet settings are:'),
          tags$ul(
            tags$li('Channels: ', rave:::deparse_selections(l$channels)),
            tags$li('CAR Strategy: ', l$car_plan),
            tags$li('Frequencies: ', paste(l$frequencies[1:2], collapse = ', '), ', ...'),
            tags$li('Wavelet Cyles: ', l$wave_num)
          ),
          hr(),
          selectInput(ns('c_block'), 'Select a Block', choices = user_data$blocks),
          selectInput(ns('c_channel'), 'Channel', choices = l$channels),
          numericInput(ns('start'), 'Time', value = 0, min = 0, step = 1),
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next')
        )
      })

      observeEvent(input$do_wavelet, {
        modal = shiny::modalDialog(
          title = 'Confirmation',
          p('The following channels will be calculated: '),
          tags$blockquote(rave:::deparse_selections(user_data$valid_channels)),
          p('Current CAR strategy is: '),
          tags$blockquote(user_data$subject$logger$get_or_save('CAR_plan')),
          p('Ready?'),
          footer = tagList(
            actionButton(ns("wave_cacnel"), "Cancel"),
            actionButton(ns("ok"), "Sure!")
          )
        )
        showModal(modal)
      })

      observeEvent(input$wave_cacnel, {
        if(user_data$doing_wavelet){
          session$sendCustomMessage(
            type = 'alertmessage', message = str_c(
              "Wavelet is going in the background, I can't let you cancel this. ",
              "Usually it would take 10 min/channel. Just be patient, grab a cup ",
              "of coffee, go out and have a good lunch/dinner, or enjoy the sunshine... ",
              "Well, as long as your boss is happy about it :P"
            ))
        }else{
          removeModal()
        }
      })
      observeEvent(input$ok, {
        if(user_data$doing_wavelet){
          return(NULL)
        }
        target_srate = round(input$target_srate)
        target_srate = min(max(10, target_srate), user_data$srate)
        wave_num = max(1, round(input$wave_num))
        save_original = input$save_original
        ncores = max(1, round(input$ncores))
        frequencies = seq(input$freq_range[1],input$freq_range[2], by = input$freq_step)
        if(ncores == 1){
          # for debug
          fpl = future::sequential
        }else{
          fpl = future::multiprocess
        }
        if(length(frequencies)){
          showNotification(p('Wavelet started!'), type = 'message')
          a_c = user_data$channels
          plan = user_data$subject$logger$get_or_save('CAR_plan')[1]
          parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
          parsed = as.vector(parsed)
          name = parsed[1]
          exc = rave:::parse_selections(parsed[2])
          bad = rave:::parse_selections(parsed[3])
          epi = rave:::parse_selections(parsed[4])
          meta_elec = data.frame(
            Channel = a_c,
            EpilepsyChan = (a_c %in% epi),
            BadChan = (a_c %in% bad),
            ExcludedChan = (a_c %in% exc)
          )
          rave:::save_meta(
            data = meta_elec,
            meta_type = 'electrodes',
            project_name = user_data$project_name,
            subject_code = user_data$subject_code
          )
          user_data$doing_wavelet = TRUE
          rave:::bulk_wavelet(
            project_name = user_data$project_name,
            subject_code = user_data$subject_code,
            blocks = user_data$blocks,
            channels = user_data$valid_channels,
            srate = user_data$srate,
            target_srate = target_srate,
            frequencies = frequencies,
            wave_num = wave_num,
            compress = 2,
            replace = T,
            save_original = save_original,
            ncores = ncores,
            plan = fpl
          ) -> check
          # check = function(){return(rep(TRUE,4))}
          user_data$subject$logger$save(last_wavelet = list(
            channels = user_data$valid_channels,
            car_plan = user_data$subject$logger$get_or_save('CAR_plan'),
            succeed = FALSE,
            target_srate = target_srate,
            frequencies = frequencies,
            wave_num = wave_num,
            save_original = save_original
          ))
          longtimer_env$wavelet_check = function(){
            res = check()
            r = (unlist(res) != FALSE)
            if(sum(r) == length(r)){
              showNotification(p('Wavelet Finished!'), duration = NULL, type = 'message')
              last_wavelet = user_data$subject$logger$get_or_save('last_wavelet')
              last_wavelet[['succeed']] = TRUE
              user_data$subject$logger$save(last_wavelet = last_wavelet)
              local_data$refresh = Sys.time()
              rm('wavelet_check', envir = longtimer_env)
              removeModal()
              rave_setup()
              user_data$doing_wavelet = FALSE
            }
          }

        }else{
          showNotification(p('No valid frequencies found!'), type = 'error')
        }
      })

      observeEvent(input$prev, {
        s = max(0, isolate(local_data$start - local_data$default_step))
        updateNumericInput(session, 'start', value = s)
      })
      observeEvent(input$nxt, {
        s = isolate(local_data$start + local_data$default_step)
        updateNumericInput(session, 'start', value = s)
      })
      observe({
        if(!is.null(input$start)){
          local_data$start = max(0, floor(input$start))
        }
      })
      observe({
        b = input$c_block
        chl = as.integer(input$c_channel)
        dirs = get_dir(user_data$subject_code, user_data$project_name)
        cache_dir = dirs$cache_dir
        if(
          length(b) == 1 &&
          length(chl) == 1 &&
          !is.null(local_data$last_wavelet) &&
          file.exists(h5_file <- file.path(cache_dir, sprintf('%d.h5', chl))) &&
          TRUE # debug
        ){

          local_data$phase = h5read(h5_file, name = sprintf('/wavelet/phase/%s', b))
          local_data$power = h5read(h5_file, name = sprintf('/wavelet/power/%s', b))
          gc()
        }else{
          local_data$phase = NULL
          local_data$power = NULL
          gc()
        }
      })
      output$power_plot <- renderPlot({
        validate(
          need(!user_data$doing_wavelet, "Please wait till wavelet to be finished."),
          need(!is.null(local_data$power) && !is.null(local_data$last_wavelet), "Wavelet not started/finished.")
        )

        freq = local_data$last_wavelet$frequencies
        t = 0:local_data$default_step
        start = local_data$start
        sep = freq[2] - freq[1]
        ind = seq(1, local_data$default_step * local_data$last_wavelet$target_srate) +
          local_data$start * local_data$last_wavelet$target_srate
        data = local_data$power[, ind]
        data = apply(data, 1, function(x){
          (x / mean(x) - 1) * 100
        })
        {
          par(mar=c(5,4.5,4,7))
          image(data, axes=F, col=fields::tim.colors(64), ylab = 'Frequency', xlab = 'Time(s)')
          axis(2, at = (freq - min(freq) + sep / 2) / (max(freq) - min(freq) + 1), labels = paste(freq, 'Hz'))
          axis(1, at=t / max(t), labels=t + floor(start))

          fields::image.plot(data, legend.only=T, nlevel = 64)
        }
      })
      output$phase_plot <- renderPlot({
        validate(
          need(!user_data$doing_wavelet, "Please wait till wavelet to be finished."),
          need(!is.null(local_data$phase) && !is.null(local_data$last_wavelet), "Wavelet not started/finished.")
        )

        freq = local_data$last_wavelet$frequencies
        t = 0:local_data$default_step
        start = local_data$start
        ind = seq(1, local_data$default_step * local_data$last_wavelet$target_srate) +
          local_data$start * local_data$last_wavelet$target_srate
        f_ind = freq < 0.9 * local_data$last_wavelet$target_srate
        sep = max(round(sum(f_ind) / 10), 1)
        sub_ind = seq(1, sum(f_ind), by = sep)
        data = local_data$phase[sub_ind, ind]
        {

          rave:::plot_signals(data, sample_rate = local_data$last_wavelet$target_srate,
                              channel_names = paste(freq[sub_ind], 'Hz'), space = 7, start_time = start, ylab = 'Frequency')
        }
      })
    }

    return(list(
      body = body,
      server = server
    ))
  }
  # Init Modules
  OVERVIEW_MODUDLE <- rave_pre_overview()
  NOTCH_MODULE <- rave_pre_notch()
  CAR_MODULE <- rave_pre_car()
  WAVELET_MODULE <- rave_pre_wavelet()

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
                                   tabName = 'WAVELET')
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


