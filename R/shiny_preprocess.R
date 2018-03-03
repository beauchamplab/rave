#' @export
init_preprocess = function(){
  input_width = 3; main_width = 12 - input_width

  ui = shinyUI(ui = rave::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = 'RAVE Preprocess'
    ),
    control = dashboardControl(
      fluidRow(
        column(
          12,
          textInput('project_name', 'Project Name'),
          textInput('subject_code', 'Subject Code'),
          selectInput('blocks', 'Blocks', selected = NULL, choices = '', multiple = T),
          textInput('channels', 'Channels', placeholder = 'E.g. 1-84'),
          numericInput('srate', 'Sample Rate', value = 2000, step = 1, min = 1),
          textInput('exclchan', 'Excluded Channels', placeholder = 'E.g. 51,44-45'),
          textInput('badchan', 'Bad Channels', placeholder = 'E.g. 3,4,5,11-20'),
          textInput('epichan', 'Epilepsy Channels', placeholder = 'E.g. 1-10'),

          actionButton('save', 'Save Changes')
        )
      )
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
      fluidRow(
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            'OVERVIEW',
            shinydashboard::box(
              width = 5,
              title = 'Subject Info',
              collapsible = T,
              collapsed = F,
              uiOutput('subject_info')
            ),
            shinydashboard::box(
              width = 7,
              title = 'History',
              collapsible = T,
              collapsed = F,
              dataTableOutput('subject_log')
            )
          ),
          shinydashboard::tabItem(
            'NOTCH',
            shinydashboard::box(
              width = input_width,
              title = 'Notch Filter',
              textInput('notch_channels', 'Channels to Apply Notch Filter', value = '', placeholder = 'E.g. 1-80'),
              actionButton('notch', 'Apply Notch Filters'),

              hr(),h4('After Notch Filter'),
              selectInput('notch_blocks', 'Select a Block', choices = '', multiple = F),
              selectInput('notch_current_channel', 'Select a Channel for Visualization', choices = ''),
              checkboxGroupInput('notch_channel_cls', 'Mark This Channel as - ',
                                 choiceNames = c('Excluded from CAR', 'Bad Channel', 'Epilepsy Channel'),
                                 choiceValues = c('GE', 'B', 'E')),
              p(tags$small('Any channels marked as a "bad" or "epilepsy" channel will be ignored.')),
              div(
                actionButton('notch_prev', 'Previous'),
                actionButton('notch_next', 'Next')
              )

            ),
            column(width = main_width, uiOutput('notch_channel_plot'))
          ),
          shinydashboard::tabItem(
            'CAR',
            shinydashboard::box(
              width = input_width,
              title = 'Common Average Referencing',
              h4('Before CAR'),
              p(
                'Please enter bad channels, epilepsy channels, and excluded channels on the right panel. ',
                'These three channels will be ignored when calculating CAR baseline. However, "Excluded channels" ',
                'will be counted as valid channels'
              ),

              fluidRow(
                column(8, textInput('car_name', 'Name', value = '', placeholder = 'Channel_1-80')),
                column(4, actionButton('car_avg_run', 'Calculate'))
              ),
              p('- Then -'),
              selectInput('car_plans', 'Select your history strategies', selected = '',
                          choices = '', multiple = TRUE),

              selectInput('car_blocks', 'Block Number', choices = ''),
              sliderInput('car_time', 'Time range', value = 0, min = 0, max = 300, step = 0.1),
              numericInput('car_duration', 'Set Duration Manually', value = 1, min = 0.1, max = 500, step = 0.1),



              div(
                actionButton('car_prev', 'Previous'),
                actionButton('car_next', 'Next')
              ),


              hr(), h4('Apply CAR'),
              selectInput('car_choice', 'Select a Plan', selected = NULL, choices = ''),
              textOutput('car_choice_details'),
              actionButton('car_run', 'Apply CAR')

            ),
            column(
              width = main_width,
              shinydashboard::box(
                width = 12,
                title = 'CAR Inspect',
                div(
                  class = 'ratio5to4',
                  plotOutput('car_parallel_plot')
                ),
                plotOutput('car_legend', height = '40px')
              ),
              shinydashboard::box(
                width = 12,
                title = 'Average Signals',
                plotOutput('car_avg_plot', height = '200px')
              )
            )
          ),
          shinydashboard::tabItem(
            'WAVELET',
            shinydashboard::box(
              width = input_width,
              title = 'Wavelet Transformation',

              h4('Before Wavelet - Channel inspection'),
              selectInput('wave_blocks', 'Select a Block', choices = '', multiple = F),
              selectInput('wave_current_channel', 'Select a Channel for Visualization', choices = ''),
              # checkboxGroupInput('wave_channel_cls', 'Mark This Channel as - ',
              #                    choiceNames = c('Excluded from CAR', 'Bad Channel', 'Epilepsy Channel'),
              #                    choiceValues = c('GE', 'B', 'E')),
              # p(tags$small("Re-execute CAR is highly recommended if any changes are made here")),
              div(
                actionButton('wave_prev', 'Previous'),
                actionButton('wave_next', 'Next')
              ),
              hr(), h4('Wavelet'),
              textInput('wavelet_channels', 'Enter channels for wavelet:', placeholder = 'E.g. 1-5'),
              actionButton('wavelet_run', 'Run')
            ),
            column(
              width = main_width,
              fluidRow(
                shinydashboard::box(
                  width = 12,
                  collapsible = T,
                  title = 'Channel Inspection',
                  uiOutput('wave_channel_plot')
                )
              )
            )

          )
        )
      )
    ),
    skin = 'purple', title = 'R Analysis and Visualization of ECOG Data',
    controlbar_opened = TRUE
  ))


  server = shinyServer(func = function(input, output, session){
    # Def of Interactive variables
    user_data = shiny::reactiveValues(
      refresh = 1,
      save = NULL,
      project_name = '',
      subject_code = '',  # also folder name
      blocks = '',        # all blocks
      block_num = '',     # current block to handle
      channels = NULL,    # all channels
      chl_num = 1,       # current channel to handle,
      srate = 2000,
      epichan = NULL,
      badchan = NULL,
      exclchan = NULL,
      subject = NULL,
      parallel_signals = NULL,
      valid_chan = NULL
    )

    tmp_data = new.env()
    tmp_data$.car_params = list()

    update_inputchannels <- function(inputId){
      switch (inputId,
              'epichan' = 'EpilepsyChan',
              'badchan' = 'BadChan',
              'exclchan' = 'ExcludedChan'
      ) ->
        colname
      .s = user_data$subject
      if(!is.null(.s) && length(colname) == 1 && is.data.frame(.s$channel_info) && nrow(.s$channel_info) > 0){
        info = .s$channel_info
        chls = info$Channel[info[, colname]]
        text = rave:::deparse_selections(as.numeric(chls))
        updateTextInput(session, inputId, value = text)
      }
    }

    load_tmp_data <- function(force = FALSE){
      if(!is.null(user_data$subject) && 'notch_filtered' %in% names(user_data$subject$conf)){
        if(force || !exists('.all_signals', envir = tmp_data)){
          if(force){
            rm(list = ls(tmp_data, all.names = F), envir = tmp_data)
          }
          logger('Loading TMP data')
          tmp_data$.car_params = list()
          car_avg(project_name = user_data$subject$project_name,
                  subject_code = user_data$subject$subject_code,
                  blocks = user_data$subject$blocks,
                  channels = 1:2, envir = tmp_data)
        }else{
          choices = names(tmp_data$.car_params)
          updateSelectInput(session, 'car_plans', choices = choices, selected = isolate(input$car_plans))
          updateSelectInput(session, 'car_choice', choices = choices)
        }
      }
    }

    observe({
      # load subject information
      tmp_subject = rave:::SubjectInfo$new(project_name = input$project_name,
                                           subject_code = input$subject_code)

      if(tmp_subject$valid){
        user_data$subject = tmp_subject

        shiny::showNotification('Subject Found! Trying to retrieve information...', type = 'message')
      }
    }, priority = 1000L)


    # Reactive observe (handlers)
    observeEvent(user_data$subject, {

      if(!is.null(user_data$subject)){
        tmp_subject = user_data$subject

        # load subject info
        # update block info
        blocks_all = list.dirs(tmp_subject$dirs$pre_subject_dir,
                               full.names = F, recursive = F)
        blocks = tmp_subject$blocks
        updateSelectInput(session, 'blocks', selected = blocks, choices = blocks_all)

        # update channel info if exists
        channels = tmp_subject$channels
        channels = rave:::deparse_selections(channels)
        updateTextInput(session, inputId = 'channels', value = channels)

        # update sample rate if exists
        srate = tmp_subject$srate
        if(!is.null(srate) && is.numeric(srate)){
          updateNumericInput(session, 'srate', value = srate)
        }


        # update bad, epi, excluded channel info if exists
        badchan = tmp_subject$badchan
        badchan = rave:::deparse_selections(badchan)
        updateTextInput(session, inputId = 'badchan', value = badchan)

        epichan = tmp_subject$epichan
        epichan = rave:::deparse_selections(epichan)
        updateTextInput(session, inputId = 'epichan', value = epichan)

        exclchan = tmp_subject$exclchan
        exclchan = rave:::deparse_selections(exclchan)
        updateTextInput(session, inputId = 'exclchan', value = exclchan)


        # Notch - Which channels have been filtered?
        notch_filtered = rave:::parse_selections(tmp_subject$conf$notch_filtered)
        notch_tobe_filtered = tmp_subject$channels[!tmp_subject$channels %in% notch_filtered]
        updateTextInput(session, 'notch_channels', value = rave:::deparse_selections(notch_tobe_filtered))
        user_data$refresh = isolate(user_data$refresh) + 1

        updateSelectInput(session, 'notch_current_channel', choices = notch_filtered)

        load_tmp_data(force = TRUE)

        shiny::showNotification(sprintf(
          'Subject %s loaded from project %s', tmp_subject$subject_code, tmp_subject$project_name
        ), type = 'message')

      }
    }, priority = 999L)

    observe({
      user_data$project_name = input$project_name
      user_data$subject_code = input$subject_code
      user_data$blocks = input$blocks
      user_data$channels = rave:::parse_selections(input$channels)
      user_data$srate = input$srate
      user_data$badchan = rave:::parse_selections(input$badchan)
      user_data$epichan = rave:::parse_selections(input$epichan)
      user_data$exclchan = rave:::parse_selections(input$exclchan)


      if(!is.null(user_data$subject)){
        user_data$subject$set_channels(rave:::parse_selections(input$epichan),
                                       name = 'epichan')
        user_data$subject$set_channels(rave:::parse_selections(input$badchan),
                                       name = 'badchan')
        user_data$subject$set_channels(rave:::parse_selections(input$exclchan),
                                       name = 'exclchan')
        user_data$subject$srate = input$srate
      }

      updateSelectInput(session, 'notch_blocks', choices = input$blocks)
      updateSelectInput(session, 'car_blocks', choices = input$blocks)
      updateSelectInput(session, 'wave_blocks', choices = input$blocks)

      user_data$refresh = isolate(user_data$refresh) + 1
    })

    observeEvent(user_data$save, {
      if(!is.null(user_data$subject) && length(user_data$save) > 0){
        logger('Saving subject data')
        info = user_data$save
        if(is.list(info)){
          user_data$subject$logger(.list = info)
        }
        # save subject
        user_data$subject$set_blocks(user_data$blocks)
        user_data$subject$set_channels(rave:::parse_selections(user_data$channels))

        user_data$subject$save()

        # also save tmp_data
        car_file = file.path(user_data$subject$dirs$preprocess_dir, 'car.RData')
        save(list = c(ls(all.names = F, envir = tmp_data), '.car_params'), file = car_file, envir = tmp_data)

        shiny::showNotification('Subject Saved!', type = 'message')
        user_data$refresh = isolate(user_data$refresh) + 1
      }
    })

    observeEvent(input$save, {
      user_data$save = list(MannualSave = str_c(Sys.time()))
    })

    observeEvent(input$blocks, {
      if(length(input$blocks) > 0 && str_trim(input$channels) == ''){
        first_block = input$blocks[1]

        # check channels
        block_dir = file.path(user_data$subject$dirs$pre_subject_dir, first_block)
        files = list.files(block_dir)

        channels = str_match(files, '[\\w]+_ch([0-9]+)\\.mat')[,2]
        channels = channels[!is.na(channels)]
        channels = as.integer(channels)
        channels = rave:::deparse_selections(channels, concatenate = F)
        if(length(channels) > 1){
          channels = channels[1]
        }
        updateTextInput(session, 'channels', value = channels)
      }
    })



    observeEvent(input$notch, {
      channels = rave:::parse_selections(input$notch_channels)
      if(length(channels) == 0){
        logger('No channel selected for notch', level = 'WARNING')
      }else{
        logger('Applying Notch filter, this may take a while', level = 'INFO')
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Notch Filter", value = 0, detail = 'Initializing...')

        rave:::bulk_notch(project_name = user_data$project_name,
                   subject_code = user_data$subject_code,
                   blocks = user_data$blocks,
                   channels = channels, srate = user_data$srate,
                   replace = F, save_plot = T, new_cache = F, progress = progress)

        notch_filtered = as.numeric(sort(unique(c(channels, user_data$subject$conf$notch_filtered))))
        user_data$subject$configure(notch_filtered = notch_filtered)
        user_data$save = list(NotchFilter = rave:::deparse_selections(channels))
        user_data$refresh = isolate(user_data$refresh) + 1
        updateSelectInput(session, 'notch_current_channel', choices = notch_filtered)
        updateTextInput(session, 'notch_channels', value = '')
      }
    })

    observeEvent(input$notch_current_channel, {
      chl = input$notch_current_channel
      .s = user_data$subject
      if(!is.null(.s) && !is.null(chl) && str_trim(chl) != '' && is.data.frame(.s$channel_info)){
        chl = as.integer(chl)
        info = .s$channel_info
        # assume channel exists and unique otherwise error should pop out
        info = info[info$Channel == chl, ]
        val = NULL
        if(info$BadChan) {val = c(val, 'B')}
        if(info$EpilepsyChan) {val = c(val, 'E')}
        if(info$ExcludedChan) {val = c(val, 'GE')}
        if(is.null(val)){
          val = character(0)
        }
        updateCheckboxGroupInput(session = session, 'notch_channel_cls', selected = val)
      }
    })

    observeEvent(input$notch_channel_cls, {
      chl = input$notch_current_channel
      .s = user_data$subject
      if(!is.null(.s) && !is.null(chl) && str_trim(chl) != '' && is.data.frame(.s$channel_info)){
        chl = as.integer(chl)
        info = .s$channel_info
        cls = input$notch_channel_cls
        info[info$Channel == chl, 'BadChan'] = ('B' %in% cls)
        info[info$Channel == chl, 'EpilepsyChan'] = ('E' %in% cls)
        info[info$Channel == chl, 'ExcludedChan'] = ('GE' %in% cls)
        .s$channel_info = info

        update_inputchannels('epichan')
        update_inputchannels('badchan')
        update_inputchannels('exclchan')
      }
    })

    observeEvent(input$notch_next, {
      chl = as.numeric(input$notch_current_channel)
      notch_filtered = user_data$subject$conf$notch_filtered
      if(length(notch_filtered) > 0){
        chl = notch_filtered[notch_filtered > chl]
        if(length(chl) > 0){
          updateSelectInput(session, 'notch_current_channel', selected = min(chl))
        }
      }
    })
    observeEvent(input$notch_prev, {
      chl = as.numeric(input$notch_current_channel)
      notch_filtered = user_data$subject$conf$notch_filtered
      if(length(notch_filtered) > 0){
        chl = notch_filtered[notch_filtered < chl]
        if(length(chl) > 0){
          updateSelectInput(session, 'notch_current_channel', selected = max(chl))
        }
      }
    })

    observeEvent(input$sidebar, {
      load_tmp_data()
    })


    observeEvent(input$car_avg_run, {
      car_name = input$car_name


      if(car_name == ''){
        car_name = strftime(x = Sys.time(), format = 'CAR_%Y%m%d%H%M%s')
      }
      chls = user_data$channels
      chls = chls[!chls %in% c(user_data$epichan, user_data$badchan, user_data$exclchan)]
      re = car_avg(project_name = user_data$project_name,
              subject_code = user_data$subject_code,
              blocks = user_data$blocks,
              chls, envir = tmp_data, name = car_name)

      showNotification(p('CAR average signal generated - ', car_name), type = 'message')

      load_tmp_data()
    })


    # outputs
    output$subject_info <- renderUI({
      validate(
        need(!is.null(user_data$subject), 'Please enter subject information.')
      )
      flag = user_data$refresh

      conf = user_data$subject$conf
      notch_filtered = rave:::deparse_selections(conf[['notch_filtered']])
      CAR_channels = rave:::deparse_selections(conf[['CAR_channels']])
      wavelet_channels = rave:::deparse_selections(conf[['wavelet_channels']])

      if(notch_filtered == ''){
        notch_filtered = '(Not Started)'
      }
      if(CAR_channels == ''){
        CAR_channels = '(Not Started)'
      }
      if(wavelet_channels == ''){
        wavelet_channels = '(Not Started)'
      }

      tagList(
        p(
          h3('Basic Information'), hr(),
          tags$dl(
            tags$dt('Project Name'),tags$dd(conf$project_name),
            tags$dt('Subject Code'),tags$dd(conf$subject_code),
            tags$dt('All Blocks'),tags$dd(str_c(conf$blocks, collapse = ', ')),
            tags$dt('All Channels'),tags$dd(rave:::deparse_selections(conf$channels))
          )
        ),
        p(
          h3('Channel Settings'), hr(),
          tags$dl(
            tags$dt('Excluded Channels'),tags$dd(rave:::deparse_selections(conf$exclchan)),
            tags$dt('Bad Channels'),tags$dd(rave:::deparse_selections(conf$badchan)),
            tags$dt('Epilepsy Channels'),tags$dd(rave:::deparse_selections(conf$epichan))
          )
        ),
        p(
          h3('Preprocess Procedure'), hr(),
          tags$dl(
            tags$dt('Notch Filter'),tags$dd(notch_filtered),
            tags$dt('CAR'),tags$dd(CAR_channels),
            tags$dt('Wavelet'),tags$dd(wavelet_channels)
          )
        )
      )
      # cat('Subject Info\n')
      #
      #
      # cat('Project Name\t\t', conf$project_name, '\n')
      # cat('Subject Code\t\t', conf$subject_code, '\n')
      # cat('All Blocks\t\t', conf$blocks, '\n')
      # cat('All Channels\t\t', rave:::deparse_selections(conf$channels), '\n')
      # cat('Excluded Channels\t', rave:::deparse_selections(conf$exclchan), '\n')
      # cat('Bad Channels\t\t', rave:::deparse_selections(conf$badchan), '\n')
      # cat('Epilepsy Channels\t', rave:::deparse_selections(conf$epichan), '\n')

      # cat('\nPreprocess:\n')
      # cat('Notch Filter:\t', notch_filtered, '\n')
      # cat('CAR:\t\t', CAR_channels, '\n')
      # cat('Wavelet:\t', wavelet_channels, '\n')

    })

    output$subject_log <- renderDataTable({
      validate(
        need(!is.null(user_data$subject), 'Please enter subject information.')
      )
      flag = user_data$refresh
      log = user_data$subject$log
      log
    })

    output$notch_channel_plot <- renderUI({
      .s = user_data$subject
      blocks = input$notch_blocks
      chl = as.integer(input$notch_current_channel)
      validate(
        need(!is.null(.s), 'Not Subject Detected'),
        need(!is.null(blocks) && !is.na(blocks), 'No Block Selected for Visualization'),
        need(!is.null(chl) && !is.na(chl) && length(chl) == 1, 'No Channel Selected for Visualization')
      )

      vis_dir = .s$dirs$pre_visual_dir
      files = list.files(vis_dir, all.files = T, full.names = F, recursive = F)


      fluidRow(
        lapply(blocks, function(block_num){
        f = sprintf('notch_%s_ch_%d.png', block_num, chl)
        if(f %in% files){
          src =  file.path(vis_dir, f)
          return(
            shinydashboard::box(
              title = sprintf('Block - %s', block_num),
              collapsible = F,
              width = 12,
              tags$img(src = session$fileUrl(file = src), width = '100%')
            )
          )
        }else{
          shinydashboard::box(
            title = sprintf('Block - %s (Missing Plot)', block_num),
            collapsible = F,
            width = 12,
            ''
          )
        }
      }))


    })

    observeEvent(input$car_prev, {
      tr = isolate(input$car_time)
      tr = max(0, tr - input$car_duration)
      updateSliderInput(session, 'car_time', value = tr)
    })

    observeEvent(input$car_next, {
      tr = isolate(input$car_time)
      tr = tr + input$car_duration
      updateSliderInput(session, 'car_time', value = tr)
    })

    observe({
      block = input$car_blocks
      r = user_data$refresh

      if(input$sidebar == 'CAR' && exists('.all_signals', envir = tmp_data) && block %in% names(tmp_data$.all_signals)){
        ss = tmp_data$.all_signals[[block]]
        srate = 1000
        max = round(ncol(ss) / srate, 2)
        logger('Block ', block, ' - ', max, ' Seconds.')
        updateSliderInput(session = session, 'car_time', max = max)
      }
    })

    output$car_avg_plot <- renderPlot({
      block = input$car_blocks
      start = input$car_time
      time_span = input$car_duration
      srate = 1000
      plans = input$car_plans


      validate(
        need(length(plans) > 0, 'No Strategy Chosen')
      )
      ss = NULL
      all_channels = NULL

      for(cname in plans){
        if(exists(cname, envir = tmp_data)){
          cs = get(cname, envir = tmp_data)
          if(block %in% names(cs)){
            cs = cs[[block]]

            ind = 1:length(cs)
            cs = cs[(ind >= start * srate) & (ind <= (start + time_span) * srate)]

            ss = rbind(ss, cs)
            all_channels = c(all_channels, cname)
          }
        }
      }

      if(!is.null(all_channels) && is.matrix(ss)){
        if(dim(ss)[1] == 1){
          ss = rbind(
            ss, rep(0, length(ss))
          )
          all_channels = c(all_channels, '')
          col = c(TRUE, FALSE)
        }else{
          col = 1:length(all_channels)
        }
        if(time_span >250){
          # faster way?
          ss = t(apply(ss, 1, rave::decimate_fir, q = 100))
          srate = srate / 100
        }else if(time_span > 50){
          ss = t(apply(ss, 1, rave::decimate_fir, q = 10))
          srate = srate / 10
        }

        par(mar = c(3, 1,1,1))
        plot_signals(
          ss, sample_rate = srate, space = 0.99999, plot = 'base',
          channel_names = all_channels,
          col = col
        )
      }
    })




    output$car_choice_details <- renderText({
      car_plan = input$car_choice
      validate(
        need(length(car_plan) == 1 && car_plan!='', 'Generate a stratege to apply CAR.')
      )
      valid_channels = tmp_data$.car_params[[car_plan]]
      text = rave:::deparse_selections(valid_channels)
      text = sprintf('Valid channels:\n%s', text)
      return(text)
    })

    output$car_legend <- renderPlot({
      par(mar = c(0,1,0,1))
      plot(1:10, type = 'n', axes =FALSE, ann = F)
      legend('top', col = c(1,2,3,4,5), lty = 1, y = c(
        'Averaged signal', 'Good channels', 'Good, but excluded channels', 'Epilepsy channels', 'Bad channels'
      ), horiz = T, cex = 0.7)
    })

    output$car_parallel_plot <- renderPlot({

      block = input$car_blocks
      start = input$car_time
      time_span = input$car_duration
      srate = 1000

      all_channels = user_data$channels
      badchan = user_data$badchan
      epichan = user_data$epichan
      exclchan = user_data$exclchan

      col = 2 + (all_channels %in% exclchan) + 2 * (all_channels %in% epichan) + 3 * (all_channels %in% badchan)


      ss = tmp_data$.all_signals[[block]]
      user_data$parallel_signals = ss
      # Additional signals
      plans = input$car_plans
      for(cname in plans){
        if(exists(cname, envir = tmp_data)){
          cs = get(cname, envir = tmp_data)
          if(block %in% names(cs)){
            cs = cs[[block]]
            if(length(cs) == ncol(ss)){
              col = c(col, 1)
              ss = rbind(ss, cs)
              all_channels = c(all_channels, cname)
            }
          }
        }
      }



      ind = 1:ncol(ss)
      ss = ss[, (ind >= start * srate) & (ind <= (start + time_span) * srate)]

      if(time_span >250){
        # faster way?
        ss = t(apply(ss, 1, rave::decimate_fir, q = 100))
        srate = srate / 100
      }else if(time_span > 50){
        ss = t(apply(ss, 1, rave::decimate_fir, q = 10))
        srate = srate / 10
      }

      par(mar = c(3, 1,1,1))
      plot_signals(
        ss, sample_rate = srate, space = 0.99999, plot = 'base',
        channel_names = all_channels,
        col = col
      )
    })



    observeEvent(input$car_run, {
      # check and show modal
      # adjust channel selections
      plan = input$car_choice
      channels = tmp_data$.car_params[[plan]]
      all_channels = user_data$channels

      epichan = user_data$epichan
      badchan = user_data$badchan
      exclchan = user_data$exclchan

      has_epi = sum(channels %in% epichan) > 0
      has_bad = sum(channels %in% badchan) > 0
      has_exc = sum(channels %in% exclchan) > 0
      not_exc = sum(!all_channels %in% c(channels, exclchan, badchan, epichan)) > 0

      epi_input = input$epichan
      bad_input = input$badchan
      exc_input = input$exclchan

      warns = NULL

      if(has_epi){
        old_epi_input = epi_input
        epichan = epichan[!epichan %in% channels]
        epi_input = rave:::deparse_selections(epichan)
        warns = c(warns, (sprintf('Epilepsy channels <label>%s</label> to <label>%s</label>', old_epi_input, epi_input)))
      }
      if(has_bad){
        old_bad_input = bad_input
        badchan = badchan[!badchan %in% channels]
        bad_input = rave:::deparse_selections(badchan)
        warns = c(warns, (sprintf('Bad channels: <label>%s</label> to <label>%s</label>', old_bad_input, bad_input)))
      }
      if(has_exc || not_exc){
        old_exc_input = exc_input
        exclchan = all_channels[!all_channels %in% c(channels, badchan, epichan)]
        exc_input = rave:::deparse_selections(exclchan)
        warns = c(warns, (sprintf('Channels excluded from CAR: <label>%s</label> to <label>%s</label>', old_exc_input, exc_input)))
      }


      run_car_func <- function(){
        # channels is needed for CAR baseline
        # c(channels, exclchan) will be calculated
        progress = shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "CAR", value = 0, detail = 'Initializing...')

        bulk_CAR(
          user_data$project_name,
          user_data$subject_code,
          user_data$blocks,
          c(channels, exclchan),
          user_data$srate, excluded = exclchan,
          progress = progress
        )

        text = rave:::deparse_selections(channels)
        user_data$subject$configure(CAR_channels = channels, Excluded_channels = exclchan)
        user_data$save = list(CAR_channels = text, Excluded_channels = exc_input)
      }


      inputId = sprintf('MODAL_%.0f', runif(1) * 1000)
      if(!is.null(warns)){
        messageModal(
          inputId = inputId,
          title = 'Warnings',
          msg = 'Your selected channels for CAR conflict with current input. Press "Cancel" to revise your selection on the right panel, or press "OK" if you accept the following changes and proceed to CAR: ',
          details = tags$ul(lapply(warns, function(s){tags$li(HTML(s))})),
          type = 'danger'
        )
        local({
          observeEvent(input[[inputId]],{
            removeModal()
            if(has_epi){
              updateTextInput(session, 'epichan', value = epi_input)
            }
            if(has_bad){
              updateTextInput(session, 'badchan', value = bad_input)
            }
            if(has_exc || not_exc){
              updateTextInput(session, 'exclchan', value = exc_input)
            }
            run_car_func()
          }, once = T)
        })
      }else{
        messageModal(
          inputId = inputId,
          title = 'Ready',
          msg = 'Everything is ready. Press "OK" to proceed.',
          details = NULL,
          type = 'success'
        )
        local({
          observeEvent(input[[inputId]],{
            removeModal()
            run_car_func()
          }, once = T)
        })
      }

    })

    observe({
      if(!is.null(user_data$subject)){
        good_chan = user_data$subject$conf[['CAR_channels']]
        excl_chan = user_data$subject$conf[['Excluded_channels']]
        valid_chan = c(good_chan, excl_chan)
        user_data$valid_chan = valid_chan
        if(length(valid_chan) > 0){
          updateSelectInput(session, 'wave_current_channel', choices = valid_chan)
        }
      }
    })

    observeEvent(input$wave_prev, {
      cc = input$wave_current_channel
      vc = user_data$valid_chan
      vc= vc[vc < cc]
      if(length(vc) > 0){
        updateSelectInput(session, 'wave_current_channel', selected = max(vc))
      }
    })
    observeEvent(input$wave_next, {
      cc = input$wave_current_channel
      vc = user_data$valid_chan
      vc= vc[vc > cc]
      if(length(vc) > 0){
        updateSelectInput(session, 'wave_current_channel', selected = min(vc))
      }
    })


    output$wave_channel_plot <- renderUI({
      .s = user_data$subject
      blocks = input$wave_blocks
      chl = as.integer(input$wave_current_channel)
      validate(
        need(!is.null(.s), 'Not Subject Detected'),
        need(!is.null(blocks) && !is.na(blocks), 'No Block Selected for Visualization'),
        need(!is.null(chl) && !is.na(chl) && length(chl) == 1, 'No Channel Selected for Visualization')
      )

      # update
      # val = NULL
      # if(chl %in% isolate(user_data$badchan)){
      #   val = c(val, 'B')
      # }
      # if(chl %in% isolate(user_data$epichan)){
      #   val = c(val, 'E')
      # }
      # if(chl %in% isolate(user_data$exclchan)){
      #   val = c(val, 'GE')
      # }
      # if(length(val) > 0){
      #   updateCheckboxGroupInput(session, 'wave_channel_cls', selected = val)
      # }

      # Visualiza

      vis_dir = .s$dirs$pre_visual_dir
      files = list.files(vis_dir, all.files = T, full.names = F, recursive = F)


      fluidRow(
        lapply(blocks, function(block_num){
          f = sprintf('CAR_%s_ch_%d.png', block_num, chl)
          if(f %in% files){
            src =  file.path(vis_dir, f)
            return(
              shinydashboard::box(
                title = sprintf('Block - %s', block_num),
                collapsible = F,
                width = 12,
                tags$img(src = session$fileUrl(file = src), width = '100%')
              )
            )
          }else{
            shinydashboard::box(
              title = sprintf('Block - %s (Missing Plot)', block_num),
              collapsible = F,
              width = 12,
              ''
            )
          }
        }))


    })


    # textInput('wavelet_channels', 'Enter channels for wavelet:', placeholder = 'E.g. 1-5,10'),
    # actionButton('wavelet_run', 'Run')
    observeEvent(input$wavelet_run, {
      # check channels
      wc = rave:::parse_selections(input$wavelet_channels)
      is_valid = wc %in% user_data$valid_chan
      vc = wc[is_valid]

      inputId = sprintf('MODAL_%.0f', runif(1) * 1000)
      if(sum(!is_valid) > 0){
        # there are some channels that are bad or epi
        messageModal(inputId, 'Warning',
                     msg = 'Your input contains bad or epilepsey channels. Only the following channels will be transformed: ',
                     details = tags$ul(
                       tags$li(HTML('Your input: <label>', input$wavelet_channels, '</label>')),
                       tags$li(HTML('Channels to be tranformed: <label>', rave:::deparse_selections(vc), '</label>'))
                     ),
                     type = 'danger')
      }else{
        messageModal(inputId, 'Ready',
                     msg = 'Ready. The following channels are in the queue. Press "OK" to proceed.',
                     details = tags$ul(
                       tags$li(HTML('Channels to be tranformed: <label>', rave:::deparse_selections(vc), '</label>'))
                     ),
                     type = 'success')
      }

      local({
        observeEvent(input[[inputId]], {
          removeModal()
          if(length(vc) > 0){
            rave:::bulk_wavelet(
              user_data$subject$project_name,
              user_data$subject$subject_code,
              user_data$subject$blocks,
              vc, user_data$subject$srate,
              ncores = rave_options('max_worker'), plan = future::multisession
            ) ->
              check

            showNotification('Wavelet transformation is running in the background. DO NOT close this session. Grab a cup of coffee, have a good sleep, or enjoy sun shine with friends. It takes 5 min per channel per block on my laptop. Good Luck!',
                             type = 'message', duration = 20L)

            # print check information for debug
            logger('If you see all "TRUE/FALSE" here, then wavelet is running in the background', level = 'INFO')
            logger(unlist(check()), level = 'INFO')
          }
        })
      })
    })


  })



  messageModal <- function(inputId, title = '', msg = '', details = NULL, type = 'success'){
    m = modalDialog(
      title = title, easyClose = T,
      div(
        class = sprintf('callout callout-%s', type),
        msg
      ),
      p(
        details
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(inputId, "OK")
      )
    )

    showModal(m)
  }


  shinyApp(ui, server)


}

# init_preprocess()
