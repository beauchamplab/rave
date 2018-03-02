mini_events_gen <- function(subject_code, project_name){
  dirs = get_dir(subject_code, project_name)
  blocks = list.dirs(dirs$pre_subject_dir, full.names = F, recursive = F)

  ui = miniPage(
    gadgetTitleBar("Event Generator"),
    miniTabstripPanel(
      miniTabPanel(
        "Import", icon = icon("table"),
        fillRow(
          fillCol(
            flex = c(NA,NA,NA,NA,NA,1),
            textInput('event_name', 'Event Name', value = '',
                      placeholder = 'Only letters and numbers are allowed'),
            selectInput('block_num', 'Block', selected = blocks[1], choices = blocks),
            textInput('event_file', 'Event File', value = ''),
            textInput('channel_file', 'Signal File', value = '', placeholder = 'Either channel number or full file name'),
            numericInput('srate', 'Sample Rate', value = 30000, min = 1),
            actionButton('import', 'Load Data')

          ),
          fillCol(
            flex = c(NA,NA,NA,NA,NA,1),
            numericInput('clip', 'Clip Start Code', value = 80),
            numericInput('event', 'Event Code', value = 4000)
          )
        )
      ),
      miniTabPanel(
        "Visualize", icon = icon("area-chart"),
        fillRow(
          fillCol(
            flex = c(1,2),
            fillCol(
              selectInput('thred_method', 'Method', choices = c(
                'First Exceed',
                'Bump'
              )),
              numericInput('thred', 'Threshold', value = 0),
              checkboxInput('rev', 'Reversed Direction', value = FALSE)
            ),
            plotOutput('raw_signal', height = '100%', width = '100%', brush = 'sub_sel')
          ),
          plotOutput('subplot', height = '100%', width = '100%', click = 'sel_level')
        )
      )
    )
  )

  server = function(input, output, session){
    dat = reactiveValues(
      eventInfos = NULL,
      eventData = NULL,
      eventTime = NULL,
      analogTraces = NULL,
      analogInfos = NULL,
      threshold = 0,
      ind_start = 0,
      ind_end = 30000,
      events = NULL,
      start_ind = NULL
    )

    observe({
      block_num = input$block_num
      default_file = sprintf('%sDatafile%s_timeStamp.mat', subject_code, block_num)
      prev = isolate(input$event_file)
      if(prev == '' || str_detect(prev, 'Datafile[0-9]+_timeStamp\\.mat')){
        updateTextInput(session, 'event_file', value = default_file)
      }

      # channel file
      cf = isolate(input$channel_file)
      d = get_dir(subject_code, project_name, block_num)$block_dir
      f = list.files(d)
      if(cf == '' || !(str_detect(cf, '^[0-9]+$') || cf %in% f)){
        f = str_extract(str_extract(f, 'ch[0-9]+\\.mat'), '[0-9]+')
        f = f[!is.na(f)]
        f = as.numeric(f)
        f = max(f)
        default_channel = sprintf('%d', f)
        updateTextInput(session,'channel_file',value = default_channel)
      }
    })

    observeEvent(input$import, {
      block_num = input$block_num
      event_file = file.path(dirs$pre_subject_dir, block_num, input$event_file)

      chl = input$channel_file
      if(str_detect(chl, '^[0-9]+$')){
        chl = sprintf('%sDatafile%s_ch%s.mat', subject_code, block_num, chl)
      }
      signal_file = file.path(dirs$pre_subject_dir, input$block_num, chl)

      ready = T
      if(!file.exists(event_file)){
        logger('Event file does NOT exist', level='ERROR')
        ready = F
      }
      if(!file.exists(signal_file)){
        logger('Signal file does NOT exist', level='ERROR')
        ready = F
      }

      if(ready){
        logger('Loading event data', level = 'INFO')
        # import event
        e = R.matlab::readMat(event_file)
        dat$eventInfos = e$eventInfos
        dat$eventData = e$eventData
        dat$eventTime = e$eventTime

        s = R.matlab::readMat(signal_file)
        dat$analogTraces = s$analogTraces
        dat$analogInfos = s$analogInfos

        srate = as.vector(s$analogInfos[,,1]$SampleRate)
        updateNumericInput(session, 'srate', value = srate)

      }
    })

    observe({
      srate = input$srate
      thred_method = input$thred_method
      rev = input$rev

      start_time = dat$eventTime[dat$eventData == input$clip]
      start_ind = round(start_time * srate)
      dat$start_ind = start_ind


      if(rev){
        sig = which(as.vector(dat$analogTraces) < dat$threshold)
      }else{
        sig = which(as.vector(dat$analogTraces) > dat$threshold)
      }


      if(thred_method == 'First Exceed'){
        sapply(start_ind, function(ind){
          subs = sig[sig > ind]
          if(length(subs) > 0){
            return(min(subs))
          }else{
            return(NA)
          }
        }) ->
          events
      }else{
        events = deparse_selections(sig, concatenate = F)
        events = events[str_detect(events, '\\-')]
        events = str_split(events, '\\-', simplify = T)
        events = as.numeric(events[,1])
      }



      dat$events = events
    })


    observe({
      l = input$sel_level
      if(!is.null(l)){
        updateNumericInput(session, 'thred', value = round(l$y, 2))
      }

    })

    observe({dat$threshold = input$thred})
    observe({
      l = input$sub_sel
      if(!is.null(l)){
        dat$ind_start = l$xmin * input$srate
        dat$ind_end = l$xmax * input$srate
      }
    })

    output$raw_signal <- renderPlot({


      plot_signal(dat$analogTraces, sample_rate = input$srate, boundary = NULL,
                  xbins = 500, details = F) ->
        p

      p + ggplot2::geom_hline(yintercept = dat$threshold, color = 'red') ->
        p

      p
    })

    output$subplot <- renderPlot({
      s = dat$analogTraces
      start_ind = dat$start_ind
      events = dat$events
      n = length(s)

      print(dat$ind_start)
      ind = 1:n
      ind = ind[ind >= dat$ind_start & ind <= dat$ind_end]


      s = s[ind]



      plot(s, main = sprintf('Sub-plot (%d data points)', length(ind)), xlab = 'Time points',
           ylab = 'Strength', type='l')

      abline(h = dat$threshold, col = 'red')
      if(!is.null(start_ind)){
        abline(v = start_ind - min(ind), col = 'blue')
      }

      if(!is.null(events)){
        abline(v = events - min(ind), col = 'red')
      }

    })


    observeEvent(input$done, {

      stopApp(invisible(reactiveValuesToList(dat)))
    })
  }

  shiny::runGadget(ui, server)

}

