rave_pre_wavelet3 <- function(module_id = 'OVERVIEW_M', sidebar_width = 2, doc_prefix = 'ravepreprocesswavelet', ...){
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  body = fluidRow(
    shiny::column(
      width = sidebar_width,
      uiOutput(ns('wavelet_inputs1')),
      uiOutput(ns('wavelet_inputs2')),
      uiOutput(ns('wavelet_inputs3'))
    ),
    box(
      collapsible = TRUE,
      width = 12 - sidebar_width,
      title = 'Wavelet Kernels',
      box_link = sprintf(url_format, 'output', 'waveletkernels'),
      plotOutput(ns('wave_windows_plot'), height = '80vh')
    )
  )
  
  server = function(input, output, session, user_data, utils, ...){
    local_data = reactiveValues(
      prevent_rewavelet = TRUE,
      wave_param = NULL
    )
    
    conf_dir = file.path(rave_options('module_root_dir'), 'cache', 'preprocess', 'wavelet')
    dir.create(conf_dir, recursive = T, showWarnings = F)
    
    output$wavelet_inputs1 <- renderUI({
      user_data$reset
      if(!utils$notch_filtered()){
        return(box(collapsible = TRUE, title = 'General Settings', width = 12L, tags$small('Please finish previous steps first (Import subject, Notch filter).')))
      }
      electrodes = utils$get_electrodes()
      vc_txt = dipsaus::deparse_svec(electrodes)
      max_core = max(1, rave_options('max_worker'))
      best_ncores = 4L
      try({
        tmp = as.integer(floor(dipsaus::mem_limit2()$total / 1024^3 / 8))
        if(length(tmp) == 1 && !is.na(tmp) && is.integer(tmp) && tmp > 0){
          best_ncores = tmp
        }
      }, silent = TRUE)
      best_ncores = min(best_ncores, max_core)
      
      srate = utils$get_srate()
      target_srate = 100
      
      if(utils$waveleted()){
        log = utils$get_from_subject('wavelet_log', customized = T)
        log = log[[length(log)]]
        target_srate = log$target_srate
      }
      
      box(
        collapsible = TRUE,
        box_link = sprintf(url_format, 'input', 'generalsettings'),
        title = 'General Settings', width = 12L,
        textInput(ns('wave_electrodes'), 'Electrodes:', value = vc_txt, placeholder = 'Select at least one electrode.'),
        numericInput(ns('target_srate'), 'Target Sample Rate', value = target_srate, min = 10L, max = srate, step = 1L),
        numericInput(ns('ncores'), 'Parallel, Number of Cores:', value = best_ncores, min = 1L, max = max_core, step = 1L)
      )
    })
    
    output$wavelet_inputs2 <- renderUI({
      user_data$reset
      local_data$refresh_wavelet
      validate(
        need(utils$notch_filtered(), '')
      )
      
      # srate = utils$get_srate()
      # freq_range = c(2,200)
      # freq_step = 2
      # wave_num = c(3,20)
      #
      # tagList(
      #   hr(),
      #   sliderInput(ns('freq_range'), 'Frequency Range (Hz):', value = freq_range, step = 1L, round = TRUE, min = 1L, max = srate / 2),
      #   numericInput(ns('freq_step'), 'Frequency Step Size (Hz): ', value = freq_step, step = 1L, min = 1L),
      #   sliderInput(ns('wave_num'), 'Number of Wavelet Cycles: ', value = wave_num, step = 1L, min = 1L, max = 40L, round = T)
      # )
      
      # check all wavelet settings
      choices = list.files(conf_dir, pattern = '^[^_].*\\.csv', full.names = F, recursive = F)
      
      dname = stringr::str_replace_all(utils$get_subject_id(), '[/]', '_')
      dname = paste0('_', dname, '.csv')
      
      choices = choices[choices != dname]
      choices = c('[New setting]', choices)
      selected = isolate(local_data$wavelet_selected)
      
      if(utils$waveleted()){
        log = utils$get_from_subject('wavelet_log', customized = T)
        log = log[[length(log)]]
        freqs = log$frequencies
        cycles = log$wave_num
        target_srate = log$target_srate
        
        try({
          # compatibility issue
          if(length(cycles) == 2 && length(freqs) != length(cycles)){
            # this is old scheme, try to be compatible
            cycles = exp((log(cycles[2]) - log(cycles[1])) / (log(max(freqs)) - log(min(freqs))) * (log(freqs) - log(min(freqs)))) * cycles[1]
          }
          
          last = data.frame(Frequency = freqs, WaveCycle = cycles)
          utils::write.csv(last, file = file.path(conf_dir, dname), row.names = F)
          selected %?<-% dname
          choices = c(dname, choices)
        })
      }
      
      
      selected %?<-% choices[1]
      
      
      box(
        collapsible = TRUE,
        title = 'Wavelet Settings', width = 12L,
        box_link = sprintf(url_format, 'input', 'waveletsettings'),
        div(
          class = 'rave-grid-inputs',
          div(
            style = 'flex-basis:50%; height: 80px;',
            selectInput(ns('wave_conf_name'), 'Configuration', choices = choices, selected = selected)
          ),
          div(
            style = 'flex-basis:50%; height: 80px;',
            fileInput(ns('wave_conf_load'), 'Upload Preset', multiple = F, accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")
            )
          )
        ),
        actionButton(ns('do_wavelet'), 'Run Wavelet')
      )
    })
    
    output$wavelet_inputs3 <- renderUI({
      fname = input$wave_conf_name
      path = file.path(conf_dir, fname)
      validate(need(length(path) == 1 && (
        file.exists(path) || fname %in% c('[New setting]')
      ), message = ''))
      
      srate = utils$get_srate()
      freq_range = c(2,200)
      freq_step = 2
      wave_num = c(3,20)
      additional_ui = NULL
      if(fname == '[New setting]'){
        additional_ui = tagList(
          sliderInput(ns('freq_range'), 'Frequency Range (Hz):', value = freq_range, step = 1L, round = TRUE, min = 1L, max = srate / 2),
          numericInput(ns('freq_step'), 'Frequency Step Size (Hz): ', value = freq_step, step = 1L, min = 1L),
          sliderInput(ns('wave_num'), 'Number of Wavelet Cycles: ', value = wave_num, step = 1L, min = 1L, max = 40L, round = T),
          hr()
        )
      }else{
        # read from config name
        local_data$refresh_wavelet
        tbl = utils::read.csv(path, header = TRUE)
        set_wave_param(tbl$Frequency, tbl$WaveCycle, as_is = TRUE)
      }
      
      box(
        collapsible = TRUE,
        title = 'Details', width = 12L,
        box_link = sprintf(url_format, 'input', 'details'),
        additional_ui,
        p(
          downloadLink(ns('wave_conf_save'), label = 'Download Current Setting')
        ),
        
        DT::dataTableOutput(ns('wave_details'))
      )
    })
    
    # observeEvent(input$wave_conf_save, {
    #   name = input$wave_conf_new
    #   name = stringr::str_remove_all(name, '[\\w]')
    #   if(stringr::str_length(name) == 0){
    #     name = 'no_name'
    #   }
    #   name = paste(name, '.csv')
    #   tbl = local_data$wave_param
    #
    #   # write to rave_modules/cache
    #   utils::write.csv(tbl, file.path(conf_dir, name), row.names = F)
    #
    #   local_data$refresh_wavelet = Sys.time()
    #   local_data$wavelet_selected = name
    # })
    
    output$wave_conf_save <- downloadHandler(
      filename = function(){
        name = isolate(input$wave_conf_name)
        name = stringr::str_remove_all(name, '[^\\w\\.]')
        name = stringr::str_remove(name, '^[_]+')
        if(!stringr::str_detect(name, '\\.csv$')){
          name = paste0(name, '.csv')
        }
        name
      },
      content = function(file){
        try({
          utils::write.csv(local_data$wave_param, file, row.names = F)
        })
      }
    )
    
    observe({
      fname = input$wave_conf_name
      if(!is.null(fname) && fname == '[New setting]'){
        freq_range = input$freq_range
        freq_step = input$freq_step
        if(is_invalid(freq_step)){
          freq_step = 2
        }
        freq_step = max(freq_step, 0.5)
        wave_num = input$wave_num
        if(length(freq_range) == 2 && length(freq_step) == 1 && length(wave_num) == 2){
          freq = seq(freq_range[1], freq_range[2], by = freq_step)
          set_wave_param(freq, wave_num)
        }
      }
      
    })
    
    set_wave_param = function(f, w, as_is = FALSE){
      if(length(w) == 2 && length(f) != 2){
        w = sort(w)
        w = exp((log(w[2]) - log(w[1])) / (log(max(f)) - log(min(f))) * (log(f) - log(min(f))) + log(w[1]))
      }
      
      d = duplicated(f)
      f = f[!d]
      w = w[!d]
      o = order(f)
      f = f[o]
      if(!as_is){
        w = round(w[o])
      }else{
        w = w[o]
      }
      
      local_data$wave_param = data.frame(
        Frequency = f,
        WaveCycle = w
      )
    }
    
    
    output$wave_details <- DT::renderDataTable({
      local_data$wave_param
    })
    
    
    observeEvent(input$wave_conf_load, {
      # Check data
      print(input$wave_conf_load)
      finfo = input$wave_conf_load
      name = finfo$name
      path = finfo$datapath
      name = stringr::str_split(name, '\\.')[[1]]
      name = name[[1]]
      name = stringr::str_remove_all(name, '[\\W]')
      name = paste0(name, '.csv')
      # try to read csv from datapath
      tryCatch({
        dat = utils::read.csv(path, header = T)
        freq = as.numeric(dat[,1])
        stopifnot2(all(!is.na(freq)), msg = 'Non-numeric values found in frequencies.')
        cycles = NULL
        if(ncol(dat) >= 2){
          cycles = as.numeric(dat[,2])
          if(any(is.na(cycles))){
            cycles = NULL
          }
        }
        cycles %?<-% (exp((log(14/3)) / (log(40)) * (log(freq) - log(2))) * 3)
        cycles[cycles < 1] = 1
        
        
        # write to rave_modules/cache
        utils::write.csv(data.frame(
          Frequency = freq,
          WaveCycle = cycles
        ), file.path(conf_dir, name), row.names = F)
        
        local_data$refresh_wavelet = Sys.time()
        local_data$wavelet_selected = name
      }, error = function(e){
        showNotification(p('Cannot read uploaded file. Make sure the first column contains frequencies and the second column (optional) contains wavelet cycles.'), type = 'error')
      })
    })
    
    # Modal
    observeEvent(input$do_wavelet, {
      # check
      w_e = dipsaus::parse_svec(input$wave_electrodes)
      es = utils$get_electrodes()
      w_e = w_e[w_e %in% es]
      tbl = local_data$wave_param
      
      w_freqs = as.numeric(tbl$Frequency)
      w_cycle = as.numeric(tbl$WaveCycle)
      
      local_data$prevent_rewavelet = FALSE
      
      
      if(length(w_e) && length(w_freqs) > 1 && length(w_freqs) == length(w_cycle) && !any(is.na(w_freqs)) && !any(is.na(w_cycle))){
        showModal(
          modalDialog(
            title = 'Confirmation',
            easyClose = F,
            footer = tagList(
              actionButton(ns('cancel'), 'Cancel'),
              actionButton(ns('ok'), 'Do it!')
            ),
            fluidRow(
              column(
                width = 12,
                p('Apply wavelet to the following electrodes:'),
                tags$blockquote(dipsaus::deparse_svec(w_e)),
                p('This might take a while.'),
                hr(),
                p("* It's highly recommend that you perform wavelet to all channels first with **SAME** settings, otherwise reference module will work improperly.")
              )
            )
          )
        )
      }else{
        
        showModal(
          modalDialog(
            title = 'Warning',
            easyClose = T,
            fluidRow(
              column(
                width = 12,
                p('Check your inputs, there might be some problem(s).'),
                tags$ul(
                  tags$li('Valid Channels: ', dipsaus::deparse_svec(w_e)),
                  tags$li('Frequencies: ', paste(range(w_freqs), collapse = '-'), ' Hz. Make sure they are positive numbers.'),
                  tags$li('Wavelet Cycles: ', paste(range(w_cycle), collapse = '-'), '. Make sure they are positive numbers..')
                )
              )
            )
          )
        )
      }
    })
    
    observeEvent(input$cancel, {
      # little trick: if server is running other tasks, modal will not be removed
      removeModal()
    })
    
    observeEvent(input$ok, {
      if(isolate(local_data$prevent_rewavelet)){
        return()
      }
      local_data$prevent_rewavelet = TRUE
      w_e = dipsaus::parse_svec(input$wave_electrodes)
      es = utils$get_electrodes()
      w_e = w_e[w_e %in% es]
      # frange = round(input$freq_range)
      # fstep = max(1, round(input$freq_step))
      # w_freqs = seq(frange[1], frange[2], by = fstep)
      tbl = local_data$wave_param
      
      w_freqs = as.numeric(tbl$Frequency)
      w_cycle = as.numeric(tbl$WaveCycle)
      
      showNotification(p('Wavelet start!'), type = 'message')
      
      catgl('[WAVELET] Number of electrodes: ', length(w_e))
      
      utils$apply_wavelet(
        electrodes = w_e,
        target_srate = round(input$target_srate),
        frequencies = w_freqs,
        wave_num = w_cycle,
        ncores = round(input$ncores)
      )
      
      utils$reset()
      
      showNotification(p('Wavelet finished!'), type = 'message')
      removeModal()
    })
    
    # Output plot
    output$wave_windows_plot <- renderPlot({
      tbl = local_data$wave_param
      wave_num = tbl$WaveCycle
      freqs = tbl$Frequency
      # wave_num = input$wave_num
      # freq_range = input$freq_range
      # fstep = input$freq_step
      srate = utils$get_srate()
      
      validate(
        need(length(wave_num) > 0 && all(wave_num > 0), 'Specify number of wavelet cycles'),
        need(length(freqs) > 0 && all(freqs > 0), 'Specify valid frequency range'),
        need(length(srate) > 0 && srate > 0, 'Wait, have you loaded subject yet?')
      )
      # freqs = seq(freq_range[1], freq_range[2], by = fstep)
      wavelet_kernels(freqs, srate, wave_num)
    })
    
    
  }
  
  return(list(
    body = body,
    server = server
  ))
}


