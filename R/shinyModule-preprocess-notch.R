rave_pre_notch3 <- function(module_id = 'NOTCH_M', sidebar_width = 2, doc_prefix = 'ravepreprocessnotch', ...){
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  body = fluidRow(
    column(
      width = sidebar_width,
      fluidRow(
        uiOutput(ns('notch_inputs1'))
      )
    ),
    box(
      width = 12 - sidebar_width,
      title = 'Notch - Inspect Signals',
      box_link = sprintf(url_format, 'output', 'notchinspectsignals'),
      plotOutput(ns('plot_signal'), height = '75vh')
    )
    
  )
  
  
  server = function(input, output, session, user_data, utils, ...){
    local_data = reactiveValues(
      reset = NULL,
      has_notch = FALSE,
      has_raw_cache = FALSE,
      winlen = 7,
      nclass = 100,
      freq_lim = 300,
      last_block = NULL,
      last_elec = NULL,
      raw_signal = NULL
    )
    
    # Init/reset data when receive reset signal
    observeEvent(user_data$reset, {
      # Reset first
      local_data$reset = Sys.time()
      local_data$raw_signal = NULL
      local_data$filtered_signal = NULL
      
      # Update according to subject loaded
      local_data$has_notch = utils$notch_filtered()
      local_data$has_raw_cache = utils$has_raw_cache()
      
      # update subject data
      local_data$blocks = utils$get_blocks()
      local_data$electrodes = utils$get_electrodes()
    })
    
    # Assign from inputs to local_data
    observe({
      local_data$winlen = input$winlen
      local_data$nclass = input$nclass
      local_data$freq_lim = input$freq_lim
      local_data$last_block = input$block
      local_data$last_elec = input$electrode
    })
    
    # Load raw and notch filtered data
    observe({
      local_data$reset
      block = local_data$last_block
      electrode = as.integer(local_data$last_elec)
      local_data$raw_signal = NULL
      local_data$filtered_signal = NULL
      if(length(block) && length(electrode)){
        if(local_data$has_raw_cache){
          local_data$raw_signal = utils$load_voltage(electrodes = electrode, blocks = block, raw = T)[[1]]
        }
        if(local_data$has_notch){
          local_data$filtered_signal = utils$load_voltage(electrodes = electrode, blocks = block, raw = F)[[1]]
        }
      }
      catgl('Voltage signal loaded.')
    })
    
    # input panel
    output$notch_inputs1 <- renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache, message = 'Please import subject first.'))
      
      srate = utils$get_srate()
      
      winlen = get_val(isolate(local_data$winlen), default = ceiling(2 * srate))
      nclass = get_val(isolate(local_data$nclass), default = 100)
      freq_lim = get_val(isolate(local_data$freq_lim), default = 300)
      last_block = get_val(isolate(local_data$last_block), default = NULL)
      last_elec = get_val(isolate(local_data$last_elec), default = NULL)
      
      last = get_val(utils$last_inputs(), 'last_notch_freq', default = 60)
      
      notch_freq_bw = get_val(isolate(local_data$notch_freq_bw), default = '1,2,2')
      notch_freq_x = get_val(isolate(local_data$notch_freq_x), default = '1,2,3')
      
      tagList(
        box(
          width = 12,
          title = 'Notch Filter',
          box_link = sprintf(url_format, 'input', 'notchfilter'),
          collapsible = TRUE, collapsed = local_data$has_notch,
          numericInput(ns('notch_freq'), 'Base Frequency, (Hz)', value = last),
          textInput(ns('notch_freq_x'), 'X (Times)', value = notch_freq_x),
          textInput(ns('notch_freq_bw'), '+- (Band Width, Hz)', value = notch_freq_bw),
          hr(),
          htmlOutput(ns('notch_bands')),
          actionButton(ns('do_notch'), 'Apply Notch Filters')
        ),
        box(
          width = 12,
          title = 'Inspection',
          box_link = sprintf(url_format, 'input', 'inspection'),
          downloadLink(ns('pwelch_download'), 'Download as PDF'),
          hr(),
          div(
            class = 'rave-grid-inputs',
            div(
              style = 'flex-basis: 50%',
              selectInput(ns('block'), 'Block', choices = local_data$blocks, selected = last_block)
            ),
            div(
              style = 'flex-basis: 50%',
              selectInput(ns('electrode'), 'Electrodes', choices = local_data$electrodes, selected = last_elec)
            )
          ),
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next'),
          hr(),
          sliderInput(ns('winlen'), 'Pwelch Window Length: ',
                      min = 100, max = ceiling(2 * srate), value = winlen),
          sliderInput(ns('freq_lim'), 'Frequency limit', min = 20, max = floor(srate / 2), step = 1L, value = freq_lim),
          sliderInput(ns('nclass'), 'Number of bins (histogram): ', min = 20, max = 200, value = nclass)
        )
      )
      
      
    })
    
    output$pwelch_download <- downloadHandler(
      filename = function(){
        projn = utils$get_from_subject('project_name')
        scode = utils$get_from_subject('subject_code')
        chanl = utils$get_from_subject('channels')
        chntx = dipsaus::deparse_svec(chanl)
        if( stringr::str_length(chntx) > 20 ){
          chntx = sprintf('total %d')
        }
        sprintf('[%s][%s] inspection [%s].zip', projn, scode, chntx)
      }, content = function(con){
        projn = utils$get_from_subject('project_name')
        scode = utils$get_from_subject('subject_code')
        chanl = utils$get_from_subject('channels')
        block = utils$get_from_subject('blocks')
        srate = utils$get_srate()
        
        winlen = get_val(isolate(input$winlen), default = ceiling(2 * srate))
        freq_lim = get_val(isolate(input$freq_lim), default = 300)
        nclass = get_val(isolate(input$nclass), default = 100)
        
        tp_dir = tempfile()
        
        showNotification(p('Exporting in progress. This might take a while. Please do not change the panel nor close the window...'), 
                         duration = NULL, closeButton = FALSE, type = 'message', id = ns('notch_noti'))
        export_diagnose_voltage(
          subject = sprintf('%s/%s', projn, scode),
          electrodes = chanl, blocks = block, onefile = TRUE, 
          winlen = winlen, freq_lim = freq_lim, nclass = nclass, 
          save_dir = tp_dir
        )
        wd = getwd()
        on.exit({ setwd(wd) })
        # Get zip
        setwd(tp_dir)
        fs = list.files(tp_dir, pattern = '.pdf$')
        utils::zip(con, fs)
        removeNotification(id = ns('notch_noti'))
      }
    )
    
    # Prev, Next button
    observeEvent(input$nxt, {
      electrodes = utils$get_electrodes()
      e = as.integer(input$electrode)
      electrodes = electrodes[electrodes > e]
      if(length(electrodes)){
        updateSelectInput(session, 'electrode', selected = min(electrodes))
      }
    })
    observeEvent(input$prev, {
      electrodes = utils$get_electrodes()
      e = as.integer(input$electrode)
      electrodes = electrodes[electrodes < e]
      if(length(electrodes)){
        updateSelectInput(session, 'electrode', selected = max(electrodes))
      }
    })
    
    # outputs
    output$plot_signal <- renderPlot({
      
      raw = local_data$raw_signal
      filter = local_data$filtered_signal
      
      if(local_data$has_raw_cache && length(raw)){
        has_notch = local_data$has_notch
        
        # Get inputs
        winlen = local_data$winlen
        nclass = local_data$nclass
        freq_lim = local_data$freq_lim
        block = local_data$last_block
        electrode = as.integer(local_data$last_elec)
        srate = utils$get_srate(); srate = max(1, srate)
        
        if(has_notch){
          s1 = filter
          s2 = raw
          name = 'Notch'
          col = c('black', 'grey80')
          main = sprintf('Notch Filtered Signal - Block: %s, Electrode: %d', block, electrode)
          
        }else{
          s1 = raw
          s2 = NULL
          name = 'Raw'
          col = 'black'
          main = sprintf('Raw Voltage Signal - Block: %s, Electrode: %d', block, electrode)
        }
        
        catgl('Rendering signal plot')
        diagnose_signal(
          s1 = s1, s2 = s2, col = col,
          name = name,
          max_freq = freq_lim,
          srate = srate,
          window = winlen,
          noverlap = winlen / 2,
          nclass = nclass,
          cex = 2,
          std = 3,
          lwd = 0.3,
          main = main
        )
      }
    })
    
    output$notch_bands <- renderText({
      notch_freq = input$notch_freq
      notch_freq_x = dipsaus::parse_svec(input$notch_freq_x)
      width = dipsaus::parse_svec(input$notch_freq_bw, unique = F)
      
      if(length(notch_freq_x) != length(width)){
        return('Frequency and band width should equal in length. Check your inputs.')
      }
      if(notch_freq <= 0){
        return('Base frequency should be a positive number.')
      }
      
      center = notch_freq * notch_freq_x
      from = center - width
      to = center + width
      paste(tags$dl(
        tagList(lapply(1:length(center), function(i){
          tagList(
            tags$dt(sprintf('Band %d: ', i)),
            tags$dd(HTML(sprintf('%.0f Hz - %.0f Hz (%.0f&plusmn;%.0f)', from[i], to[i], center[i], width[i])))
          )
        }))
      ))
      
      
    })
    
    # Btn apply notch
    observeEvent(input$do_notch, {
      # numericInput(ns('notch_freq'), 'Base Frequency, (Hz)', value = last),
      # textInput(ns('notch_freq_x'), 'X (Times)', value = '1,2,3'),
      # textInput(ns('notch_freq_bw'), '+- (Band Width, Hz)', value = '1,2,2'),
      notch_freq = input$notch_freq
      notch_freq_x = dipsaus::parse_svec(input$notch_freq_x)
      widths = dipsaus::parse_svec(input$notch_freq_bw, unique = F)
      if(length(notch_freq_x) != length(widths)){
        utils$showNotification(msg = 'Frequency and band width should equal in length. Check your inputs.', type = 'error')
        return()
      }
      if(notch_freq <= 0){
        utils$showNotification(msg = 'Base frequency should be a positive number', type = 'error')
        return()
      }
      
      utils$apply_notch(bandwidths = list(
        'default' = list(
          centers = notch_freq * notch_freq_x,
          widths = widths
        )
      ))
      
      local_data$notch_freq_bw = input$notch_freq_bw
      local_data$notch_freq_x = input$notch_freq_x
      showNotification(p('Notch filter finished'), type='message')
      
      utils$reset()
    })
  }
  
  return(list(
    body = body,
    server = server
  ))
}
