#' @importFrom DT formatRound
#' @importFrom DT datatable
#' @export
rave_pre_notch3 <- function(module_id = 'NOTCH_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    column(
      width = sidebar_width,
      fluidRow(
        uiOutput(ns('notch_inputs1'))
      )
    ),
    shinydashboard::box(
      width = 12 - sidebar_width,
      title = 'Notch - Inspect Signals',
      plotOutput(ns('plot_signal'), height = '75vh')
    )

  )


  server = function(input, output, session, user_data, utils){
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
      logger('Voltage signal loaded.')
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

      tagList(
        shinydashboard::box(
          width = 12,
          title = 'Notch Filter',
          collapsible = T, collapsed = local_data$has_notch,
          numericInput(ns('notch_freq'), 'Base Frequency, (Hz)', value = last),
          textInput(ns('notch_freq_x'), 'X (Times)', value = '1,2,3'),
          textInput(ns('notch_freq_bw'), '+- (Band Width, Hz)', value = '1,2,2'),
          hr(),
          htmlOutput(ns('notch_bands')),
          actionButton(ns('do_notch'), 'Apply Notch Filters')
        ),
        shinydashboard::box(
          width = 12,
          title = 'Inspection',
          selectInput(ns('block'), 'Block', choices = local_data$blocks, selected = last_block),
          selectInput(ns('electrode'), 'Electrodes', choices = local_data$electrodes, selected = last_elec),
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next'),
          hr(),
          sliderInput(ns('winlen'), 'Pwelch Window Length): ',
                      min = 100, max = ceiling(2 * srate), value = winlen),
          sliderInput(ns('freq_lim'), 'Frequency limit', min = 20, max = floor(srate / 2), step = 1L, value = freq_lim),
          sliderInput(ns('nclass'), 'Number of bins (histogram): ', min = 20, max = 200, value = nclass)
        )
      )


    })

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

        logger('Rendering signal plot')
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
      notch_freq_x = rave:::parse_selections(input$notch_freq_x)
      width = rave:::parse_selections(input$notch_freq_bw, unique = F)

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
      notch_freq_x = rave:::parse_selections(input$notch_freq_x)
      widths = rave:::parse_selections(input$notch_freq_bw, unique = F)
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
      showNotification(p('Notch filter finished'), type='message')

      utils$reset()
    })
  }

  return(list(
    body = body,
    server = server
  ))
}



.rave_pre_notch3 <- function(module_id = 'NOTCH_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  CHANNEL_TYPES = c(
    'Good',
    'Epilepsy',
    'Bad',
    'Exclude'
  )
  COLOR_SCHEME = rbind(
    c('black', 'red'),
    c('grey20', 'red'),
    c('grey80', 'red'),
    c('blue', 'red')
  )

  body = fluidRow(
    column(
      width = sidebar_width,
      fluidRow(
        shinydashboard::box(
          width = 12,
          title = 'Notch Filter',
          uiOutput(ns('notch_inputs1'))
        )
      )
    ),
    shinydashboard::tabBox(
      width = 12 - sidebar_width,
      tabPanel(
        title = 'Notch - Inspect Signals',
        plotOutput(ns('plot_signal'), height = '35vh'),
        plotOutput(ns('plot_signal_others'), height = '35vh')
      ),
      tabPanel(
        title = 'Time Selection',
        fluidRow(
          column(
            width = 12,
            plotOutput(ns('plot_signal_only'), height = '35vh', brush = brushOpts(ns('notch_brush'), resetOnNew = TRUE, direction = 'x', delayType = 'throttle'))
          ),
          column(
            width = 6,
            plotOutput(ns('plot_signal_details'), height = '35vh')
          ),
          column(
            width = 6,
            actionButton(ns('stage'), 'Stage Changes'),
            actionButton(ns('clear'), 'Clear Selection'),
            DT::dataTableOutput(ns('signal_time'))
          )
        )
      )
    )

  )

  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      is_notch = FALSE,
      has_notch = FALSE,
      current_chl = 0,
      s1 = NULL,
      s2 = NULL,
      sc = NULL,
      compress_level = 1,
      max_time = 0,
      chl_type = 1,
      excluded_time = data.frame()
    )
    output$notch_inputs1 <- renderUI({
      user_data$reset
      local_data$has_notch = FALSE
      if(!utils$has_subject()){
        return(p('Please load subject first.'))
      }
      if(!utils$notch_filtered()){
        last = get_val(utils$last_inputs(), 'last_notch_freq', default = 60)

        return(tagList(
          numericInput(ns('notch_freq'), 'Base Frequency, (Hz)', value = last),
          textInput(ns('notch_freq_x'), 'X (Times)', value = '1,2,3'),
          textInput(ns('notch_freq_bw'), '+- (Band Width, Hz)', value = '1,2,2'),
          hr(),
          htmlOutput(ns('notch_bands')),
          actionButton(ns('do_notch'), 'Apply Notch Filters')
        ))
      }else{
        # notch filtered
        local_data$has_notch = TRUE
        blocks = utils$get_blocks()
        last_block = isolate(local_data$current_block)
        if(!length(last_block) || !last_block %in% blocks){
          last_block = NULL
        }
        channels = utils$get_channels()
        last_channel = isolate(local_data$current_chl)
        if(!length(last_channel) || !last_channel %in% channels){
          last_channel = NULL
        }

        srate = utils$get_srate()
        winlen = get_val(isolate(local_data$winlen), default = ceiling(srate * 2))
        nclass = get_val(isolate(local_data$nclass), default = 100)
        freq_lim = get_val(isolate(local_data$freq_lim), default = 300)
        tagList(
          selectInput(ns('block'), 'Block', choices = blocks, selected = last_block),
          selectInput(ns('chl'), 'Channel', choices = channels, selected = last_channel),
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next'),
          hr(),
          selectInput(ns('chl_type'), 'Channel type', choices = CHANNEL_TYPES,
                      selected = isolate(CHANNEL_TYPES[local_data$chl_type])),
          hr(),
          sliderInput(ns('winlen'), 'Pwelch Window Length): ',
                       min = 100, max = ceiling(srate * 2), value = ceiling(srate * 2)),
          sliderInput(ns('freq_lim'), 'Frequency limit', min = 20, max = floor(srate / 2), step = 1L, value = freq_lim),
          sliderInput(ns('nclass'), 'Number of bins (histogram): ', min = 20, max = 200, value = nclass)
        )
      }

    })

    observeEvent(input$nxt, {
      chls = utils$get_channels()
      cl = as.integer(input$chl)
      chls = chls[chls > cl]
      if(length(chls)){
        updateSelectInput(session, 'chl', selected = min(chls))
      }
    })

    observeEvent(input$prev, {
      chls = utils$get_channels()
      cl = as.integer(input$chl)
      chls = chls[chls < cl]
      if(length(chls)){
        updateSelectInput(session, 'chl', selected = max(chls))
      }
    })

    observe({
      local_data$current_block = input$block
      local_data$winlen = input$winlen
      local_data$nclass = input$nclass
      local_data$freq_lim = input$freq_lim
    })

    observe({
      block = input$block
      chl = as.integer(input$chl)
      srate = utils$get_srate()
      if(local_data$has_notch && length(block) && length(chl)){
        if(srate > 500){
          local_data$sc = utils$load_signal(block = block, channel = chl, group = '/notch/compress20')
          local_data$compress_level = 20
        }else{
          local_data$sc = utils$load_signal(block = block, channel = chl, group = '/notch/compress5')
          local_data$compress_level = 5
        }
        s1 = utils$load_signal(block = block, channel = chl, group = '/notch')
        local_data$s1 = s1
        local_data$s2 = utils$load_signal(block = block, channel = chl, group = '/raw')

        local_data$max_time = length(s1) / srate

        ctype = utils$get_channel_type(chl)
        local_data$chl_type = ctype
        local_data$current_chl = chl


        local_data$excluded_time = utils$get_excluded_time(block)
        local_data$new_sel = NULL
        updateSelectInput(session, 'chl_type', selected = CHANNEL_TYPES[ctype])
      }else{
        local_data$s1 = NULL
        local_data$s2 = NULL
        local_data$sc = NULL
        local_data$chl_type = 1
        local_data$max_time = 0
        local_data$excluded_time = data.frame()
        local_data$new_sel = NULL
      }
    }, priority = 10L)


    observeEvent(input$stage, {
      block = input$block

      tbl = local_data$excluded_time
      tbl %?<-% data.frame()
      xlim = local_data$new_sel

      if(utils$has_subject() && !is.null(xlim) && !(xlim[1] %in% tbl$Start && xlim[2] %in% tbl$End)){
        tbl = rbind(
          data.frame(Start = xlim[1], End = xlim[2], Staged = TRUE),
          tbl
        )
        utils$set_excluded_time(tbl, block)
      }
      local_data$excluded_time = utils$get_excluded_time(block)
    })

    observeEvent(input$clear, {
      s = input$signal_time_rows_selected
      xlim = local_data$new_sel
      tbl = isolate(local_data$excluded_time)
      block = input$block

      if(length(s)){
        if(!is.null(xlim) && !(xlim[1] %in% tbl$Start && xlim[2] %in% tbl$End)){
          s = s - 1; s = s[s > 0]
        }
        if(length(s)){
          tbl = tbl[-s,]
          utils$set_excluded_time(tbl, block)
          local_data$excluded_time = utils$get_excluded_time(block)
        }
      }
    })

    observe({
      s = input$signal_time_rows_selected
      tbl = local_data$excluded_time
      local_data$tbl_sel = NULL
      if(length(s) == 1){
        xlim = local_data$new_sel
        if(!is.null(xlim) && !(xlim[1] %in% tbl$Start && xlim[2] %in% tbl$End)){
          s = s - 1
        }

        if(s > 0){
          sel = tbl[s,]
          local_data$tbl_sel = c(sel$Start, sel$End)
        }
      }
    })



    observeEvent(input$chl_type, {
      ctype = input$chl_type
      chl = as.integer(input$chl)
      if(local_data$has_notch && length(chl) && length(ctype)){
        i = which(CHANNEL_TYPES == ctype)
        if(isolate(local_data$chl_type) != i){
          local_data$chl_type = i
          utils$add_to_channel(chl, i)
        }

      }
    })




    observe({
      f = input$notch_freq
      f %?<-% 0

      m = as.integer(unlist(str_split(input$notch_freq_x, ',')))
      m = m[!is.na(m) & m > 0]

      w = as.integer(unlist(str_split(input$notch_freq_bw, ',')))
      w = w[!is.na(w) & w > 0]
      l_m = length(m)

      passed = FALSE
      if(f <= 0){
        local_data$error = 'Base frequency must be > 0'
      }else if(l_m == 0){
        local_data$error = 'Number of bands need to be > 0'
      }else if(length(w) != l_m){
        local_data$error = 'Band widths must match with bands (equal length)'
      }else{
        local_data$error = NULL
        passed = TRUE
      }

      if(passed){
        local_data$center_freqs = m * f
        local_data$band_width = w
      }else{
        local_data$center_freqs = NULL
        local_data$band_width = NULL
      }
    })

    output$notch_bands <- renderText({
      validate(
        need(length(local_data$error) == 0, local_data$error)
      )

      center = local_data$center_freqs
      width = local_data$band_width
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

    observeEvent(input$cancel, {
      if(!local_data$is_notch){
        removeModal()
      }else{
        showNotification(p(
          'Notch filter is running. Please wait...'
        ), type = 'warning')
      }
    })

    observeEvent(input$do_notch, {
      blocks = utils$get_from_subject('blocks', NULL)
      channels = utils$get_from_subject('channels', NULL)
      srate = utils$get_from_subject('srate', 0)
      error = local_data$error
      center = local_data$center_freqs
      width = local_data$band_width

      if(
        length(blocks) &&
        length(channels) &&
        length(srate) &&
        srate > 0 &&
        length(error) == 0
      ){

        from = center - width
        to = center + width
        showModal(
          modalDialog(
            title = 'Confirmation',
            easyClose = F,
            footer = tagList(
              actionButton(ns('cancel'), 'Cancel'),
              actionButton(ns('ok'), 'Confirm')
            ),
            fluidRow(
              column(
                width = 12,
                p('You can not change the following variables once notch filter is applied:'),
                tags$ul(
                  tags$li('Blocks: ' %&% paste(blocks, collapse = ', ')),
                  tags$li('Channels: ' %&% rave:::deparse_selections(channels)),
                  tags$li('Sample Rate: ' %&% srate)
                ),
                p('Notch filter will be applied to the following frequency bands:'),
                tags$dl(
                  tagList(lapply(1:length(center), function(i){
                    tagList(
                      tags$dt(sprintf('Band %d: ', i)),
                      tags$dd(HTML(sprintf('%.0f Hz - %.0f Hz (%.0f&plusmn;%.0f)', from[i], to[i], center[i], width[i])))
                    )
                  }))
                )
              )
            )
          )
        )
      }else{
        showModal(
          modalDialog(
            title = 'Warning',
            easyClose = T,
            footer = modalButton('Dismiss'),
            fluidRow(
              column(
                width = 12,
                p('Check the following settings. One or more is incorrect'),
                tags$ul(
                  tags$li('Blocks: ' %&% paste(blocks, collapse = ', ')),
                  tags$li('Channels: ' %&% rave:::deparse_selections(channels)),
                  tags$li('Sample Rate: ' %&% srate),
                  tags$li(ifelse(length(error), 'Notch Filter Frequencies: ' %&% error, '(Notch filter frequencies are correct.)'))
                )
              )
            )
          )
        )
      }

    })



    observeEvent(input$ok, {
      # do it!
      local_data$is_notch = TRUE
      showNotification(p('Notch filter has started...'), type='message')

      utils$apply_notch(
        centers = local_data$center_freqs,
        widths = local_data$band_width
      )

      utils$reset()

      local_data$is_notch = FALSE
      showNotification(p('Notch filter finished'), type='message')
      removeModal()
    })



    plot_current_signal = function(signal_only, sc = NULL, main = NULL,...){
      chl = as.integer(input$chl)
      validate(need(!is.null(local_data$sc), 'No data found'))
      srate = utils$get_srate()
      ctype = local_data$chl_type

      col = COLOR_SCHEME[ctype, ]
      ctext = CHANNEL_TYPES[ctype]

      winlen = round(get_val(input$winlen, default = ceiling(srate * 2)))
      noverlap = winlen / 2
      nclass = round(get_val(input$nclass, default = 100))
      freq_lim = round(get_val(input$freq_lim, default = 300))
      if(!length(main)){
        main = sprintf('Notch vs. Raw - Channel %d (%s)', chl, ctext)
      }

      if(signal_only){
        if(is.null(sc)){
          srate = srate / local_data$compress_level
          diagnose_signal(
            s1 = local_data$sc,
            col = col, srate = srate, name = 'Notch', window = winlen,
            noverlap = noverlap, cex = 2, nclass = nclass, max_freq = freq_lim,
            main = main,
            try_compress = F, which = 1, ...)
        }else{
          diagnose_signal(
            s1 = sc,
            col = col, srate = srate, name = 'Notch', window = winlen,
            noverlap = noverlap, cex = 2, nclass = nclass, max_freq = freq_lim,
            main = main,
            try_compress = F, which = 1, ...)
        }

      }else{
        grid::grid.newpage()
        lay <- 1:3; dim(lay) = c(1,3)
        graphics::layout(mat = lay)
        diagnose_signal(
          s1 = local_data$s1,
          s2 = local_data$s2,
          sc = local_data$sc,
          col = col, srate = srate, name = 'Notch', window = winlen,
          noverlap = noverlap, cex = 2, nclass = nclass, max_freq = freq_lim,
          main = main,
          try_compress = F, which = 2:4)
      }
    }

    # Plots and reactives
    # plotOutput(ns('plot_signal'), height = '40vh')
    # plotOutput(ns('plot_welch'), height = '40vh')
    # plotOutput(ns('plot_welch_log'), height = '40vh')
    # plotOutput(ns('plot_hist'), height = '40vh')
    output$plot_signal <- renderPlot({
      plot_current_signal(TRUE)
    })


    output$plot_signal_others <- renderPlot({
      plot_current_signal(FALSE)
    })

    output$plot_signal_only <- renderPlot({
      plot_current_signal(TRUE)
    })

    observeEvent(input$notch_brush, {
      e = input$notch_brush
      max_time = local_data$max_time
      local_data$new_sel = NULL
      if(!is.null(e) && max_time > 0){
        xlim = c(
          max(min(e$xmin, max_time - 0.5, e$xmax - 0.5), 0),
          max(min(e$xmax, max_time), 0.5)
        )
        local_data$new_sel = xlim
      }
    })

    output$plot_signal_details <- renderPlot({
      xlim = local_data$tbl_sel
      xlim %?<-% local_data$new_sel
      validate(need(length(xlim), 'Select on plot or table...'))
      if(length(xlim)){
        srate = utils$get_srate()
        ind = round(xlim * srate)
        s1 = isolate(local_data$s1)
        sc = isolate(local_data$sc)
        boundary = 3 * sd(sc)
        sc = s1[ind[1]:ind[2]]
        plot_current_signal(TRUE, sc = sc, start_time = xlim[1], boundary = boundary,
                            main = paste(sprintf('%.2f', xlim), collapse = ' - ') %&% 'Seconds')
      }

    })



    output$signal_time <- DT::renderDataTable({
      tbl = local_data$excluded_time
      tbl %?<-% data.frame()
      xlim = local_data$new_sel

      if(!is.null(xlim)){
        # find if exists
        if(!(xlim[1] %in% tbl$Start && xlim[2] %in% tbl$End)){
          tbl = rbind(
            data.frame(Start = xlim[1], End = xlim[2], Staged = FALSE),
            tbl
          )
        }
      }
      if(nrow(tbl)){
        tbl = formatRound(DT::datatable(
          tbl, rownames = F,
          selection = list(mode = 'single', target = 'row')
        ), columns=c('Start', 'End'), digits=3)
      }
      tbl
    }, server = FALSE)

  }

  return(list(
    body = body,
    server = server
  ))
}




###################### version 2
#' @import stringr
#' @import signal
#' @import rhdf5
#' @import stringr
#' @import magrittr
#' @import future
#' @export
bulk_notch2 <- function(
  project_name, subject_code, blocks, channels, srate, ...
){
  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir'))
  channel_file = function(chl){
    cfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
    return(cfile)
  }

  progress = progress('Apply Notch filters...', max = length(channels) )
  on.exit({progress$close()})

  lapply_async(channels, function(chl){
    save = channel_file(chl)
    for(block_num in blocks){

      # import signal
      signal = pre_import_matlab(subject_code, project_name, block_num, chl)


      # Notch filter
      filtered = notch_channel(signal, srate)


      # save the original signal
      name = sprintf("raw/%s", block_num)
      save_h5(signal, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # save the filtered signal
      name = sprintf("notch/%s", block_num)
      save_h5(filtered, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # Also save CAR identical to filtered (This is a historical problem)
      # Our sequence was notch -> CAR -> wavelet
      # However, I think wavelet can be a head of CAR since conv is linear
      # If we calc ref after wavelet, I don't want to change wavelet code - too complicated
      # So instead, alloc space for CAR (Do CAR as if the refs are 0)
      name = sprintf('/REF/%s', block_num)
      rave:::save_h5(filtered, file = save, name = name, chunk = 1024, replace = T)
      H5close()

      # # save the compressed signal [2]
      # name = sprintf("notch/compress2/%s", block_num)
      # compressed = rave::decimate_fir(x = filtered, q = 2)
      # save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      # H5close()

      # save the compressed signal [5]
      name = sprintf("notch/compress5/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 5)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

      # # save the compressed signal [10]
      # name = sprintf("notch/compress10/%s", block_num)
      # compressed = rave::decimate_fir(x = filtered, q = 10)
      # save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      # H5close()

      # save the compressed signal [20]
      name = sprintf("notch/compress20/%s", block_num)
      compressed = rave::decimate_fir(x = filtered, q = 20)
      save_h5(compressed, file = save, name = name, ctype = 'double', chunk = 1024, replace = T)
      H5close()

    }
  }, .call_back = function(i){
    chl = channels[i]
    logger('Notch filter - channel: ', chl)
    if(!is.null(progress)){
      progress$inc(message = sprintf('Channel - %d', chl))
    }
  })

}


#' @export
cache_raw_voltage <- function(project_name, subject_code, blocks, electrodes, ...){
  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir'))
  # cfile = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
  # if(file.exists(cfile)){
  #   unlink(cfile)
  # }


  progress = rave:::progress('Reading raw voltage signals from MATLAB...', max = length(electrodes)+1 )
  on.exit({progress$close()})

  lapply_async(electrodes, function(e){
    cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e))
    if(file.exists(cfile)){
      unlink(cfile)
    }

    sapply(blocks, function(block_num){
      s = pre_import_matlab(subject_code, project_name, block_num, e)
      # save s to cfile - /raw/block_num
      save_h5(as.vector(s), cfile, name = sprintf('/raw/%s', block_num), chunk = 1024, replace = T)
      NULL
    }, simplify = F, USE.NAMES = T) ->
      re
    return(re)
  }, .call_back = function(i){
    e = electrodes[i]
    progress$inc(message = sprintf('Electrode - %d', e))
  }) ->
    re

  invisible()
}



