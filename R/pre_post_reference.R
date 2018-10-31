#' Preprocess Module - post-reference (deprecated)
#' @param module_id internally used
#' @param sidebar_width sidebar width from 1 to 12
rave_pre_postref3 <- function(module_id = 'POSTREF_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  CHANNEL_TYPES = c(
    'Good',
    'Epilepsy',
    'Bad',
    'Exclude'
  )
  COLOR_SCHEME = rbind(
    c('black', 'grey60'),
    c('blue', 'grey60'),
    c('blue', 'grey60'),
    c('black', 'grey60')
  )
  palette = colorRampPalette(c('navy', 'white', 'red'))(1001)

  body = fluidRow(
    shinydashboard::box(
      title = 'Post Reference',
      width = sidebar_width,
      uiOutput(ns('inputui1')),
      hr(),
      uiOutput(ns('inputui2'))
    ),
    shinydashboard::tabBox(
      width = 12 - sidebar_width,
      id = ns('main_panel'),
      tabPanel(
        title = 'Overview',
        plotOutput(ns('overview_plot'), height = '80vh')
      ),
      tabPanel(
        title = 'Inspection',
        fluidRow(
          column(
            width = 8,
            plotOutput(ns('inspection_plot'), height = '30vh', click = ns('set_start'))
          ),
          column(
            width = 4,
            plotOutput(ns('volt_plot'), height = '30vh')
          ),
          column(
            width = 12,
            plotOutput(ns('wave_plot'), height = '35vh')
          ),
          column(
            width = 12,
            plotOutput(ns('phase_plot'), height = '35vh')
          )
        )
      ),
      tabPanel(
        title = 'Trial Plot',
        plotOutput(ns('trial_power_plot'), height = '40vh'),
        plotOutput(ns('trial_phase_plot'), height = '40vh')
      )
    )
  )

  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      checklevel = 0,
      reset = NULL,
      ref = NULL,
      notch = NULL,
      volt = NULL,
      power = NULL,
      phase = NULL,
      epoch = list()
    )


    local_env = new.env()

    #### Step 1 Check signal from main app and init ####
    observe({
      user_data$reset
      local_data$checklevel = utils$get_check_level()
      rm(list = names(as.list(local_env, all.names=T)), envir = local_env)

      local_env$blocks = utils$get_blocks()
      local_env$channels = utils$get_channels()
      local_data$reset = Sys.time()
    })

    observe({
      user_data$reset
      print(input$epoch_name)
      epoch = utils$load_epoch_time(input$epoch_name)
      if(!is.list(epoch)){
        epoch = list()
      }
      local_data$epoch = epoch
    })

    ##### step 2: util functions ####
    is_referenced = function(){
      local_data$reset
      cl = local_data$checklevel
      if(length(cl) == 0){
        cl = 1
      }
      cl >= 4
    }

    output$inputui1 <- renderUI({
      local_data$reset
      validate(need(is_referenced(), 'Please reference channels first.'))
      epochs = utils$get_epochs()
      epochs %?<-% ''

      tagList(
        selectInput(ns('epoch_name'), 'Epoch', choices = epochs),
        selectInput(ns('block'), 'Block', choices = local_env$blocks),
        selectInput(ns('channel'), 'Channel', choices = local_env$channels),
        div(
          actionButton(ns('prev_chl'), 'Previous Channel'),
          actionButton(ns('nxt_chl'), 'Next Channel')
        )
      )
    })

    output$inputui2 <- renderUI({
      local_data$reset
      validate(need(is_referenced(), ''))
      main_panel = input$main_panel
      wavelet_log = utils$get_wavelet_log()

      if(length(main_panel) && length(wavelet_log)){
        rg = wavelet_log$frequencies
        tsrate = wavelet_log$target_srate
        htsrate = min(round(tsrate / 2), max(rg))

        if(main_panel == 'Inspection'){
          tagList(
            numericInput(ns('start'), 'Start', min = 0, value = 0, step = 3L),
            sliderInput(ns('duration'), 'Duration', min = 1L, max = 10L, step = 1L, value = 5L),
            div(
              actionButton(ns('prev_time'), 'Previous'),
              actionButton(ns('nxt_time'), 'Next')
            ),
            hr(),
            sliderInput(ns('max_freq'), 'Max Frequency for Phase', value = htsrate, min = min(rg), max = htsrate, step = 1L),
            checkboxInput(ns('phasediff'), 'Phase Difference Plot',value = F)
          )
        }else if(main_panel == 'Trial Plot'){

          tagList(
            sliderInput(ns('freq_range'), 'Frequency Range for Power', min = min(rg), max = max(rg), value = range(rg), round = T),
            sliderInput(ns('freq_phase'), 'Frequency for Phase', value = min(rg), min = min(rg), max = htsrate, step = 1L),
            numericInput(ns('thred'), 'Plot Threshold', value = 0, min = 0)
          )

        }
      }
    })

    ##### Step3 observation #####

    output$trial_power_plot <- renderPlot({
      block = input$block
      epoch_time = NULL
      # power = local_data$median_power
      if(length(block)){
        epoch_time = local_data$epoch[[block]]
      }
      chl = input$channel
      wavelet_log = utils$get_wavelet_log()
      frange = input$freq_range

      validate(need(!is.null(epoch_time), 'No epoch found.'),
               need(length(power), 'No power data found.'),
               need(length(wavelet_log), 'No wavelet?'),
               need(length(frange) == 2, ''))

      tsrate = wavelet_log$target_srate

      thred = input$thred
      if(is_invalid(thred, .invalids = 'na') || thred <= 0){
        crop = NULL
      }else{
        crop = c(-thred, thred)
      }

      utils$snapshot_power(block = block,
                           chl = as.integer(chl), freq_range = frange,
                           epoch = input$epoch_name, crop = crop)
    })

    output$trial_phase_plot <- renderPlot({
      block = input$block
      epoch_time = NULL
      # phase = local_data$phase_all
      if(length(block)){
        epoch_time = local_data$epoch[[block]]
      }
      wavelet_log = utils$get_wavelet_log()
      fq = input$freq_phase

      validate(need(!is.null(epoch_time), 'No epoch found.'),
               need(length(power), 'No power data found.'),
               need(length(wavelet_log), 'No wavelet?'),
               need(length(fq) == 1, ''))

      tsrate = wavelet_log$target_srate
      freqs = wavelet_log$frequencies

      chl = input$channel

      utils$snapshot_phase(block = block, chl = as.integer(chl),
                           epoch = input$epoch_name, freq = fq, crop = c(-1, 1))
    })

    # load signal
    observe({
      if(is.null(input$block)){
        return()
      }
      block = input$block
      chl = input$channel
      local_data$ref =
        utils$load_signal(
          block = block,
          channel = chl,
          group = 'REF'
        )
      local_data$notch =
        utils$load_signal(
          block = block,
          channel = chl,
          group = 'notch'
        )
    })
    observe({
      if(is.null(local_data$ref) || !input$main_panel %in% c('Inspection', 'Trial Plot')){
        return()
      }
      block = input$block
      chl = as.integer(input$channel)
      start = input$start
      duration = input$duration
      wavelet_log = utils$get_wavelet_log()
      wave_srate = wavelet_log$target_srate
      srate = utils$get_srate()

      if(zero_length(block, chl, start, duration, wavelet_log, wave_srate, srate, na.rm = T)){
        return()
      }

      local_env$wave_srate = wave_srate
      local_env$srate = srate
      local_env$wavelet_log = wavelet_log

      wave_ind = floor(wave_srate * (c(0, duration) + start)) + 1
      wave_time_points = seq(wave_ind[1], wave_ind[2])
      volt_ind = floor(srate * (c(0, duration) + start)) + 1
      volt_time_points = seq(volt_ind[1], volt_ind[2])


      re = utils$load_wave_data(block = block, chl = chl, complex = F, raw = F)

      power = (re$coef) ^2
      local_data$power = power[, wave_time_points]
      local_data$phase = re$phase[, wave_time_points]
      local_data$volt = (local_data$ref)[volt_time_points]
      md = apply(power, 1, median)
      local_data$median_power = (power/md - 1) * 100
      local_data$phase_all = re$phase
    })

    observeEvent(input$prev_chl, {
      chl = as.integer(input$channel)
      channels = local_env$channels
      sel = channels < chl
      if(sum(sel)){
        updateSelectInput(session, 'channel', selected = max(channels[sel]))
      }
    })
    observeEvent(input$nxt_chl, {
      chl = as.integer(input$channel)
      channels = local_env$channels
      sel = channels > chl
      if(sum(sel)){
        updateSelectInput(session, 'channel', selected = min(channels[sel]))
      }
    })
    observeEvent(input$prev_time, {
      start = input$start
      duration = input$duration
      updateNumericInput(session, 'start', value = max(start - duration, 0))
    })
    observeEvent(input$nxt_time, {
      start = input$start
      duration = input$duration
      updateNumericInput(session, 'start', value = start + duration)
    })
    observeEvent(input$set_start, {
      e = input$set_start
      if(!is.null(input$set_start)){
        start = max(0, e$x)
        start = round(start, digits = 2)
        updateNumericInput(session, 'start', value = start)
      }
    })

    ##### Step4 Output #####
    output$overview_plot <- renderPlot({
      s1 = local_data$ref
      s2 = local_data$notch
      validate(need(!is.null(s1), 'No signal input... Reference signal first?'))

      chl = as.integer(input$channel)
      ctype_n = utils$get_channel_type(chl)
      ctype = CHANNEL_TYPES[ctype_n]
      col = COLOR_SCHEME[ctype_n, ]
      diagnose_signal(s1, s2, srate = utils$get_srate(), name = 'Ref', try_compress = T, max_freq = 300, cex = 2,
                      main = sprintf('Before vs After Reference - Channel %d (%s)', chl, ctype), col = col)
    })

    output$inspection_plot <- renderPlot({
      s1 = local_data$ref

      validate(need(!is.null(s1), 'No signal input... Reference signal first?'))

      chl = as.integer(input$channel)
      ctype_n = utils$get_channel_type(chl)
      ctype = CHANNEL_TYPES[ctype_n]
      col = COLOR_SCHEME[ctype_n, 1]
      re = diagnose_signal(s1, srate = utils$get_srate(), name = 'Ref', try_compress = T, max_freq = 300, cex = 2,
                      main = sprintf('Before vs After Reference - Channel %d (%s)', chl, ctype), col = col, which = 1)

      start = input$start
      duration = input$duration
      rect(start, -re$boundary, start + duration, re$boundary, lty = 1, col = NA, lwd = 3, border = 'red')
    })

    output$volt_plot <- renderPlot({
      vols = local_data$volt
      validate(need(!is.null(vols), ''))

      start = input$start
      duration = input$duration
      chl = as.integer(input$channel)
      diagnose_signal(vols, srate = utils$get_srate(), name = '', try_compress = FALSE, max_freq = 300, cex = 2, start_time = start,
                      main = sprintf('Subset Time (%.2f-%.2f, sec)', start, start+duration), col = 'black', which = 1, boundary = 3 * sd(local_data$ref))

    })

    output$wave_plot <- renderPlot({
      start = input$start
      duration = input$duration
      power = local_data$power
      validate(need(!is.null(power), ''))

      wavelet_log = local_env$wavelet_log
      freq = wavelet_log$frequencies
      nfreq = length(freq)

      power = apply(power, 1, function(x){
        m = median(x)
        (x / m -1) * 100
      })
      main = 'Power (normalized per frequency)'
      image(power, main = main, col = palette, cex.main = 2, useRaster = F, xaxt = 'n', yaxt = 'n',
            xlab = 'Time (Second)', ylab = 'Frequency (Hz)', cex.lab = 1.6)
      ind = pretty(seq_len(nfreq))
      ind[length(ind)] = nfreq-1
      axis(1, at = seq(0, 1, length.out = 6), labels = sprintf('%.0f', seq(start, start+duration, length.out = 6)), cex.axis = 1.4)
      axis(2, at = ind / nfreq, labels = freq[ind+1], cex.axis = 1.4, las = 1)
    })

    output$phase_plot <- renderPlot({
      # local_data = isolate(reactiveValuesToList(ld)); wavelet_log = utils$get_wavelet_log(); input=list(phasediff_lag=1, start = 80,
      # duration = 10, max_freq = 30)
      start = input$start
      duration = input$duration
      phase = local_data$phase
      validate(need(!is.null(phase), ''))

      wavelet_log = local_env$wavelet_log
      target_srate = wavelet_log$target_srate
      max_freq = input$max_freq
      plot_diff = input$phasediff

      freq = wavelet_log$frequencies



      if(plot_diff){
        delta = freq * 2 * pi / target_srate
        dif = cbind(delta, t(apply(phase, 1, diff))) - delta
        di = exp(1i * dif)
        phase = Arg(di)
        main = 'Phase plot - Difference'
        space = pi + 1
        space_mode = 'abs'
      }else{
        main = 'Phase Plot'
        space = 1
        space_mode = 'quantile'
      }

      phase = phase[freq < max_freq, , drop = F]
      freq = freq[freq < max_freq]
      nfreq = length(freq)

      re = plot_signals(phase, sample_rate = wavelet_log$target_srate, space = space, space_mode = space_mode, time_shift = start,
                   ylab = 'Frequency (Hz)', main = main, channel_names = freq, cex = 2)

      if(plot_diff){
        sds = apply(phase, 1, sd) * 3
        abline(h = re$space * seq_len(nfreq) + sds, col = 'red', lty = 2)
        abline(h = re$space * seq_len(nfreq) - sds, col = 'red', lty = 2)
      }
    })
  }


  return(list(body = body, server = server))
}
