rave_pre_wavelet <- function(module_id = 'WAVELET_M', sidebar_width = 2){
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
    shinydashboard::tabBox(
      id = ns('main'),
      width = 12 - sidebar_width,
      tabPanel(
        title = 'Wavelet Kernels',
        h5('Wavelet Window'),
        plotOutput(ns('wave_windows_plot'), height = '80vh')
      ),
      tabPanel(
        title = 'Post-Wavelet Inspection (Power & Phase)',
        h5('Power Plot'),
        plotOutput(ns('power_plot'), height = '40vh'),
        hr(),
        h5('Phase Plot (only display several frequencies)'),
        plotOutput(ns('phase_plot'), height = '40vh')
      )

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
      validate(need(length(vc) > 0, 'Please calculate Notch filter first.'))
      vc_txt = rave:::deparse_selections(vc)
      tagList(
        p('Here are the channels that can be transformed:'),
        tags$blockquote(vc_txt),
        hr(),
        textInput(ns('wave_electrodes'), 'Channels:', value = vc_txt, placeholder = 'Select at least one electrode.'),
        sliderInput(ns('freq_range'), 'Frequency Range (Hz):', value = c(2,200), step = 1L, round = TRUE, min = 1L, max = 300L),
        numericInput(ns('freq_step'), 'Frequency Step Size (Hz): ', value = 2, step = 1L, min = 1L),
        sliderInput(ns('wave_num'), 'Number of Wavelet Cycles: ', value = c(3,20), step = 1L, min = 1L, max = 30L, round = T),
        numericInput(ns('target_srate'), 'Target Sample Rate', value = 100, min = 10L, max = isolate(user_data$srate), step = 1L),
        checkboxInput(ns('save_original'), 'Save Original (Not Recommended)', value = FALSE),
        numericInput(ns('ncores'), 'Parallel, Number of Cores:', value = future::availableCores(), min = 1L, max = rave_options('max_worker'), step = 1L),
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
      wave_electrodes = rave:::parse_selections(input$wave_electrodes)
      if(length(wave_electrodes) == 0){
        wave_electrodes = user_data$valid_channels
      }else{
        wave_electrodes = wave_electrodes[wave_electrodes %in% user_data$valid_channels]
      }

      modal = shiny::modalDialog(
        title = 'Confirmation',
        p('The following channels will be calculated: '),
        tags$blockquote(rave:::deparse_selections(wave_electrodes)),
        p('Current CAR strategy is: '),
        tags$blockquote(user_data$subject$logger$get_or_save('CAR_plan')),
        p('Ready?'),
        footer = tagList(
          actionButton(ns("wave_cancel"), "Cancel"),
          actionButton(ns("ok"), "Sure!")
        )
      )
      showModal(modal)
    })

    observeEvent(input$wave_cancel, {
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
      wave_num = input$wave_num
      save_original = input$save_original
      ncores = max(1, round(input$ncores))
      frequencies = seq(input$freq_range[1],input$freq_range[2], by = input$freq_step)
      wave_electrodes = parse_selections(input$wave_electrodes)
      if(length(wave_electrodes) == 0){
        selected_electrodes = user_data$valid_channels
      }else{
        selected_electrodes = wave_electrodes[wave_electrodes %in% user_data$valid_channels]
      }
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

        user_data$subject$logger$save(last_wavelet = list(
          channels = user_data$valid_channels,
          car_plan = user_data$subject$logger$get_or_save('CAR_plan'),
          succeed = FALSE,
          target_srate = target_srate,
          frequencies = frequencies,
          wave_num = wave_num,
          save_original = save_original
        ))

        rave:::bulk_wavelet(
          project_name = user_data$project_name,
          subject_code = user_data$subject_code,
          blocks = user_data$blocks,
          channels = selected_electrodes,
          srate = user_data$srate,
          target_srate = target_srate,
          frequencies = frequencies,
          wave_num = wave_num,
          compress = 2,
          replace = T,
          save_original = save_original,
          ncores = ncores,
          plan = fpl
        )

        showNotification(p('Wavelet Finished!'), duration = NULL, type = 'message')
        last_wavelet = user_data$subject$logger$get_or_save('last_wavelet')
        last_wavelet[['succeed']] = TRUE
        user_data$subject$logger$save(last_wavelet = last_wavelet)
        local_data$refresh = Sys.time()
        removeModal()
        user_data$doing_wavelet = FALSE

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
      f_ind = freq <= 0.5 * local_data$last_wavelet$target_srate
      sep = max(round(sum(f_ind) / 10), 1)
      sub_ind = seq(1, sum(f_ind), by = sep)
      data = local_data$phase[sub_ind, ind]
      {

        rave:::plot_signals(data, sample_rate = local_data$last_wavelet$target_srate,
                            channel_names = paste(freq[sub_ind], 'Hz'), space = 7, start_time = start, ylab = 'Frequency')
      }
    })

    output$wave_windows_plot <- renderPlot({
      wave_num = input$wave_num
      freq_range = input$freq_range
      fstep = input$freq_step
      srate = user_data$srate

      validate(
        need(length(wave_num) > 0, 'Specify number of wavelet cycles'),
        need(length(freq_range) > 0 && freq_range[1] < freq_range[2], 'Specify valid frequency range'),
        need(length(fstep) > 0 && fstep > 0, 'Specify valid frequency steps'),
        need(length(srate) > 0, 'Wait, have you loaded subject yet?')
      )
      freqs = seq(freq_range[1], freq_range[2], by = fstep)
      wavelet_kernels(freqs, srate, wave_num)
    })
  }

  return(list(
    body = body,
    server = server
  ))
}
