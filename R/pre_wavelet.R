#' @export
rave_pre_wavelet3 <- function(module_id = 'OVERVIEW_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    shinydashboard::box(
      width = sidebar_width,
      title = 'Wavelet Parameters',
      uiOutput(ns('wavelet_inputs1'))
    ),
    shinydashboard::box(
      width = 12 - sidebar_width,
      title = 'Wavelet Kernels',
      plotOutput(ns('wave_windows_plot'), height = '80vh')
    )
  )

  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      is_wavelet = FALSE
    )

    output$wavelet_inputs1 <- renderUI({
      user_data$reset
      validate(
        need(utils$has_subject(), 'Load subject first.'),
        need(utils$is_notch_filtered(), 'Apply notch filter first.')
      )
      channels = utils$get_channels()
      vc_txt = rave:::deparse_selections(channels)
      max_core = max(1, rave_options('max_worker'), future::availableCores())
      srate = utils$get_srate()

      tagList(
        textInput(ns('wave_electrodes'), 'Channels:', value = vc_txt, placeholder = 'Select at least one electrode.'),
        sliderInput(ns('freq_range'), 'Frequency Range (Hz):', value = c(2,200), step = 1L, round = TRUE, min = 1L, max = 300L),
        numericInput(ns('freq_step'), 'Frequency Step Size (Hz): ', value = 2, step = 1L, min = 1L),
        sliderInput(ns('wave_num'), 'Number of Wavelet Cycles: ', value = c(3,20), step = 1L, min = 1L, max = 30L, round = T),
        numericInput(ns('target_srate'), 'Target Sample Rate', value = 100, min = 10L, max = srate, step = 1L),
        numericInput(ns('ncores'), 'Parallel, Number of Cores:', value = future::availableCores(), min = 1L, max = max_core, step = 1L),
        actionButton(ns('do_wavelet'), 'Run Wavelet')
      )
    })

    output$wave_windows_plot <- renderPlot({
      wave_num = input$wave_num
      freq_range = input$freq_range
      fstep = input$freq_step
      srate = utils$get_srate()

      validate(
        need(length(wave_num) > 0, 'Specify number of wavelet cycles'),
        need(length(freq_range) > 0 && freq_range[1] < freq_range[2], 'Specify valid frequency range'),
        need(length(fstep) > 0 && fstep > 0, 'Specify valid frequency steps'),
        need(length(srate) > 0 && srate > 0, 'Wait, have you loaded subject yet?')
      )
      freqs = seq(freq_range[1], freq_range[2], by = fstep)
      wavelet_kernels(freqs, srate, wave_num)
    })

    observeEvent(input$cancel, {
      if(local_data$is_wavelet){
        showNotification(p('Wavelet is running. This may take a while. Please wait till finished. Grab a cup of coffee, or take a nap, or go outside to enjoy your life. Life is more than a wavelet, don\'t dit here and wait for it.'), duration = 20, type = 'message')
      }else{
        removeModal()
      }

    })

    observeEvent(input$ok, {
      w_channels = rave:::parse_selections(input$wave_electrodes)
      channels = utils$get_channels()
      w_channels = w_channels[w_channels %in% channels]
      frange = round(input$freq_range)
      fstep = max(1, round(input$freq_step))
      w_freqs = seq(frange[1], frange[2], by = fstep)

      showNotification(p('Wavelet start!'), type = 'message')
      local_data$is_wavelet = TRUE

      utils$apply_wavelet(
        channels = w_channels,
        target_srate = round(input$target_srate),
        frequencies = w_freqs,
        wave_num = round(input$wave_num),
        ncores = round(input$ncores)
      )


      local_data$is_wavelet = FALSE
      showNotification(p('Wavelet finished!'), type = 'message')
      removeModal()


    })

    observeEvent(input$do_wavelet, {
      # check
      w_channels = rave:::parse_selections(input$wave_electrodes)
      channels = utils$get_channels()
      w_channels = w_channels[w_channels %in% channels]
      frange = round(input$freq_range)
      fstep = max(1, round(input$freq_step))
      w_freqs = seq(frange[1], frange[2], by = fstep)

      if(length(w_channels) && length(w_freqs) > 1){
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
                p('Apply wavelet to the following channels:'),
                tags$blockquote(rave:::deparse_selections(w_channels)),
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
                  tags$li('Valid channels: ' %&% rave:::deparse_selections(w_channels)),
                  tags$li('Frequency bands: ' %&% paste(frange, collapse = '-') %&% ' Hz with step of ' %&% fstep %&% ' Hz.')
                )
              )
            )
          )
        )
      }
    })

  }

  return(list(
    body = body,
    server = server
  ))
}





# Bulk wavelet

bulk_wavelet <- function(
  project_name, subject_code, blocks, channels, srate, target_srate = 100,
  frequencies = seq(4, 200, by = 4), wave_num = c(3,14), compress = 1, replace = F, save_original = F,
  ncores = future::availableCores() - 2, plan = NULL, save_dir = 'cache_dir', filename = '%d.h5', reference_name = 'CAR', ...
){
  # This function takes long to execute, parallel is highly recommended
  # However, simply parallel the process will cause IO error
  # solution is to use futures package and use multiprocess
  # Remember you can't edit one h5 file via different session in R at the same time
  # therefore even though the process is paralleled, each single h5 file is
  # handled in single process

  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir', save_dir))


  channel_file = function(chl){
    cfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
    return(cfile)
  }
  # save meta info
  rave:::save_meta(
    data = data.frame(Frequency = frequencies), meta_type = 'frequencies',
    project_name = project_name, subject_code = subject_code
  )

  for(chl in channels){
    cfile = file.path(dirs[[save_dir]], sprintf(filename, chl))
    # remove if replace = T
    if(replace && file.exists(cfile)){
      unlink(cfile)
    }
  }


  # split the work load by channels

  nrows = ceiling(length(channels) / ncores)

  schedule = matrix(rep(NA, ncores * nrows), ncol = ncores); schedule[1:length(channels)] = channels


  progress = progress('Wavelet in Progress...', max = max(length(channels), 1))
  on.exit({progress$close()})
  if(length(channels) <= 0){
    return()
  }
  logger('Time to grab a cup of coffee/go home.', level = 'INFO')
  lapply_async(channels, function(chl){
    require(rave)
    require(signal)
    require(rhdf5)
    require(stringr)
    cfile = file.path(dirs[[save_dir]], sprintf(filename, chl))
    for(block_num in blocks){
      logger('Performing wavelet - channel: ', chl)

      save = channel_file(chl)

      s = rhdf5::h5read(save, name = sprintf('/%s/%s', reference_name, block_num))

      if(compress > 1){
        s = rave::decimate_fir(s, compress)
      }
      gc()
      re = rave::wavelet(s, freqs = frequencies, srate = srate / compress, wave_num = wave_num)

      cname_coef = sprintf('wavelet/coef/%s', block_num)
      cname_power = sprintf('wavelet/power/%s', block_num)
      cname_phase = sprintf('wavelet/phase/%s', block_num)
      cname_cumsum = sprintf('wavelet/cumsum/%s', block_num)

      if(save_original){
        rhdf5::H5close()
        save_h5(re$coef, file = save, name = cname_coef,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')

        save_h5(re$power, file = save, name = cname_power,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')

        save_h5(re$phase, file = save, name = cname_phase,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      }
      rhdf5::H5close()


      # down-sample power and phase to 100Hz
      q = srate / target_srate / compress
      ind = seq(1, ncol(re$power), by = q)
      power = re$power[, ind]
      cfile = file.path(dirs[[save_dir]], sprintf(filename, chl))
      save_h5(power, file = cfile, name = cname_power,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      rhdf5::H5close()

      # coef
      coef = re$phase[, ind]
      save_h5(coef, file = cfile, name = cname_coef,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      rhdf5::H5close()

      # phase
      phs = re$phase[, ind]
      save_h5(phs, file = cfile, name = cname_phase,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      rhdf5::H5close()

      # cumsum
      cumsum = t(apply(power, 1, cumsum))
      save_h5(cumsum, file = cfile, name = cname_cumsum,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      rhdf5::H5close()

      # save time_points info
      if(chl == channels[1]){
        tp = rave::load_meta('time_points', project_name = project_name, subject_code = subject_code)
        if(is.null(tp) || !block_num %in% tp$Block){
          tp = rbind(tp,
                     data.frame(
                       Block = paste(block_num),
                       Time = seq(1, ncol(power)) / target_srate,
                       stringsAsFactors = F
                     ))
          rave::save_meta(tp, 'time_points', project_name = project_name, subject_code = subject_code)
        }
      }
    }
  }, .call_back = function(i){
    chl = channels[i]
    logger('Performing wavelet - channel: ', chl)
    progress$inc(sprintf('Channel - %d', chl))
  }, .ncores = ncores)



}
