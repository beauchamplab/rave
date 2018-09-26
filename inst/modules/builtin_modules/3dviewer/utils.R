get_ui = function(name){
  mask = env$masks[[name]]

  switch (name,
    'Power, Referenced' = {
      freqs = preload_info$frequencies
      time = preload_info$time_points
      tagList(
        selectInput(
          ns('power_trials'),
          'Condition',
          choices = preload_info$condition,
          selected =
            cache_input('power_trials', preload_info$condition),
          multiple = T
        ),
        sliderInput(
          ns('power_freq'),
          'Frequency',
          min = min(freqs),
          max = max(freqs),
          value = cache_input('power_freq', range(freqs))
        ),
        sliderInput(
          ns('power_bs'),
          'Baseline',
          min = min(time),
          max = max(time),
          value = cache_input('power_bs', c(-1,0)),
          step = 0.01
        ),
        actionButton(ns('gen_3d'), 'Generate 3D Animation')
      )
    },
    'Voltage, Referenced' = {
      freqs = preload_info$frequencies
      time = preload_info$time_points
      tagList(
        selectInput(
          ns('volt_trials'),
          'Condition',
          choices = preload_info$condition,
          selected =
            cache_input('volt_trials', preload_info$condition),
          multiple = T
        ),
        actionButton(ns('gen_3d'), 'Generate 3D Animation')
      )
    },
    'Phase, Raw' = {
      freqs = preload_info$frequencies
      time = preload_info$time_points
      tagList(
        selectInput(
          ns('phase_trials'),
          'Condition',
          choices = preload_info$condition,
          selected =
            cache_input('phase_trials', preload_info$condition),
          multiple = T
        ),
        selectInput(ns('phase_freq'), 'Frequency', choices = freqs, selected = cache_input('phase_freq', min(freqs))),
        actionButton(ns('gen_3d'), 'Generate 3D Animation')
      )
    }
  )
}

observeEvent(input$gen_3d, {
  name = local_data$mask_name
  mask = env$masks[[name]]
  tbl = module_tools$get_meta('trials')
  switch (
    name,
    'Voltage, Referenced' = {
      cond = input$volt_trials
      trial_ind = tbl$Trial[tbl$Condition %in% cond]
      if(!length(trial_ind)){
        showNotification(p('Condition cannot be blank'), type = 'error')
        return()
      }else{
        cache_input('volt_trials', cond, read_only = F)
      }

      dat = module_tools$get_voltage(force = T, referenced = T)
      dat = dat$subset(Trial = Trial %in% trial_ind, drop = F, data_only = T)
      dat = rutabaga::collapse(dat, c(3,2)) / dim(dat)[1]

      mask$body = dat
    },
    'Power, Referenced' = {
      cond = input$power_trials
      freqs = input$power_freq
      bs = input$power_bs


      trial_ind = tbl$Trial[tbl$Condition %in% cond]
      # step 0 check
      if(!length(trial_ind)){
        showNotification(p('Condition cannot be blank'), type = 'error')
        return()
      }else{
        cache_input('power_trials', cond, read_only = F)
      }
      if(length(bs) != 2 || bs[1] == bs[2]){
        showNotification(p('Baseline is invalid'), type = 'error')
        return()
      }else{
        cache_input('power_bs', bs, read_only = F)
      }
      if(sum(preload_info$frequencies %within% freqs)){
        cache_input('power_freq', freqs, read_only = F)
      }else{
        showNotification(p('No frequency is found'), type = 'error')
        return()
      }
      progress = progress(title = 'Generating 3D Viewer', max = 4, session = session)
      on.exit({progress$close()}, add = T, after = FALSE)
      # Step 1, baseline
      progress$inc('Calculating Baseline')

      bl = cache(list(bs, preload_info), module_tools$baseline(module_tools$get_power(), from = bs[1], to = bs[2]))


      progress$inc('Subset...')
      mask$electrodes = bl$dimnames$Electrode
      bl = bl$subset(Trial = Trial %in% trial_ind, Frequency = Frequency %within% freqs, drop = F, data_only = T)

      progress$inc('Collapse...')
      mask$body = apply(bl, c(4,3), median)
      # mask$body = rutabaga::collapse(bl, keep = c(4, 3)) / prod(dim(bl)[1:2])



    },
    'Phase, Raw' = {
      cond = input$phase_trials
      freq = as.numeric(input$phase_freq)

      tbl = module_tools$get_meta('trials')
      trial_ind = tbl$Trial[tbl$Condition %in% cond]
      # step 0 check
      if(!length(trial_ind)){
        showNotification(p('Condition cannot be blank'), type = 'error')
        return()
      }else{
        cache_input('phase_trials', cond, read_only = F)
      }

      progress = progress(title = 'Generating 3D Viewer', max = 4, session = session)
      on.exit({progress$close()}, add = T, after = FALSE)
      progress$inc('Loading Phase')
      dat = module_tools$get_phase(referenced = F)

      progress$inc('Subset')
      dat = dat$subset(Trial = Trial %in% trial_ind, Frequency = Frequency == freq, data_only = T, drop = F)
      # foreach timepoints, intercoherent coef
      progress$inc('Inter-trial Coherence...')
      dat = exp(1i * dat)
      # Collapse mean TODO implement rutabaga::collapse to support complex
      dat = apply(dat, c(4,3), mean)
      dat = Mod(dat)
      mask$body = dat
      progress$inc('Rendering...')
    }
  )

  local_data$controller_data = mask
  env$masks[[name]] = mask
})


get_data = function(dt, referenced = T, frequencies, trial_num, ...){
  trials = module_tools$get_meta('trials')
  sel = trials$Trial %in% trial_num

  freqs = module_tools$get_meta('frequencies')
  fsel = freqs$Frequency %within% frequencies
  re = NULL
  switch(
    dt,
    'phase' = {
      dat = module_tools$get_phase(force = T, referenced = referenced)
      srate = subject$sample_rate
      electrodes = dat$dimnames$Electrode
      valid = electrodes %in% subject$valid_electrodes
      name = sprintf('Phase, %s', ifelse(referenced, 'Referenced', 'Raw'))
      re = list(
        name = name,
        header = seq_along(preload_info$time_points) / srate,
        body = NULL,
        type = 'animation',
        electrodes = electrodes,
        loaded = rep(T, length(electrodes)),
        valid = valid,
        cached = TRUE
      )
    },
    'volt' = {
      dat = module_tools$get_voltage(force = T, referenced = referenced)
      srate = subject$preprocess_info('srate')
      electrodes = dat$dimnames$Electrode
      valid = electrodes %in% subject$valid_electrodes
      name = sprintf('Voltage, %s', ifelse(referenced, 'Referenced', 'Raw'))
      re = list(
        name = name,
        header = seq_len(dat$dim[2]) / srate,
        body = NULL,
        type = 'animation',
        electrodes = electrodes,
        loaded = rep(T, length(electrodes)),
        valid = valid,
        cached = TRUE
      )
    },

    'power' = {
      dat = module_tools$get_power(force = T, referenced = referenced)
      srate = subject$sample_rate
      electrodes = dat$dimnames$Electrode
      valid = electrodes %in% subject$valid_electrodes
      name = sprintf('Power, %s', ifelse(referenced, 'Referenced', 'Raw'))
      re = list(
        name = name,
        header = seq_len(dat$dim[3]) / srate,
        body = NULL,
        type = 'animation',
        electrodes = electrodes,
        loaded = rep(T, length(electrodes)),
        valid = valid,
        cached = TRUE
      )
    }
  )
  return(re)
}



check_mask_format = function(mask){
  if(!is.list(mask)){
    return(FALSE)
  }
  hdiff = setdiff(c("name","header","body","type","electrodes","valid","loaded","cached"), names(mask))
  if(length(hdiff)){
    return(FALSE)
  }
  # if(!mask$name %in% .preserved && length(mask$header) != ncol(mask$body)){
  #   return(FALSE)
  # }
  if(!mask$type %in% c('static', 'animation', 'null')){
    return(FALSE)
  }
  return(T)
}

