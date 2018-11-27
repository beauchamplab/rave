# Debug
# if(F){
#   project_name = 'Complete'
#   subject_code = 'YAB'
#   epoch = 'YABa'
#   time_range = c(2,4)
#   create_local_cache(project_name, subject_code, epoch, time_range)
#   start = Sys.time()
#   re = load_local_cache(
#     project_name, subject_code, epoch, time_range = c(1,2),
#     frequency_range = NULL, electrodes = 1:84,
#     referenced = 'default', data_type = 'power'
#   )
#   Sys.time() - start
# }
NULL

#' Create local cache for fast loadings
#' @export
create_local_cache <- function(project_name, subject_code, epoch, time_range){

  cache_dir = '~/rave_data/cache_dir'


  # TODO: check data
  check_results = rave:::check_subjects2(project_name = project_name, subject_code = subject_code, quiet = T)

  # 1. get directories
  subject = Subject$new(subject_code = subject_code, project_name = project_name, strict = F)
  dirs = subject$dirs
  subject_cache_dir = file.path(cache_dir, project_name, subject_code, epoch)

  dir.create(file.path(subject_cache_dir, 'coef'), recursive = T, showWarnings = F)
  dir.create(file.path(subject_cache_dir, 'voltage'), recursive = T, showWarnings = F)
  dir.create(file.path(subject_cache_dir, 'ref'), recursive = T, showWarnings = F)

  # 2. get epoch data
  epoch_tbl = load_meta(meta_type = 'epoch', subject_id = subject$id, meta_name = epoch)
  # sort by trial orders
  epoch_tbl = epoch_tbl[order(epoch_tbl$Trial), ]

  # 3. get sample rate, time points
  srate_wave = subject$sample_rate
  time_pts_wave = seq(-time_range[1] * srate_wave, time_range[2] * srate_wave)
  srate_volt = subject$preprocess_info(key = 'srate')
  time_pts_volt = seq(-time_range[1] * srate_volt, time_range[2] * srate_volt)

  # 2. store all raw phase and power
  electrodes = subject$electrodes$Electrode
  progress = progress('Create cache', max = length(electrodes) + 1)
  on.exit({progress$close()})
  gc(); gc()
  lapply_async(electrodes, function(e){
    # read power, phase, volt
    elec = Electrode$new(subject = subject, electrode = e, preload = c('raw_power', 'raw_phase', 'raw_volt'), reference_by = 'noref', is_reference = FALSE)

    # get power
    power = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]

      time = round(row$Time * srate_wave)

      as.vector(elec$raw_power[[row$Block]][, time + time_pts_wave, drop = F])
    })

    dims = dim(power)
    dim(power) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
    power = aperm(power, c(3,1,2))

    # get phase
    phase = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]

      time = round(row$Time * srate_wave)

      as.vector(elec$raw_phase[[row$Block]][, time + time_pts_wave, drop = F])
    })

    dims = dim(phase)
    dim(phase) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
    phase = aperm(phase, c(3,1,2))

    coef = as.vector(sqrt(power) * exp(1i * phase))
    rm(power, phase)


    coef = data.frame(
      re = Re(coef),
      im = Im(coef)
    )

    fst_file = file.path(subject_cache_dir, 'coef', sprintf('%d.fst', e))
    fst::write_fst(coef, fst_file, compress = 100)
    rm(coef)

    # get voltage
    volt = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]

      time = round(row$Time * srate_volt)

      as.vector(elec$raw_volt[[row$Block]][time + time_pts_volt])
    })
    volt = data.frame(volt = as.vector(t(volt)))
    fst_file = file.path(subject_cache_dir, 'voltage', sprintf('%d.fst', e))
    fst::write_fst(volt, fst_file, compress = 100)
    rm(volt)

  }, .call_back = function(ii){
    progress$inc(sprintf('Electrode %d', electrodes[[ii]]))
    # progress$inc(sprintf('Electrode %d', e))
  })

  # save references
  ref_dir = file.path(dirs$cache_dir, 'reference')
  progress$inc('Caching References')
  if(dir.exists(ref_dir)){
    ref_files = list.files(ref_dir, pattern = '*\\.h5')
    for(f in ref_files){
      progress$inc(f)
      fpath = file.path(ref_dir, f)
      volt_h5 = load_h5(fpath, '/voltage/008', ram = T)

      # get voltage
      volt = sapply(seq_len(nrow(epoch_tbl)), function(ii){
        row = epoch_tbl[ii, ]
        time = round(row$Time * srate_volt)
        as.vector(volt_h5[time + time_pts_volt])
      })
      volt = data.frame(Volt = as.vector(t(volt)))
      fst_file = file.path(subject_cache_dir, 'ref', sprintf('%s.volt.fst', f))
      fst::write_fst(volt, fst_file, compress = 100)
      rm(volt)


      coef = load_h5(fpath, '/wavelet/coef/008', ram = T)
      coef = coef[,,1, drop = F] * exp(1i * coef[,,2, drop = F])
      coef = sapply(seq_len(nrow(epoch_tbl)), function(ii){
        row = epoch_tbl[ii, ]
        time = round(row$Time * srate_wave)
        as.vector(coef[, time + time_pts_wave, 1])
      })
      dims = dim(coef)
      dim(coef) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
      coef = aperm(coef, c(3,1,2))
      coef = as.vector(coef)
      coef = data.frame(
        re = Re(coef),
        im = Im(coef)
      )

      fst_file = file.path(subject_cache_dir, 'ref', sprintf('%s.coef.fst', f))
      fst::write_fst(coef, fst_file, compress = 100)
      rm(coef)
    }
  }

  # generate yamls

  config = list(
    project_name = 'congruency',
    subject_code = 'YAB',
    epoch = 'YABa',
    srate_volt = subject$preprocess_info('srate'),
    srate_wave = subject$sample_rate,
    frequecies = subject$frequencies$Frequency,
    time_range = c(2, 4),
    electrodes = deparse_selections(electrodes)
  )

  config$digest = digest::digest(config)
  config$epoch_signatures = digest::digest(epoch_tbl[, c('Block', 'Time', 'Trial')]) # sorted by trial!


  yaml::write_yaml(config, file.path(subject_cache_dir, 'config.yaml'), fileEncoding = 'utf-8')

  write.csv(epoch_tbl, file.path(subject_cache_dir, 'epoch.csv'), row.names = F)

  # save all references
  sapply(check_results$references, function(ref){
    tbl = load_meta('references', project_name = project_name, subject_code = subject_code, meta_name = ref)
    write.csv(tbl, file.path(subject_cache_dir, 'ref', sprintf('reference_%s.csv', ref)), row.names = F)
  })

  invisible()
}


#' Load local cache for fast importing voltage, power, and phase
#' @export
load_local_cache <- function(project_name, subject_code, epoch, time_range,
                             frequency_range = NULL, electrodes,
                             referenced = FALSE, data_type = 'voltage'){
  # project_name = 'congruency'
  # subject_code = 'YAB'
  # epoch = 'YABa'
  # time_range = c(1,2)

  # first, check if cache exists
  cache_dir = file.path('~/rave_data/cache_dir', project_name, subject_code, epoch)
  if(!dir.exists(cache_dir)){
    # cache missing
    return(invisible())
  }

  # 2, load cached configs
  tryCatch({
    config = yaml::read_yaml(file.path(cache_dir, 'config.yaml'))

    epoch_cached = read.csv(file.path(cache_dir, 'epoch.csv'), colClasses = 'character', stringsAsFactors = F)[, c('Block', 'Time', 'Trial')]
    epoch_cached$Time = as.numeric(epoch_cached$Time)
    epoch_cached$Trial = as.numeric(epoch_cached$Trial)


    # get epoch file if possible
    epoch_tbl = NULL
    subject = NULL
    try({
      epoch_tbl = load_meta('epoch', project_name = project_name, subject_code = subject_code, meta_name = epoch)
      epoch_tbl = epoch_tbl[order(epoch_tbl$Trial), c('Block', 'Time', 'Trial')]

      subject = Subject$new(project_name = project_name, subject_code = subject_code, strict = F)
    })

    if(!is.null(subject)){
      assertthat::assert_that(
        config$srate_volt == subject$preprocess_info('srate'),
        config$srate_wave == subject$sample_rate
      )
    }

    if(is.data.frame(epoch_tbl)){
      assertthat::assert_that(
        nrow(epoch_tbl) == nrow(epoch_cached),
        all(epoch_tbl$Block == epoch_cached$Block),
        all(abs(epoch_tbl$Time - epoch_cached$Time) < 0.01),
        all(epoch_tbl$Trial == epoch_cached$Trial)
      )
    }


    # check if cache contains all data
    assertthat::assert_that(
      config$epoch == epoch,
      all(time_range <= config$time_range),
      all(electrodes %in% parse_selections(config$electrodes))
    )

    # get data
    coef_dir = file.path(cache_dir, 'coef')
    volt_dir = file.path(cache_dir, 'voltage')
    ref_dir = file.path(cache_dir, 'ref')

    # get reference table
    if(isFALSE(referenced)){
      ref_table = NULL
    }else{
      ref_file = file.path(ref_dir, sprintf('reference_%s.csv', referenced))
      if(file.exists(ref_file)){
        ref_table = read.csv(ref_file, row.names = NULL, stringsAsFactors = F)
      }else{
        ref_table = load_meta('references', project_name = project_name, subject_code = subject_code, meta_name = referenced)
      }

      assertthat::assert_that(!is.null(ref_table))
    }

    re = list()
    if(any(data_type %in% c('volt', 'voltage'))){
      # load voltage
      volt = load_cached_voltage(
        cache_dir = cache_dir,
        electrodes = electrodes,
        time_range = config$time_range,
        srate_volt = config$srate_volt,
        trial = epoch_cached$Trial,
        ref_table = ref_table,
        subset_time = time_range
      )

      dims = volt$dims

      el = Tensor$new(0, dim = c(1,1,1), varnames = c('Trial', 'Time', 'Electrode'), hybrid = F)
      el$dim = dims
      el$dimnames = list(
        Trial = epoch_cached$Trial,
        Time = volt$time_points,
        Electrode = electrodes
      )
      el$set_data(NULL)
      el$use_index = T
      el$hybrid = T
      volt$data = data.frame(volt$data)
      names(volt$data) = paste0('V', seq_len(ncol(volt$data)))
      fst::write_fst(volt$data, el$swap_file)

      rm(volt)

      re[['volt']] = el
    }

    if(any(c('power', 'phase') %in% data_type)){
      coef = load_cached_wave(
        cache_dir = cache_dir,
        electrodes = electrodes,
        time_range = config$time_range,
        srate_wave = config$srate_wave,
        trial = epoch_cached$Trial,
        frequency = config$frequecies,
        ref_table = ref_table,
        data_type = data_type,
        subset_time = time_range,
        subset_freq = frequency_range
      )
      dims = coef$dims
      frequency_range %?<-% config$frequecies

      el = ECoGTensor$new(0, dim = c(1,1,1,1), varnames = c('Trial', 'Frequency', 'Time', 'Electrode'), hybrid = F)
      el$dim = dims
      el$dimnames = list(
        Trial = epoch_cached$Trial,
        Frequency = config$frequecies[config$frequecies %within% frequency_range],
        Time = coef$time_points,
        Electrode = electrodes
      )
      el$set_data(NULL)
      el$use_index = T
      el$hybrid = T

      colnames(coef$data) = paste0('V', seq_len(ncol(coef$data)))



      if(all(c('power', 'phase') %in% data_type)){
        el2 = el$clone(deep = T)
        el2$swap_file = tempfile()

        power = Mod(coef$data)^2;
        phase = Arg(coef$data)
        rm(coef)

        fst::write_fst(as.data.frame(phase), el2$swap_file)
        rm(phase)

        re[['phase']] = el2

        fst::write_fst(as.data.frame(power), el$swap_file)
        rm(power)

        re[['power']] = el
      }else{

        fst::write_fst(as.data.frame(coef$data), el$swap_file)
        if('power' %in% data_type){
          re[['power']] = el
        }else{
          re[['phase']] = el
        }
      }


    }

    re
  }, error = function(e){
    # error occured, no data returned
    NULL
  }) ->
    re
  return(re)


}


load_cached_wave = function(cache_dir, electrodes, time_range,
                            srate_wave, trial, frequency,
                            ref_table = NULL, data_type,
                            subset_time, subset_freq){
  subset_time[1] = -subset_time[1]
  progress = progress('Load from local cache - Power/Phase', max = length(electrodes) + 2)
  on.exit({progress$close()})

  coef_dir = file.path(cache_dir, 'coef')
  ref_dir = file.path(cache_dir, 'ref')
  tp = seq(- time_range[1] * srate_wave, time_range[2] * srate_wave) / srate_wave

  # calculate dimensions
  n_pt_cached = length(tp)
  subset_freq %?<-% frequency
  n_freq_cached = length(frequency)

  n_trial = length(trial)

  # create sample data
  final_dim = c(n_trial, n_freq_cached, n_pt_cached)
  idx = array(seq_len(prod(final_dim)), final_dim)
  idx = idx[, frequency %within% subset_freq, tp %within% subset_time]
  final_dim = c(dim(idx), length(electrodes))
  idx = as.vector(idx)
  idx_range = range(idx)
  idx = idx - idx_range[1] + 1




  # load all references
  ref_data = list()
  need_reference = FALSE
  if(is.data.frame(ref_table)){
    progress$inc(message = 'Prepare references')
    need_reference = TRUE
    refs = unique(ref_table$Reference)
    for(f in refs){
      ref_e = parse_selections(f)
      if(length(ref_e) == 1){
        # this is bipolar-ish reference
        d = fst::read_fst(file.path(coef_dir, sprintf('%d.fst', ref_e)),
                          from = idx_range[1], to = idx_range[2])
        ref_data[[f]] = d[idx, ]
        rm(d)
      }
      if(length(ref_e) > 1){
        # this is car-ish reference
        d = fst::read_fst(file.path(ref_dir, sprintf('%s.h5.coef.fst', f)),
                          from = idx_range[1], to = idx_range[2])
        ref_data[[f]] = d[idx, ]
        rm(d)
      }

    }
  }else{
    progress$inc(message = 'Initializing')
  }

  # check if both phase and power is needed
  need_both = all(c('power', 'phase') %in% data_type)




  data = sapply(electrodes, function(e){
    progress$inc(message = sprintf('Loading electrode %d', e))
    fst_file = file.path(coef_dir, sprintf('%d.fst', e))

    d = fst::read_fst(fst_file, from = idx_range[1], to = idx_range[2])
    d = d[idx, ]

    # trial x freq x time


    if(need_reference){
      f = ref_table$Reference[ref_table$Electrode == e]
      ref = ref_data[[f]]
      if(!is.null(ref)){
        d = d - ref[idx, ]
      }
    }
    if(need_both){
      d = d$re + (d$im) * 1i
    }else{
      if('power' %in% data_type){
        d = (d$re)^2 + (d$im)^2
      }else{
        d = atan2(d$im, d$re)
      }
    }
    return(d)
  })

  list(
    data = data,
    time_points = tp[tp %within% subset_time],
    dims = final_dim
  )
}



load_cached_voltage = function(cache_dir, electrodes, time_range, srate_volt, trial, ref_table = NULL,
                               subset_time){
  subset_time[1] = -subset_time[1]
  progress = progress('Load from local cache - Voltage', max = length(electrodes) + 2)
  on.exit({progress$close()})

  volt_dir = file.path(cache_dir, 'voltage')
  ref_dir = file.path(cache_dir, 'ref')
  tp = seq(- time_range[1] * srate_volt, time_range[2] * srate_volt) / srate_volt

  n_trials = length(trial)
  n_tp = length(tp)
  idx = array(seq_len(n_trials * n_tp), c(n_trials, n_tp))
  idx = idx[, tp %within% subset_time]
  final_dim = dim(idx)
  idx = range(idx)

  # load all references
  ref_data = list()
  need_reference = FALSE
  if(is.data.frame(ref_table)){
    progress$inc(message = 'Prepare references')
    need_reference = TRUE
    refs = unique(ref_table$Reference)
    for(f in refs){
      ref_e = parse_selections(f)
      if(length(ref_e) == 1){
        # this is bipolar-ish reference
        d = fst::read_fst(file.path(volt_dir, sprintf('%d.fst', ref_e)), from = idx[1], to = idx[2])[,1]
        ref_data[[f]] = d
      }
      if(length(ref_e) > 1){
        # this is car-ish reference
        d = fst::read_fst(file.path(ref_dir, sprintf('%s.h5.volt.fst', f)), from = idx[1], to = idx[2])[,1]
        ref_data[[f]] = d
      }
    }
  }else{
    progress$inc(message = 'Initializing')
  }




  data = sapply(electrodes, function(e){
    progress$inc(message = sprintf('Loading electrode %d', e))
    fst_file = file.path(volt_dir, sprintf('%d.fst', e))
    d = fst::read_fst(fst_file, from = idx[1], to = idx[2])[,1]

    if(need_reference){
      f = ref_table$Reference[ref_table$Electrode == e]
      ref = ref_data[[f]]
      if(!is.null(ref)){
        d = d - ref
      }
    }
    return(as.vector(d))
  })

  list(
    data = data,
    time_points = tp[tp %within% subset_time],
    dims = c(final_dim, length(electrodes))
  )
}
