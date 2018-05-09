# Bulk wavelet

bulk_wavelet <- function(
  project_name, subject_code, blocks, channels, srate, target_srate = 100,
  frequencies = seq(4, 200, by = 4), wave_num = c(3,14), compress = 1, replace = F, save_original = F,
  ncores = future::availableCores() - 2, plan = NULL, ...
){
  # This function takes long to execute, parallel is highly recommended
  # However, simply parallel the process will cause IO error
  # solution is to use futures package and use multiprocess
  # Remember you can't edit one h5 file via different session in R at the same time
  # therefore even though the process is paralleled, each single h5 file is
  # handled in single process

  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir', 'cache_dir'))


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
    cfile = file.path(dirs$cache_dir, sprintf('%d.h5', chl))
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
    cfile = file.path(dirs$cache_dir, sprintf('%d.h5', chl))
    for(block_num in blocks){
      logger('Performing wavelet - channel: ', chl)

      save = channel_file(chl)

      s = rhdf5::h5read(save, name = sprintf('CAR/%s', block_num))

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
      cfile = file.path(dirs$cache_dir, sprintf('%d.h5', chl))
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
