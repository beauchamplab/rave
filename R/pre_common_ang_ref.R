# CAR

pre_car2 <- function(
  project_name, subject_code, blocks, validchans,
  exclchan = NULL, progress = NULL
){
  dirs = get_dir(subject_code = subject_code, project_name = project_name)
  pre_dir = dirs$preprocess_dir
  nvalid = length(validchans)
  nexc = length(exclchan)
  nblocks = length(blocks)
  show_progress = function(
    inc = 1 / (nblocks * (nvalid + nexc+ 1) * 2),
    details = ''){
    if(!is.null(progress)){
      progress$inc(inc, detail = details)
    }else{
      logger(details)
    }
  }
  # get all signals
  all_c = c(validchans, exclchan)
  for(b in blocks){
    H5close()
    all_channels = sapply(all_c, function(chl){
      show_progress(details = sprintf('Reading: Block %s - Channel %d', b, chl))
      fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
      s = h5read(fname, name = sprintf('/notch/%s', b))
      s
    })
    H5close()
    show_progress(details = sprintf('Calculating block - %s', b))
    avg = rowMeans(all_channels[, 1:nvalid, drop = FALSE])
    all_channels = all_channels - avg
    for(ii in 1:length(all_c)){
      chl = all_c[ii]
      show_progress(details = sprintf('Writing: Block %s - Channel %d', b, chl))
      fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
      s = all_channels[,ii]
      rave:::save_h5(
        x = s, file = fname, name = sprintf('/CAR/%s', b), chunk = 1024, replace = T
      )
      H5close()
      # Compress 20
      s = rave::decimate_fir(s, 20)
      rave:::save_h5(
        x = s, file = fname, name = sprintf('/CAR/compress20/%s', b), chunk = 1024, replace = T
      )
      H5close()
    }
  }
}


car_avg <- function(project_name, subject_code, blocks, channels, envir = NULL, name = NULL){

  dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = 'preprocess_dir')
  ss = NULL
  if(is.environment(envir)){
    if(exists('.all_signals', envir = envir)){
      ss = get('.all_signals', envir = envir)
    }else{
      car_f = file.path(dirs$preprocess_dir, 'car.RData')
      if(file.exists(car_f)){
        load(car_f, envir = envir)
      }
    }
  }
  if(is.null(ss)){
    # load from subject pre-dir

    ss = list()
    for(b in blocks){
      s = rhdf5::h5read(file.path(dirs$preprocess_dir, 'all_notch.h5'), name = b)
      ss[[b]] = s
    }
    if(!is.null(envir)){
      envir$.all_signals = ss
    }
  }

  re = list()
  for(b in blocks){
    signals = t(ss[[b]][channels, ])
    cars = rowMeans(scale(signals, scale = F, center = T))
    re[[b]] = cars
  }
  if(!is.null(name) && !is.null(envir)){
    assign(name, re, envir = envir)
    if(!exists('.car_params', envir = envir)){
      envir$.car_params = list()
    }
    envir$.car_params[[name]] = channels
  }
  invisible(re)
}

bulk_CAR <- function(
  project_name, subject_code, blocks, channels, srate, excluded = NULL, replace = F, save_plot = T,
  progress = NULL, ...
){
  dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = 'preprocess_dir')
  channel_file = function(chl){
    cfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
    return(cfile)
  }

  channels = sort(as.numeric(channels))

  progress_inc = 1 / length(blocks) / 3
  # Asyc plan
  rave_setup()

  for(block_num in blocks){
    # Prepare for signals
    signals = NULL

    H5close()
    if(!is.null(progress)){
      progress$inc(progress_inc, detail = sprintf('block - %s - Loading', block_num))
    }

    for(i in 1:length(channels)){

      chl = channels[i]
      logger('Loading - channel: ', chl)
      save = channel_file(chl)

      # assume the data is saved
      s = rhdf5::h5read(save, name = sprintf('notch/%s', block_num))

      if(is.null(signals)){
        signals = matrix(0, ncol = length(channels), nrow = length(s))
      }

      signals[,i] = s
    }

    if(!is.null(progress)){
      progress$inc(progress_inc, detail = sprintf('block - %s - Calculating', block_num))
    }

    car_channels = (!channels %in% excluded)
    logger('CAR Based on these channels: ', rave:::deparse_selections(channels[car_channels]))
    if(sum(car_channels) == 1){
      center = signals[,car_channels] - mean(signals[,car_channels])
    }else{
      center = rowMeans(scale(signals[,car_channels], scale = F, center = T))
    }
    CAR_signals = signals - center

    if(!is.null(progress)){
      progress$inc(progress_inc, detail = sprintf('block - %s - Saving', block_num))
    }

    for(i in 1:length(channels)){
      chl = channels[i]

      logger('Saving - channel: ', chl)
      save = channel_file(chl)
      chname = sprintf("CAR/%s", block_num)
      s = CAR_signals[,i]


      save_h5(s, file = save, name = chname, chunk = 1024, ctype = 'double', replace = replace)

      # Save CAR per-channel image
      if(save_plot){
        async({
          rave:::save_plot({
            rave:::pre_inspect(
              process_name = 'CAR',
              project_name = project_name,
              subject_code = subject_code,
              block_num = block_num,
              srate = srate,
              chls = chl,
              details = T,
              boundary = -1
            )
          }, path = file.path(dirs$pre_visual_dir, sprintf('CAR_%s_ch_%d.png', block_num, chl)))
        }, envir = environment(), plan = NULL)
      }
    }

  }

  if(save_plot){
    logger('CAR - Do NOT close R session now, plots are generated in the background', level = 'INFO')
  }

  return(invisible())
}
