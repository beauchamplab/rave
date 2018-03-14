

# @deprecated
preprocess = function(
  process_name = 'notch', project_name, subject_code, block_num, chls, srate = 2000,
  frequencies = seq(4, 200, by = 4), wave_num = 7, compress = 2,
  data_dir = rave_options('data_dir')
){

  # assume save_dir exists
  subject_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'preprocess')
  vis_dir = file.path(subject_dir, 'visual')

  if(!dir.exists(subject_dir)){
    dir.create(subject_dir, recursive = T)
  }
  if(!dir.exists(vis_dir)){
    dir.create(vis_dir, recursive = T)
  }

  channel_file = function(chl){
    cfile = file.path(subject_dir, sprintf('chl_%d.h5', chl))

    if(!file.exists(cfile)){
      rhdf5::h5createFile(cfile)
      rhdf5::h5createGroup(cfile, 'raw')
      rhdf5::h5createGroup(cfile, 'notch')
      rhdf5::h5createGroup(cfile, 'CAR')
      rhdf5::h5createGroup(cfile, 'wavelet')
      rhdf5::h5createGroup(cfile, 'wavelet/phase')
      rhdf5::h5createGroup(cfile, 'wavelet/power')
      rhdf5::H5close()
    }
    return(cfile)
  }


  if(process_name == 'notch'){


    for(chl in chls){
      logger('Notch filter - channel: ', chl)
      save = channel_file(chl)

      # import signal
      signal = pre_import_matlab(subject_code, block_num, chl)


      # Notch filter
      filtered = notch_channel(signal, srate)

      # Compress data
      # compressed = compress_signal(filtered, 1,block$compress,1)


      # save the filtered signal
      chname = sprintf("notch/%s", block_num)
      rawname = sprintf("raw/%s", block_num)
      try({
        rhdf5::h5createDataset(save, rawname, length(signal), storage.mode = "double", chunk=1024, level=7)
        rhdf5::h5createDataset(save, chname, length(filtered), storage.mode = "double", chunk=1024, level=7)
      })
      rhdf5::h5write(filtered, file = save, name = chname)
      rhdf5::h5write(signal, file = save, name = rawname)

      # Save notch image
      png(
        filename = file.path(vis_dir, sprintf('notch_%s_ch_%d.png', block_num, chl)),
        width = as.numeric(rave_options('image_width')),
        height = as.numeric(rave_options('image_height'))
      )
      pre_inspect(
        process_name = process_name,
        project_name = project_name,
        subject_code = subject_code,
        block_num = block_num,
        chls = chl,
        details = T,
        boundary = -1
      )
      dev.off()
      rhdf5::H5close()
    }
  }



  if(process_name == 'CAR'){
    signals = NULL

    for(i in 1:length(chls)){

      chl = chls[i]
      logger('Loading - channel: ', chl)
      save = channel_file(chl)

      # assume the data is saved
      s = rhdf5::h5read(save, name = sprintf('notch/%s', block_num))

      if(is.null(signals)){
        signals = matrix(0, ncol = length(chls), nrow = length(s))
      }

      signals[,i] = s
    }

    CAR_signals = signals - rowMeans(scale(signals, scale = F, center = T))

    for(i in 1:length(chls)){
      chl = chls[i]

      logger('Saving - channel: ', chl)
      save = channel_file(chl)
      chname = sprintf("CAR/%s", block_num)
      s = CAR_signals[,i]

      try({
        rhdf5::h5createDataset(save, chname, length(s), storage.mode = "double", chunk=1024, level=7)
      })
      rhdf5::h5write(s, file = save, name = chname)
      rhdf5::H5close()

      # Save CAR per-channel image
      png(
        filename = file.path(vis_dir, sprintf('CAR_%s_ch_%d.png', block_num, chl)),
        width = as.numeric(rave_options('image_width')),
        height = as.numeric(rave_options('image_height'))
      )
      pre_inspect(
        process_name = process_name,
        project_name = project_name,
        subject_code = subject_code,
        block_num = block_num,
        chls = chl,
        details = T,
        boundary = -1
      )
      dev.off()
    }

    return(invisible(CAR_signals))
  }

  if(process_name == 'wavelet'){
    for(chl in chls){
      logger('Performing wavelet - channel: ', chl)

      save = channel_file(chl)

      s = rhdf5::h5read(save, name = sprintf('CAR/%s', block_num))

      s = rave::decimate_fir(s, compress)

      re = wavelet(s, freqs = frequencies, srate = srate / compress, wave_num = wave_num)

      cname_power = sprintf('wavelet/power/%s', block_num)
      cname_phase = sprintf('wavelet/phase/%s', block_num)

      try({
        dim = dim(re$amp)
        rhdf5::h5createDataset(save, cname_power, dim, storage.mode = "double", chunk=c(1, 1024), level=7, maxdims = c(max(200, dim[1]), dim[2]))
        rhdf5::h5createDataset(save, cname_phase, dim, storage.mode = "double", chunk=c(1, 1024), level=7, maxdims = c(max(200, dim[1]), dim[2]))
      })

      rhdf5::h5write(re$amp, file = save, name = cname_power)
      rhdf5::h5write(re$phase, file = save, name = cname_phase)

      rhdf5::H5close()
    }
  }



  return(invisible())
}


pre_inspect = function(
  process_name = 'notch', project_name, subject_code, block_num, chls, srate = 2000,
  compress = 20, window = 128, noverlap = 8, xlim = c(0, 300), xbins = 200, boundary = -1, ...
){
  data_dir = rave_options('data_dir')
  # assume save_dir exists
  subject_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'preprocess')

  if(!dir.exists(subject_dir)){
    dir.create(subject_dir, recursive = T)
  }

  channel_file = function(chl){
    cfile = file.path(subject_dir, sprintf('chl_%d.h5', chl))
    return(cfile)
  }

  if(process_name == 'raw'){
    chl = chls[1]
    save = channel_file(chl)
    s = rhdf5::h5read(save, name = sprintf('raw/%s', block_num))


    grid::grid.newpage()
    lay <- rbind(c(1,1,1),
                 c(2,3,4))

    graphics::layout(mat = lay)

    plot.new()
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$figure)
    vp1 <-grid::plotViewport(c(0,0,0,0))
    if(boundary < 0){
      boundary = 2* sd(s)
    }
    plot_signal(s, srate, boundary = boundary, xbins = xbins, ...) +
      ggplot2::ggtitle(sprintf('Channel - %d - Raw signal', chl)) ->
      rawplot
    print(rawplot, vp = vp1)
    grid::popViewport()

    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'blue', log = 'y', xlim = xlim)
    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'blue', log = 'xy', xlim = c(0, log10(xlim[2])))

    hist(s, nclass = 100, xlab = 'Signal Voltage - Raw', main = 'Histogram')
  }

  if(process_name == 'notch'){
    chl = chls[1]
    save = channel_file(chl)
    s = rhdf5::h5read(save, name = sprintf('notch/%s', block_num))
    r = rhdf5::h5read(save, name = sprintf('raw/%s', block_num))

    grid::grid.newpage()
    lay <- rbind(c(1,1,1),
                 c(2,3,4))

    graphics::layout(mat = lay)

    plot.new()
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$figure)
    vp1 <-grid::plotViewport(c(0,0,0,0))
    if(boundary < 0){
      boundary = 2* sd(s)
    }
    plot_signal(s, srate, boundary = boundary, xbins = xbins, ...) +
      ggplot2::ggtitle(sprintf('Channel - %d - Notch Filtered', chl)) ->
      rawplot
    print(rawplot, vp = vp1)
    grid::popViewport()

    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'red', log = 'y', xlim = xlim)
    pwelch(r, fs = srate, window = window, noverlap = noverlap, plot = 2, col = 'blue', log = 'y', xlim = xlim)
    legend('topright', c('Raw signal', 'Notch filtered'), col = c('blue', 'red'), lty = 1)


    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'red', log = 'xy', xlim = c(0, log10(xlim[2])))
    pwelch(r, fs = srate, window = window, noverlap = noverlap, plot = 2, col = 'blue', log = 'xy', xlim = c(0, log10(xlim[2])))
    legend('topright', c('Raw signal', 'Notch filtered'), col = c('blue', 'red'), lty = 1)

    hist(s, nclass = 100, xlab = 'Signal Voltage - Notch Filtered', main = 'Histogram')
  }

  if(process_name == 'CAR'){
    chl = chls[1]
    save = channel_file(chl)
    s = rhdf5::h5read(save, name = sprintf('CAR/%s', block_num))
    r = rhdf5::h5read(save, name = sprintf('notch/%s', block_num))

    grid::grid.newpage()
    lay <- rbind(c(1,1,1),
                 c(2,3,4))

    graphics::layout(mat = lay)

    plot.new()
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$figure)
    vp1 <-grid::plotViewport(c(0,0,0,0))
    if(boundary < 0){
      boundary = 2* sd(s)
    }
    plot_signal(s, srate, boundary = boundary, xbins = xbins, ...) +
      ggplot2::ggtitle(sprintf('Channel - %d - CAR', chl)) ->
      rawplot
    print(rawplot, vp = vp1)
    grid::popViewport()

    pwelch(r, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'blue', log = 'y', xlim = xlim)
    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 2, col = 'red', log = 'y', xlim = xlim)

    legend('topright', c('Notch filtered', 'CAR'), col = c('blue', 'red'), lty = 1)


    pwelch(r, fs = srate, window = window, noverlap = noverlap, plot = 1, col = 'blue', log = 'xy', xlim = c(0, log10(xlim[2])))
    pwelch(s, fs = srate, window = window, noverlap = noverlap, plot = 2, col = 'red', log = 'xy', xlim = c(0, log10(xlim[2])))

    legend('topright', c('Notch filtered', 'CAR'), col = c('blue', 'red'), lty = 1)

    hist(s, nclass = 100, xlab = 'Signal Voltage - CAR', main = 'Histogram')
  }


  if(process_name == 'CARinspect'){
    ss = NULL
    for(chl in chls){
      logger('Loading channel - ', chl)
      save = channel_file(chl)
      s = rhdf5::h5read(save, name = sprintf('notch/%s', block_num))
      # compress
      s = signal::decimate(s, compress, n = 30, ftype = "fir")

      ss = rbind(ss, s)
    }

    par(mfrow=c(1,1))
    # plot_signals(
    #   ss, sample_rate = srate / compress, col = 1, space = 0.99999, plot = FALSE, channel_names = chls
    # )
    return(invisible(ss))
  }
}






#' Concatenate final results of wavelet output and perform cumsum
#' TODO: add process of pdiode data
pre_concat <- function(project_name, subject_code, blocks, chl, srate = 1000, target_srate = 100){
  data_dir = rave_options('data_dir')

  # assume save_dir exists
  source_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'preprocess')
  target_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave')
  meta_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'meta')
  cache_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'cache')

  # we assume source_dir exists
  # but not target_dir
  if(!dir.exists(target_dir)){
    dir.create(target_dir, recursive = T)
  }
  if(!dir.exists(meta_dir)){
    dir.create(meta_dir, recursive = T)
  }
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir, recursive = T)
  }

  # locate source file
  cfile = file.path(source_dir, sprintf('chl_%d.h5', chl))

  # locate target file
  tfile = file.path(cache_dir, sprintf('chl_%d.h5', chl))
  if(file.exists(tfile)){
    file.remove(tfile)
  }
  rhdf5::h5createFile(tfile)
  power = NULL
  phase = NULL
  cumsum = NULL
  blocklen = NULL

  for(block_num in blocks){
    # load wavelet amp
    s = rhdf5::h5read(cfile, sprintf('wavelet/power/%s', block_num))
    ind = round(seq(1, ncol(s), srate / target_srate))
    s = s[, ind]
    power = cbind(power, s)
    blocklen = c(blocks, length(ind))

    s = t(apply(s, 1, cumsum))
    cumsum = cbind(cumsum, s)

    s = rhdf5::h5read(cfile, sprintf('wavelet/phase/%s', block_num))
    s = s[, ind]
    phase = cbind(phase, s)
  }
  rm(s)

  rhdf5::H5close()
  if(!'power' %in% rhdf5::h5ls(tfile)$name){
    dim = dim(power)
    rhdf5::h5createDataset(tfile, 'power', dims = dim, maxdims = dim * 2, storage.mode = "double", chunk = c(1, 1024), level = 3) # since the file size is gonna be around 150MB, i.e. 1e8, level = 3 is ok
  }

  rhdf5::h5write(power, tfile, 'power')

  rhdf5::H5close()
  if(!'phase' %in% rhdf5::h5ls(tfile)$name){
    dim = dim(phase)
    rhdf5::h5createDataset(tfile, 'phase', dims = dim, maxdims = dim * 2, storage.mode = "double", chunk = c(1, 1024), level = 3) # since the file size is gonna be around 150MB, i.e. 1e8, level = 3 is ok
  }

  rhdf5::h5write(phase, tfile, 'phase')

  rhdf5::H5close()
  # cumsum, it's critical for baseline percentage change
  if(!'cumsum' %in% rhdf5::h5ls(tfile)$name){
    dim = dim(cumsum)
    rhdf5::h5createDataset(tfile, 'cumsum', dims = dim, maxdims = dim * 2, storage.mode = "double", chunk = c(dim[1], 1), level = 3) # we will get cumsum by some columns
  }

  rhdf5::h5write(cumsum, tfile, 'cumsum')


  rhdf5::H5close()
  # finally, save block sequence
  if(!'blockinfo' %in% rhdf5::h5ls(tfile)$name){
    dim = length(blocks)
    rhdf5::h5createDataset(tfile, 'blockinfo', dims = dim, size = 255,
                           storage.mode = "character", level = 0)
  }

  rhdf5::h5write(paste(blocks), tfile, 'blockinfo')
  rhdf5::H5close()

  if(!'blocklen' %in% rhdf5::h5ls(tfile)$name){
    blocklen = as.integer(blocklen)
    dim = length(blocklen)
    rhdf5::h5createDataset(tfile, 'blocklen', dims = dim,
                           storage.mode = "integer", level = 0)
  }
  rhdf5::h5write(blocklen, tfile, 'blocklen')

  rhdf5::H5close()
}



