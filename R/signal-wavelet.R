# Signal processing - Wavelet

#' Returns wavelets to be used for wavelet function
#' @param freqs vector of center frequencies for decomposition
#' @param srate sample rate (in Hz)
#' @param wave_num desired number of cycles in wavelet (typically 3-20 for frequencies 2-200).
wavelet_kernels <- function(freqs, srate, wave_num){
  srate = round(srate);
  # calculate wavelet cycles for each frequencies
  if(length(wave_num) != length(freqs)){
    # calculate wavelet cycles for each frequencies
    ratio = (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
    wavelet_cycles = exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num)))
  }else{
    wavelet_cycles = wave_num
  }
  f_l = length(freqs)
  
  
  # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
  # sts = wavelet_cycles / (2 * pi * freqs)
  # wavelet_wins = cbind(-3 * sts, 3 * sts)
  
  lapply(1:f_l, function(ii){
    fq = freqs[ii]
    cycles = wavelet_cycles[ii]
    # standard error
    st = cycles / (2 * pi * fq)
    
    # calculate window size
    wavelet_win = seq(-3 * st, 3 * st, by = 1/srate)
    
    # half of window length
    w_l_half = (length(wavelet_win) - 1) / 2
    
    # wavelet 1: calc sinus in complex domain
    tmp_sine = exp((0+1i) * 2 * pi * fq / srate * (-w_l_half:w_l_half))
    
    # Gaussian normalization part
    A = 1/sqrt(st*sqrt(pi))
    
    # wavelet 2: calc gaussian wrappers
    tmp_gaus_win = A * exp(-wavelet_win^2/(2 * (cycles/(2 * pi * fq))^2))
    
    # wave kernel
    tmp_wavelet = tmp_sine * tmp_gaus_win
    
    tmp_wavelet
  }) ->
    fft_waves
  
  max_l = as.integer(max(sapply(fft_waves, length)) + 0.1 * srate)
  sapply(fft_waves, function(s){
    l = (max_l - length(s))
    pre = floor(l / 2)
    post = ceiling(l / 2)
    c(rep(NA, pre), s, rep(NA, post))
  }) ->
    s
  s_re = Re(s)
  s_im = Im(s)
  ind = exp(seq(log(min(freqs)), log(max(freqs)), length.out = 10))
  sapply(ind, function(i){
    which.min(abs(i - freqs))
  }) ->
    ind
  ind = unique(sort(ind))
  gap = (1:length(ind)) * 1.8 * max(abs(s_re), na.rm = T)
  tmp_re = t(t(s_re[, ind]) + gap)
  tmp_im = t(t(s_im[, ind]) + gap)
  tmp = rbind(tmp_re)
  x_all = (1:max_l) / srate; x_re = x_all; x_im = x_re + max(x_all)
  grid::grid.newpage()
  lay <- rbind(c(1,1),
               c(2,3))
  
  graphics::layout(mat = lay)
  graphics::matplot(y = tmp_re, x = x_re, type='l', col = 'red',
          xlim = c(0, max(x_im)), ylim = c(min(tmp_re, na.rm = T), max(gap) + 1.5 * min(gap)),
          lty = 1, cex.lab = 1.4, cex.main = 1.6, xlab = 'Wavelet Length (seconds)', cex.axis = 1.2,
          ylab = 'Frequency (Hz)', main = 'Wavelet Kernels (Real & Imaginary)', yaxt="n", xaxt="n")
  
  graphics::matlines(y = tmp_im, x = x_im, type='l', col = 'red', lty = 1)
  
  n_halftickers = 7
  x_actual = c(x_re, x_im)
  x_label = c(x_all, x_all) - mean(x_all)
  xind = seq(1, length(x_re), length.out = n_halftickers); xind = c(xind, xind + length(x_re))
  xind = as.integer(xind[-n_halftickers])
  x_label = x_label[xind]; x_label[n_halftickers] = abs(x_label[n_halftickers])
  x_label = sprintf('%.2f', x_label)
  x_label[n_halftickers] = paste0('\u00B1', x_label[n_halftickers])
  x_text = stats::median(x_actual)
  
  graphics::axis(1, at=x_actual[xind], x_label, cex.axis = 1.2)
  graphics::axis(2, at=gap, freqs[ind], cex.axis = 1.2, las = 1)
  graphics::abline(h = gap, col = 'grey80', lty = 2)
  leading_mod = sapply(ind, function(ii){
    x = s[,ii]
    cycles = wavelet_cycles[ii]
    x = x[!is.na(x)] #Mod(x[1]) / max(Mod(x)) * 100  #= 1.111%
    c(length(x) / srate, cycles)
  })
  graphics::text(x = x_text, y = gap, '|', cex = 1.2)
  graphics::text(x = x_text, y = gap, sprintf('%.3f', leading_mod[1,]), cex = 1.2, pos = 2)
  graphics::text(x = x_text, y = gap, sprintf('%.2f', leading_mod[2,]), cex = 1.2, pos = 4)
  y_mini_title = min(gap) + max(gap)
  graphics::text(x = x_text, y = y_mini_title, '|', cex = 1.4)
  graphics::text(x = x_text, y = y_mini_title, 'Wave Length', cex = 1.4, pos = 2)
  graphics::text(x = x_text, y = y_mini_title, '# of Cycles', cex = 1.4, pos = 4)
  
  # plot freq over wavelength and wave cycles
  wave_len = sapply(fft_waves, length) / srate
  graphics::plot(freqs, wave_len, type = 'l', ylab = 'Wavelet Length (seconds)',
       xlab = 'Frequency (Hz)', main = 'Wavelet Length | Frequency',
       las = 1, cex.lab = 1.4, cex.main = 1.6, cex.axis = 1.2, col = 'grey80')
  graphics::points(freqs, wave_len, col = 'red', pch = 4)
  
  graphics::plot(freqs, wavelet_cycles, type = 'l', ylab = 'Wavelet Cycle',
       xlab = 'Frequency (Hz)', main = 'Wavelet Cycle | Frequency',
       las = 1, cex.lab = 1.4, cex.main = 1.6, cex.axis = 1.2, col = 'grey80')
  graphics::points(freqs, wavelet_cycles, col = 'red', pch = 4)
  
  invisible(fft_waves)
}


# Last edited: Zhengjia Wang - Mar, 2018
# Changes:
#   1. Uses fftwtools instead of native functions
#   manually calculate convolutions
#   Save 50% runtime
#   2. Adjusted window length
#   3. Return power spec instead of spectrum
#   4. Use mvfftw instead of fftw, speed up by another 50%


#' @title Wavelet Transformation With Phase
#' @description The code was translated from Matlab script written by 
#' Brett Foster, Stanford Memory Lab, 2015 with permission to use in `RAVE`.
#' @param data - vector of time series to be decomposed
#' @param freqs - vector of center frequencies for decomposition
#' @param srate - sample rate (in Hz)
#' @param wave_num - desired number of cycles in wavelet (typically 3-20 for 
#' frequencies 2-200).
#' @param demean - whether to remove the mean of data first?
#' @details
#' Decompose time series data into time-frequency
#' representation (spectral decomposition) using wavelet transform. Employs
#' "Morlet" wavelet method (gaussian taper sine wave) to obtain the analytic
#' signal for specified frequencies (via convolution).
#'
#' @export
wavelet <- function(data, freqs, srate, wave_num, demean = TRUE){
  srate = round(srate);
  if(length(wave_num) != length(freqs)){
    # calculate wavelet cycles for each frequencies
    ratio = (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
    wavelet_cycles = exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num)))
  }else{
    wavelet_cycles = wave_num
  }
  
  # Instead of using fixed wave_cycles, use flex cycles
  # lower num_cycle is good for low freq, higher num_cycle is good for high freq.
  # wavelet_cycles = wave_num;
  # lowest_freq = freqs[1];
  
  f_l = length(freqs)
  d_l = length(data)
  
  # normalize data, and fft
  if(demean){
    fft_data = fftwtools::fftw_r2c(data - mean(data))
  }else{
    fft_data = fftwtools::fftw_r2c(data)
  }
  
  
  # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
  # sts = wavelet_cycles / (2 * pi * freqs)
  # wavelet_wins = cbind(-3 * sts, 3 * sts)
  
  sapply(1:f_l, function(ii){
    fq = freqs[ii]
    cycles = wavelet_cycles[ii]
    # standard error
    st = cycles / (2 * pi * fq)
    
    # calculate window size
    wavelet_win = seq(-3 * st, 3 * st, by = 1/srate)
    
    # half of window length
    w_l_half = (length(wavelet_win) - 1) / 2
    
    # wavelet 1: calc sinus in complex domain
    tmp_sine = exp((0+1i) * 2 * pi * fq / srate * (-w_l_half:w_l_half))
    
    # Gaussian normalization part
    A = 1/sqrt(st*sqrt(pi))
    
    # wavelet 2: calc gaussian wrappers
    tmp_gaus_win = A * exp(-wavelet_win^2/(2 * (cycles/(2 * pi * fq))^2))
    
    # wave kernel
    tmp_wavelet = tmp_sine * tmp_gaus_win
    
    # padding
    w_l = length(tmp_wavelet)
    n_pre  = ceiling(d_l / 2) - floor(w_l/2)
    n_post = d_l - n_pre - w_l
    wvi = c(rep(0, n_pre), tmp_wavelet, rep(0, n_post))
    fft_wave = Conj(fftwtools::fftw_c2c(wvi))
    
    fft_wave
  }) ->
    fft_waves
  
  
  # Convolution Notice that since we don't pad zeros to data
  # d_l is nrows of wave_spectrum. However, if wave_spectrum is changed
  # we should not use d_l anymore. instead, use nrow(fft_waves)
  wave_len = nrow(fft_waves)
  # wave_spectrum = apply(fft_waves * fft_data, 2, fftwtools::fftw_c2c, inverse = 1) / wave_len
  wave_spectrum = fftwtools::mvfftw_c2c(fft_waves * fft_data, inverse = 1) / wave_len
  
  # according to profile, fft_waves takes very large memory (3GB / 100 frequencies)
  rm(fft_waves); gc()
  
  ind = (1:(ceiling(wave_len / 2)))
  # Normalizing and re-order
  wave_spectrum = t(rbind(wave_spectrum[-ind, ], wave_spectrum[ind, ])) / sqrt(srate / 2)
  
  
  
  # extract amplitude and phase data
  # BF: apply amplitude normalization in function?
  coef = Mod(wave_spectrum);
  power = coef^2
  phase = Arg(wave_spectrum);
  
  rm(wave_spectrum); gc()
  
  list(
    coef = coef,
    power = power,
    phase = phase
  )
}






# Bulk run wavelet (deprecated)
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
  save_meta(
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
  catgl('Time to grab a cup of coffee/go home.', level = 'INFO')
  lapply_async(channels, function(chl){
    do.call('require', list(package = 'rave', character.only = T))
    do.call('require', list(package = 'stringr', character.only = T))
    cfile = file.path(dirs[[save_dir]], sprintf(filename, chl))
    for(block_num in blocks){
      catgl('Performing wavelet - channel: ', chl)
      
      save = channel_file(chl)
      
      s = load_h5(save, name = sprintf('/%s/%s', reference_name, block_num), ram = T)
      
      if(compress > 1){
        s = decimate_fir(s, compress)
      }
      gc()
      re = wavelet(s, freqs = frequencies, srate = srate / compress, wave_num = wave_num)
      
      cname_coef = sprintf('wavelet/coef/%s', block_num)
      cname_power = sprintf('wavelet/power/%s', block_num)
      cname_phase = sprintf('wavelet/phase/%s', block_num)
      cname_cumsum = sprintf('wavelet/cumsum/%s', block_num)
      
      if(save_original){
        save_h5(re$coef, file = save, name = cname_coef,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
        
        save_h5(re$power, file = save, name = cname_power,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
        
        save_h5(re$phase, file = save, name = cname_phase,
                chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      }
      
      
      # down-sample power and phase to 100Hz
      q = srate / target_srate / compress
      ind = seq(1, ncol(re$power), by = q)
      power = re$power[, ind]
      cfile = file.path(dirs[[save_dir]], sprintf(filename, chl))
      save_h5(power, file = cfile, name = cname_power,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      
      # coef
      coef = re$coef[, ind]
      save_h5(coef, file = cfile, name = cname_coef,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      
      # phase
      phs = re$phase[, ind]
      save_h5(phs, file = cfile, name = cname_phase,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      
      # cumsum
      cumsum = t(apply(power, 1, cumsum))
      save_h5(cumsum, file = cfile, name = cname_cumsum,
              chunk = c(length(frequencies), 1024), replace = replace, ctype = 'double')
      
      # save time_points info
      if(chl == channels[1]){
        tp = load_meta('time_points', project_name = project_name, subject_code = subject_code)
        if(is.null(tp) || !block_num %in% tp$Block){
          tp = rbind(tp,
                     data.frame(
                       Block = paste(block_num),
                       Time = seq(1, ncol(power)) / target_srate,
                       stringsAsFactors = F
                     ))
          save_meta(tp, 'time_points', project_name = project_name, subject_code = subject_code)
        }
      }
    }
  }, .call_back = function(i){
    chl = channels[i]
    catgl('Performing wavelet - channel: ', chl)
    progress$inc(sprintf('Channel - %d', chl))
  }, .ncores = ncores)
  
  
  
}

