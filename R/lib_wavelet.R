wavelet_kernels <- function(freqs, srate, wave_num){
  srate = round(srate);
  # calculate wavelet cycles for each frequencies
  ratio = (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
  wavelet_cycles = exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num)))

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
  matplot(y = tmp_re, x = x_re, type='l', col = 'red',
          xlim = c(0, max(x_im)), ylim = c(min(tmp_re, na.rm = T), max(gap) + 1.5 * min(gap)),
          lty = 1, cex.lab = 1.4, cex.main = 1.6, xlab = 'Wavelet Length (seconds)', cex.axis = 1.2,
          ylab = 'Frequency (Hz)', main = 'Wavelet Kernels (Real & Imaginary)', yaxt="n", xaxt="n")

  matlines(y = tmp_im, x = x_im, type='l', col = 'red', lty = 1)

  n_halftickers = 7
  x_actual = c(x_re, x_im)
  x_label = c(x_all, x_all) - mean(x_all)
  xind = seq(1, length(x_re), length.out = n_halftickers); xind = c(xind, xind + length(x_re))
  xind = as.integer(xind[-n_halftickers])
  x_label = x_label[xind]; x_label[n_halftickers] = abs(x_label[n_halftickers])
  x_label = sprintf('%.2f', x_label)
  x_label[n_halftickers] = paste0('\u00B1', x_label[n_halftickers])
  x_text = median(x_actual)

  axis(1, at=x_actual[xind], x_label, cex.axis = 1.2)
  axis(2, at=gap, freqs[ind], cex.axis = 1.2, las = 1)
  abline(h = gap, col = 'grey80', lty = 2)
  leading_mod = sapply(ind, function(ii){
    x = s[,ii]
    cycles = wavelet_cycles[ii]
    x = x[!is.na(x)] #Mod(x[1]) / max(Mod(x)) * 100  #= 1.111%
    c(length(x) / srate, cycles)
  })
  text(x = x_text, y = gap, '|', cex = 1.2)
  text(x = x_text, y = gap, sprintf('%.3f', leading_mod[1,]), cex = 1.2, pos = 2)
  text(x = x_text, y = gap, sprintf('%.2f', leading_mod[2,]), cex = 1.2, pos = 4)
  y_mini_title = min(gap) + max(gap)
  text(x = x_text, y = y_mini_title, '|', cex = 1.4)
  text(x = x_text, y = y_mini_title, 'Wave Length', cex = 1.4, pos = 2)
  text(x = x_text, y = y_mini_title, '# of Cycles', cex = 1.4, pos = 4)

  # plot freq over wavelength and wave cycles
  wave_len = sapply(fft_waves, length) / srate
  plot(wave_len, freqs, type = 'l', xlab = 'Wavelet Length (seconds)',
       ylab = 'Frequency (Hz)', main = 'Frequency vs. Wavelet Length',
       las = 1, cex.lab = 1.4, cex.main = 1.6, cex.axis = 1.2, col = 'grey80')
  points(wave_len, freqs, col = 'red', pch = '.')

  plot(wavelet_cycles, freqs, type = 'l', xlab = 'Wavelet Cycle',
       ylab = '', main = 'Frequency vs. Wavelet Cycle',
       las = 1, cex.lab = 1.4, cex.main = 1.6, cex.axis = 1.2, col = 'grey80')
  points(wavelet_cycles, freqs, col = 'red', pch = '.')

  invisible(fft_waves)
}

#' @title Wavelet Transformation With Phase
#' @details
#' Wavelet
#' Function for decomposing time series data into time-frequency
#' representation (spectral decomposition) using wavelet transform. Employs
#' Morlet wavelet method (gaussian taper sine wave) to obtain the analytic
#' signal for specified frequencies (via convolution).
#' Inputs:
#'  data - vector of time series to be decomposed
#'  freqs - vector of center frequencies for decomposition
#'  srate - sample rate (in Hz)
#'  wave_num - desired number of cycles in wavelet (typically 5-10).
#'
#' Translated from Matlab script written by Brett Foster,
#' Stanford Memory Lab, Feb. 2015
#'
#' Last edited: Zhengjia Wang - Mar, 2018
#' Changes:
#'   1. Uses fftwtools instead of native functions
#'   manually calculate convolutions
#'   Save 50% runtime
#'   2. Adjusted window length
#'   3. Return power spec instead of spectrum
#'   4. Use mvfftw instead of fftw, speed up by another 50%
#' @export
wavelet <- function(data, freqs, srate, wave_num){
  srate = round(srate);
  # calculate wavelet cycles for each frequencies
  ratio = (log(max(wave_num)) - log(min(wave_num))) / (log(max(freqs)) - log(min(freqs)))
  wavelet_cycles = exp((log(freqs) - log(min(freqs))) * ratio + log(min(wave_num)))
  # Instead of using fixed wave_cycles, use flex cycles
  # lower num_cycle is good for low freq, higher num_cycle is good for high freq.
  # wavelet_cycles = wave_num;
  # lowest_freq = freqs[1];

  f_l = length(freqs)
  d_l = length(data)

  # normalize data, and fft
  fft_data = fftwtools::fftw_r2c(data - mean(data))

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


wavelet.old <- function(data, freqs, srate, wave_num){
  srate = round(srate);
  wavelet_cycles = wave_num;
  lowest_freq = freqs[1];
  max_win_size = (1/lowest_freq)*(wavelet_cycles/2);
  max_win_size = max_win_size*1.1; #add 10% length to ensure zero is reached

  # wavelet window
  wavelet_win = seq(-max_win_size, max_win_size, by = 1/srate);

  f_l = length(freqs)
  d_l = length(data)


  ## Decompose
  # Decomposition iterates through each center frequency (wavelet), with
  # specified width, performing a convolution between the signal and complex
  # wavelet.

  freqs_mat = freqs; dim(freqs_mat) = c(f_l, 1)
  wavelet_win_mat = wavelet_win; dim(wavelet_win_mat) = c(1, length(wavelet_win))
  tmp_sines = exp(2*1i*pi*freqs_mat %*% wavelet_win_mat)
  tmp_gaus_wins = t(sapply(freqs, function(frq){
    exp(-wavelet_win^2/(2*(wavelet_cycles/(2*pi*frq))^2))
  }))

  tmp_wavelets = sapply(freqs, function(frq){

    ## create sign wave at center frequency
    tmp_sine = exp(2*1i*pi*frq*wavelet_win);

    # make gaussian window, with a width/sd = cycles
    tmp_gaus_win = exp(-wavelet_win^2/(2*(wavelet_cycles/(2*pi*frq))^2));

    # make wavelet as dot-product of sine wave and gaussian window
    tmp_wavelet = rev(tmp_sine*tmp_gaus_win)
    tmp_wavelet
  })
  s = ceiling(dim(tmp_wavelets)[1] / 2)

  # padding tmp_wavelets
  n = d_l + nrow(tmp_wavelets) - 1
  tmp_wavelets = rbind(
    tmp_wavelets,
    matrix(rep(0, (d_l - 1) * f_l), ncol = f_l)
  )

  # Calculate fft of tmp_wavelets
  # stats::fft and fftwtools::fftw_c2c are the same
  # however is much faster with large vectors,
  # Original FFT function is fastest when the length of the series being
  # transformed is highly composite. Thus stats::fft is very unstable
  # and sometimes very slow
  #
  # To test it:
  #  require(microbenchmark)
  #   microbenchmark({fft(tmp_wavelets[,1])}, times = 5)
  #   microbenchmark({fftwtools::fftw_c2c(tmp_wavelets[,1])}, times = 5)
  # for each column, apply fftw_c2c
  #
  fft_wavelets = apply(tmp_wavelets, 2, fftwtools::fftw_c2c)

  # padding data with zeros
  tmp_data = c(data, rep(0, n - d_l))

  # FFT to padded data,
  # same idea, use fftwtools::fftw_r2c(data) instead of stats::fft(data)
  # test:
  #   require(microbenchmark)
  #   microbenchmark({fftwtools::fftw_r2c(data)}, times = 50)
  #   microbenchmark({stats::fft(tmp_data)}, times = 50)
  #   hist(Mod(stats::fft(data) - fftwtools::fftw_r2c(tmp_data)))
  fft_data = fftwtools::fftw_r2c(tmp_data)

  # Magic of R
  # A * b when A is a matrix and b is a vector
  # the result C will be: each columns of A dot multiplies b:
  # C[, 1] = A[, 1] * b
  fft_prod = fft_wavelets * fft_data

  # IFFT
  # same, use either fftwtools::fftw_c2c or signal::ifft
  # no big difference
  # a = fftwtools::fftw_c2c(tmp_freq_analytic[, 1], inverse = 1) / n
  # b = signal::ifft(tmp_freq_analytic[, 1])
  # hist(Mod(a - b))
  # hist(Arg(a) - Arg(b))
  # junk = rnorm(300000)
  # microbenchmark({fftwtools::fftw_c2c(junk, inverse = 1) / n}, times = 50)
  # microbenchmark({signal::ifft(junk)}, times = 50)
  tmp_freq_analytic = apply(fft_prod, 2, fftwtools::fftw_c2c, inverse = 1) / n


  tmp_freq_analytic = t(tmp_freq_analytic[(s+1): (s + d_l), ])



  # extract amplitude and phase data
  # BF: apply amplitude normalization in function?
  tmp_amplitude = Mod(tmp_freq_analytic);
  tmp_phase = Arg(tmp_freq_analytic);


  # let's test if the faster version produces the same result
  # require(fields)
  # args = force(list(
  #   data = rnorm(200000),
  #   freqs = (1:50)*4,
  #   srate = 2000,
  #   wave_num = 7
  # ))
  # t = Sys.time()
  # re = do.call(wavelet, args = args)
  # t1 = Sys.time()
  # re.old = do.call(wavelet.old, args = args)
  # t2 = Sys.time()
  # logger('Old time: ', t2-t1,'\nNew time: ', t2-t)
  #
  # summary(as.vector(re$amp - re.old$amp))
  # image.plot(t(re$amp[, 1:1000]))
  # image.plot(t(re.old$amp[, 1:1000]))
  # image.plot(t((re$amp - re.old$amp)[, 1:1000]))


  list(
    amp = tmp_amplitude,
    phase = tmp_phase
  )
}


