

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
#' Last edited: Zhengjia Wang - Feb, 2018
#' Changes:
#'   Uses fftwtools instead of native functions
#'   manually calculate convolutions
#'   Save 50% runtime
#' @export
wavelet <- function(data, freqs, srate, wave_num){
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

wavelet.old <- function(data, freqs, srate, wave_num){
  srate = round(srate);

  # wavelet cycles
  wavelet_cycles = wave_num;

  # set wavelet window size, using lowest freq, wave number and sample rate
  # high-freqs will have greater zero padding
  lowest_freq = freqs[1];
  max_win_size = (1/lowest_freq)*(wavelet_cycles/2);
  max_win_size = max_win_size*1.1; #add 10% length to ensure zero is reached

  # wavelet window
  wavelet_win = seq(-max_win_size, max_win_size, by = 1/srate);

  f_l = length(freqs)
  d_l = length(data)

  tmp_amplitude = matrix(0, nrow = f_l, ncol = d_l)
  tmp_phase = tmp_amplitude

  ## Decompose
  # Decomposition iterates through each center frequency (wavelet), with
  # specified width, performing a convolution between the signal and complex
  # wavelet.

  # freqs_mat = freqs; dim(freqs_mat) = c(f_l, 1)
  # wavelet_win_mat = wavelet_win; dim(wavelet_win_mat) = c(1, length(wavelet_win))
  # tmp_sines = exp(2*1i*pi*freqs_mat %*% wavelet_win_mat)
  # tmp_gaus_wins = t(sapply(freqs, function(frq){
  #   exp(-wavelet_win^2/(2*(wavelet_cycles/(2*pi*frq))^2))
  # }))
  #
  # tmp_wavelets = sapply(freqs, function(frq){
  #   tmp_sine = exp(2*1i*pi*frq*wavelet_win);
  #   tmp_gaus_win = exp(-wavelet_win^2/(2*(wavelet_cycles/(2*pi*frq))^2));
  #   tmp_wavelet = rev(tmp_sine*tmp_gaus_win)
  #   tmp_wavelet
  # })
  # s = ceiling(dim(tmp_wavelets)[1] / 2)

  for(fi in 1:f_l){

    ## create sign wave at center frequency
    tmp_sine = exp(2*1i*pi*freqs[fi]*wavelet_win);
    # make gaussian window, with a width/sd = cycles
    tmp_gaus_win = exp(-wavelet_win^2/(2*(wavelet_cycles/(2*pi*freqs[fi]))^2));
    # make wavelet as dot-product of sine wave and gaussian window
    tmp_wavelet = rev(tmp_sine*tmp_gaus_win)
    # tmp_wavelet = tmp_wavelets[, fi]

    # convolve data with wavelet - remove zero padding ('same' length as input)
    # BF - pre-flip kernel, to deal with flip in conv, keeps phase ok?

    # R comes with convolve, but it's not accurate. So I choose signal::conv as alternative (zw, Dec, 2017)
    # tmp_freq_analytic = convolve(data,rev(tmp_wavelet), type = 'open');

    tmp_freq_analytic = signal::conv(data, tmp_wavelet)

    # There's no option for "same", manually chop the data (zw, Dec, 2017)
    s = ceiling(length(tmp_wavelet) / 2)
    tmp_freq_analytic = tmp_freq_analytic[(s+1): (s + d_l)]


    # extract amplitude and phase data
    # BF: apply amplitude normalization in function?
    tmp_amplitude[fi,] = Mod(tmp_freq_analytic);
    tmp_phase[fi,] = Arg(tmp_freq_analytic);
  }


  list(
    amp = tmp_amplitude,
    phase = tmp_phase
  )
}



