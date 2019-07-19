import numpy as np

try:
  from accelerate.mkl.fftpack import rfft, irfft, fft, ifft
except ImportError:
  try:
    from mkl_fft._numpy_fft import rfft, irfft, fft, ifft
  except ImportError:
    try:
      from pyfftw.interfaces.numpy_fft import rfft, irfft, fft, ifft
    except ImportError:
      from numpy.fft import rfft, irfft, fft, ifft

def wavelet(data, freqs, srate, wave_num, demean=True):
  '''
  @title Wavelet Transformation With Phase
  @param data - vector of time series to be decomposed
  @param freqs - vector of center frequencies for decomposition
  @param srate - sample rate (in Hz)
  @param wave_num - desired number of cycles in wavelet (typically 3-20 for frequencies 2-200).
  @param demean - whether to de-mean data first?
  @details
  Wavelet
  Function for decomposing time series data into time-frequency
  representation (spectral decomposition) using wavelet transform. Employs
  Morlet wavelet method (gaussian taper sine wave) to obtain the analytic
  signal for specified frequencies (via convolution).
  Inputs:
   data - vector of time series to be decomposed
   freqs - vector of center frequencies for decomposition
   srate - sample rate (in Hz)
   wave_num - desired number of cycles in wavelet (typically 5-10).
  
  Translated from Matlab script written by Brett Foster,
  Stanford Memory Lab, Feb. 2015
  '''
  srate = round(srate);
  
  # wave_num can be an array equals to length of freqs or just two
  if not len(wave_num) == len(freqs):
    ratio = (np.log(np.max(wave_num)) - np.log(np.min(wave_num))) / (np.log(np.max(freqs)) - np.log(np.min(freqs)))
    wavelet_cycles = np.exp((np.log(freqs) - np.log(np.min(freqs))) * ratio + np.log(np.min(wave_num)))
  else:
    wavelet_cycles = np.array(wave_num)
    
  f_l = len(freqs)
  d_l = data.shape[-1]
  
  # normalize data, and fft
  if demean:
    fft_data = fft(data - np.mean(data))
  else:
    fft_data = fft(data)
  
  # wavelet window calc - each columns of final wave is a wavelet kernel (after fft)
  # sts = wavelet_cycles / (2 * pi * freqs)
  # wavelet_wins = cbind(-3 * sts, 3 * sts)
  
  # calculate and fft wavelet kernels
  fft_waves = np.ndarray(shape = [f_l, d_l], dtype = np.complex128)
  
  for ii in range(f_l):
    fq = freqs[ii]
    cycles = wavelet_cycles[ii]
    
    # standard error
    st = cycles / (2 * np.pi * fq)
    
    # calculate window size
    wavelet_win = np.arange(-3 * st, 3 * st, 1/srate)
    # half of window length
    w_l_half = (len(wavelet_win) - 1) / 2
    
    # wavelet 1: calc sinus in complex domain
    tmp_sine = np.exp((1j) * 2 * np.pi * fq / srate * np.arange(-w_l_half,w_l_half+1))
    # Gaussian normalization part
    A = 1/np.sqrt(st*np.sqrt(np.pi))
    
    # wavelet 2: calc gaussian wrappers
    tmp_gaus_win = A * np.exp(-np.power(wavelet_win, 2)/(2 * np.power(cycles/(2 * np.pi * fq), 2)))
    
    # wave kernel
    tmp_wavelet = tmp_sine * tmp_gaus_win
    
    # padding
    w_l = len(tmp_wavelet)
    n_pre  = int(np.ceil(d_l / 2) - np.floor(w_l/2))
    n_post = int(d_l - n_pre - w_l)
    
    wvi = np.pad(tmp_wavelet, [n_pre, n_post], 'constant', constant_values = 0)
    fft_wave = np.conj(fft(wvi))
    
    fft_waves[ii, :] = fft_wave
  
  # Convolution Notice that since we don't pad zeros to data
  # d_l is nrows of wave_spectrum. However, if wave_spectrum is changed
  # we should not use d_l anymore. instead, use nrow(fft_waves)
  wave_len = fft_waves.shape[-1]
  wave_spectrum = ifft(fft_data * fft_waves)
  
  # use numpy fancy index
  cut_off = np.ceil(wave_len / 2)
  ind = np.concatenate([np.arange(cut_off, wave_len, dtype = np.int), np.arange(cut_off, dtype = np.int)])
  wave_spectrum = wave_spectrum[:, ind] / np.sqrt(srate / 2)
  
  # returns amplitude and phase data
  phase = np.angle(wave_spectrum)
  power = np.power(np.real(wave_spectrum), 2) + np.power(np.imag(wave_spectrum), 2)
  coef = np.sqrt(power)
  
  return {
    'power' : power,
    'phase' : phase,
    'coef' : coef
  }
