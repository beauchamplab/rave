from __future__ import division

import numpy as np

from numpy.fft import rfftfreq, fftfreq

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

__all__ = ['gaussian', 'hamming', 'hilbert']

def gaussian(X, rate, center, sd):
  '''
  X : ndarray (n_channels, n_time)
      Input data, dimensions
  '''
  time = X.shape[-1]
  freq = fftfreq(time, 1./rate)
  k = np.exp((-(np.abs(freq) - center)**2)/(2 * (sd**2)))
  k /= k.sum()
  return k

def hamming(X, rate, min_freq, max_freq):
  time = X.shape[-1]
  freq = fftfreq(time, 1./rate)
  
  pos_in_window = np.logical_and(freq >= min_freq, freq <= max_freq)
  neg_in_window = np.logical_and(freq <= -min_freq, freq >= -max_freq)
  
  k = np.zeros(len(freq))
  window_size = np.count_nonzero(pos_in_window)
  window = np.hamming(window_size)
  k[pos_in_window] = window
  window_size = np.count_nonzero(neg_in_window)
  window = np.hamming(window_size)
  k[neg_in_window] = window
  k /= k.sum()
  
  return k

def hilbert(X, rate, filters=None, phase=None, X_fft_h=None):
  """
  Apply bandpass filtering with Hilbert transform using
  a prespecified set of filters.
  Parameters
  ----------
  X : ndarray (n_channels, n_time)
      Input data, dimensions
  rate : float
      Number of samples per second.
  filters : filter or list of filters (optional)
      One or more bandpass filters
  Returns
  -------
  Xh : ndarray, complex
      Bandpassed analytic signal
  """
  if not isinstance(filters, list):
    filters = [filters]
  time = X.shape[-1]
  freq = fftfreq(time, 1. / rate)

  Xh = np.zeros((len(filters),) + X.shape, dtype=np.complex)
  if X_fft_h is None:
    # Heavyside filter
    h = np.zeros(len(freq))
    h[freq > 0] = 2.
    h[0] = 1.
    h = h[np.newaxis, :]
    X_fft_h = fft(X) * h
    if phase is not None:
      X_fft_h *= phase
  for ii, f in enumerate(filters):
    if f is None:
      Xh[ii] = ifft(X_fft_h)
    else:
      f = f / f.sum()
      Xh[ii] = ifft(X_fft_h * f)
  if Xh.shape[0] == 1:
    return Xh[0], X_fft_h

  return Xh, X_fft_h
