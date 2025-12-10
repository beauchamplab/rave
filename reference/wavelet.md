# Wavelet Transformation With Phase

The code was translated from Matlab script written by Brett Foster,
Stanford Memory Lab, 2015 with permission to use in \`RAVE\`.

## Usage

``` r
wavelet(data, freqs, srate, wave_num, demean = TRUE)
```

## Arguments

- data:

  \- vector of time series to be decomposed

- freqs:

  \- vector of center frequencies for decomposition

- srate:

  \- sample rate (in Hz)

- wave_num:

  \- desired number of cycles in wavelet (typically 3-20 for frequencies
  2-200).

- demean:

  \- whether to remove the mean of data first?

## Details

Decompose time series data into time-frequency representation (spectral
decomposition) using wavelet transform. Employs "Morlet" wavelet method
(gaussian taper sine wave) to obtain the analytic signal for specified
frequencies (via convolution).
