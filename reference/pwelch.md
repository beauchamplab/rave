# Plot "Welch" Periodogram

Plot "Welch" Periodogram

## Usage

``` r
pwelch(
  x,
  fs,
  window = 64,
  noverlap = 8,
  nfft = 256,
  col = "black",
  xlim = NULL,
  ylim = NULL,
  main = "Welch periodogram",
  plot = TRUE,
  log = "xy",
  spec_func = stats::spectrum,
  cex = 1,
  ...
)
```

## Arguments

- x:

  signal

- fs:

  sample rate

- window:

  window length, default 128

- noverlap:

  overlap between two adjacent windows, by default is 8

- nfft:

  number of basis functions

- col, xlim, ylim, main, cex, ...:

  will be passed to plot

- plot:

  logical, plot the result or not

- log:

  indicates which axis should be
  [`log10`](https://rdrr.io/r/base/Log.html) value. options are `''`,
  `'x'`, `'y'`, `'xy'`.

- spec_func:

  deprecated
