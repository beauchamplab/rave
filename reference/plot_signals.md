# Plot signals line by line

Plot signals line by line

## Usage

``` r
plot_signals(
  signals,
  sample_rate = 1,
  col = 1,
  space = 0.995,
  space_mode = "quantile",
  start_time = 0,
  duration = NULL,
  compress = TRUE,
  channel_names = NULL,
  ylab = "Channel",
  time_shift = 0,
  lwd = 0.5,
  cex = 2,
  new_plot = TRUE,
  plot = "base",
  xlim = NULL,
  ...
)
```

## Arguments

- signals:

  signals to plot, with each row one signal

- sample_rate:

  sample rate

- col:

  Color, either length of 1 or number of signals. Can be numeric or
  color name

- space:

  space between to signals. If `space_mode='quantile'`, then space is
  determined by quantile of signal (0 - 1). If `space_mode='absoute'`,
  then space will be as is.

- start_time:

  Time in seconds at which time point the signal should be drawn

- duration:

  length of 1. Time in seconds the duration of time to be drawn. Default
  is NULL (Total time range)

- compress:

  FALSE means no compression for signals, TRUE is auto-detection, 2, 3,
  4,... means compress signals by x and then plot. (usually compress
  signal to save time)

- channel_names:

  Names for each signals. Will be Y tick labels

- ylab:

  Y axis label

- plot, xlim, space_mode, time_shift, lwd, cex, new_plot:

  Deprecated

- ...:

  pass to [`matplot`](https://rdrr.io/r/graphics/matplot.html)
