#' Signal Inspection Plots
#' @param s1 Signal for inspection
#' @param s2 Signal to compare, default NULL
#' @param sc compressed signal to speedup , if not provided, then either s1 is used,
#' or s1 will be compressed to 100Hz, see also \code{try_compress}
#' @param srate Sample rate of s1, I assume that s2 and s1 have the same sample rate
#' @param name Analysis name, for e.g. 'CAR', 'Notch', etc.
#' @param try_compress If length(s1) is big, R might take long to run, a solution is to
#' down-sample s1 first (like what matlab does), and then plot compressed
#' signal. However, some information will be lost during this process.
#' \code{try_compress=FALSE} indicates that you want raw signal (s1) rather than
#' compressed signal
#' @param max_freq Max frequency to be ploted, should be smaller than srate/2
#' @param flim log10-Frequency range to plot (left bottom plot y range)
#' @param window Window length to draw periodogram
#' @param noverlap Number of data points that each adjacent windows overlap?
#' @param std Error bar (red line) be draw at std * sd(s1), by default is 3, which
#' means 3 standard deviation
#' @param main Plot title
#' @param col Two elements: the first is for s1, second is for s2.
#' @param cex Label, title size. See \code{\link[graphics]{plot.default}}
#' @param lwd Line size for the top plot. See \code{\link[graphics]{plot.default}}
#' @examples
#' time <- seq(0, 300, 1/2000)
#' s2 <- sin(pi/30 * time + rcauchy(length(time)) / 1000) + rnorm(length(time))
#' diagnose_signal(s2, srate = 2000, flim = c(-4,-1))
#'
#' # Apply notch filter
#' s1 = notch_filter(s2, 2000, 58,62)
#' diagnose_signal(s1, s2, srate = 2000, flim = c(-4,-1))
#'
#' @export
diagnose_signal <- function(
  s1, s2 = NULL, sc = NULL, srate, name = '', try_compress = TRUE,
  max_freq = 300, window = ceiling(srate * 2), noverlap = window / 2, std = 3,
  cex = 1.5, lwd = 0.5, flim = NULL, nclass = 100,
  main = 'Channel Inspection', col = c('black', 'red'),
  which = NULL, start_time = 0, boundary = NULL,
  ...){

  # is sc not specified, and srate is too high, compress s1
  if(try_compress && (is.null(sc) || (srate > 200 && length(s1) / srate > 300))){
    sratec = 100
    sc <- s1[round(seq(1, length(s1), by = srate/sratec))]
  }else{
    sc %?<-% s1
    sratec = srate / length(s1) * length(sc)
  }
  max_freq = min(max_freq, floor(srate/ 2))
  xlim = c(0, max_freq)

  # Calculate boundary to draw
  if(is.null(boundary)){
    boundary = std* sd(s1)
  }
  ylim = max(abs(s1), boundary)

  # Grid layout
  if(length(which) == 0){
    grid::grid.newpage()
    lay <- rbind(c(1,1,1), c(2,3,4))
    graphics::layout(mat = lay)
    # mai = par('mai');
    # on.exit({par(mai = mai)}, add = T)
    # par(mai = c(1.1, 0.8 ,0.4, 0.25))
  }

  # First plot: plot sc directly with col[1]
  if(length(which) == 0 || 1 %in% which){
    plot(start_time + (seq_along(sc) / sratec), sc, xlab = 'Time (seconds)', ylab = 'Voltage',
         main = main, lwd = lwd,
         type = 'l', ylim = c(-ylim-1, ylim+1), yaxt="n", col = col[1],
         cex.axis = cex * 0.7, cex.lab = cex *0.8, cex.main = cex, cex.sub = cex, ...)
    abline(h = c(-1,1) * boundary, col = 'red')
    ticks<-c(-ylim, -boundary,0,boundary, ylim)
    axis(2,at=ticks,labels=round(ticks), las = 1,
         cex.axis = cex*0.7, cex.lab = cex *0.8, cex.main = cex, cex.sub = cex)
  }

  # plot 2, 3 too slow, need to be faster - pwelch periodogram
  if(length(which) == 0 || 2 %in% which){
    if(!is.null(s2)){
      pwelch(s2, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[2], cex = cex, ylim = flim,
             log = 'y', xlim = xlim, spec_func = rave:::spectrum.pgram, max_freq = max_freq)
      pwelch(s1, fs = srate, window = window, noverlap = noverlap, cex = cex, ylim = flim,
             plot = 2, col = col[1], log = 'y', xlim = xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      legend('topright', sprintf('%s %s', c('Before', 'After'), name), col = rev(col), lty = 1, cex = cex * 0.7)
    }else{
      pwelch(s1, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[1], cex = cex, ylim = flim,
             log = 'y', xlim = xlim, spec_func = rave:::spectrum.pgram, max_freq = max_freq)
    }
  }


  if(length(which) == 0 || 3 %in% which){
    log_xlim = log10(sapply(xlim, max, 1))
    if(!is.null(s2)){
      pwelch(s2, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[2], cex = cex, ylim = flim,
             log = 'xy', xlim = log_xlim, spec_func = rave:::spectrum.pgram, max_freq = max_freq)
      pwelch(s1, fs = srate, window = window, noverlap = noverlap, cex = cex, ylim = flim,
             plot = 2, col = col[1], log = 'xy', xlim = log_xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      legend('topright', c('Before ', 'After ') %&% name, col = rev(col), lty = 1, cex = cex * 0.8)
    }else{
      pwelch(s1, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[1], cex = cex, ylim = flim,
             log = 'xy', xlim = log_xlim, spec_func = rave:::spectrum.pgram, max_freq = max_freq)
    }
  }


  if(length(which) == 0 || 4 %in% which){
    # Plot 4:
    hist(s1, nclass = nclass,
         xlab = 'Signal Voltage Histogram', main = 'Histogram ' %&% name,
         cex.axis = cex * 0.7, cex.lab = cex*0.8, cex.main = cex, cex.sub = cex)
  }

  return(list(
    ylim = ylim,
    boundary = boundary
  ))
}


#' Hanning window
#' @usage hanning(n)
hanning <- function(n) {
  if(n == 1){
    return(1)
  }else{
    return(0.5 - 0.5 * cos (2 * pi * seq(0, n - 1) / (n - 1)))
  }
}

detrend.naive <- function(x, y){
  if(missing(y)){
    y = x
    x = seq_along(y)
  }else{
    assertthat::assert_that(length(x) == length(y), msg = 'x and y must have the same length.')
  }
  n = length(y)
  b = ( y[n] - y[1] ) / ( x[n] - x[1] )
  a = y[1] - b * x[1]
  list(Y = y - (a + b * x), a = a, b = b)
}

postpad <- function(x, n){
  x_len <- length(x)
  if (n > x_len) {
    return(c(x, rep(0, n - x_len)))
  }
  else{
    return(x[seq_len(n)])
  }
}


#' Welch Periodogram
#' @details This function is modified from \code{pwelch} function from "oce"
#' package and is designed for visualizing ECoG voltage data.
#' @param x signal
#' @param fs sample rate
#' @param window window length, default 128
#' @param noverlap overlap between two adjacent windows, by default is 8
#' @param log '', 'x', 'y', 'xy', indicates which axis should be log10 value.
#' @param plot logical, plot the result or not
#' @param ... will be passed to plot
#' @export
pwelch <- function (
  x, fs, window = 64, noverlap = 8, nfft = 256,
  col = 'black', xlim = NULL, ylim = NULL, main = 'Welch periodogram',
  plot = TRUE, log = 'xy', spec_func = spectrum, cex = 1, ...) {


  x <- as.vector(x)
  x_len = length(x)

  nfft = max(min(nfft, length(x)), window)

  window <- hanning(window)

  window_norm = norm(window, '2')
  window_len <- length(window)

  normalization <- mean(window^2)

  step <- max(floor(window_len - noverlap + 0.99), 1)

  ## Average the slices
  offset = seq(1, x_len-window_len+1, by = step)

  N = length(offset);

  sapply(seq_len(N), function(i){
    a = detrend.naive(x[offset[i] - 1 + seq_len(window_len)])
    a = fftwtools::fftw_r2c(postpad(a$Y * window, nfft))
    Mod(a)^2
  }) ->
    re

  NN = floor((nfft + 1) / 2)

  freq = seq(1, fs / 2, length.out = NN)
  spec = rowMeans(re[seq_len(NN),,drop = F]) / (window_len / 2)^2


  res = list(
    freq = freq,
    spec = spec,
    method = "Welch"
  )

  # psd <- NULL
  # nrow <- 0
  # start <- 1
  # end <- window_len
  # args <- list(...)
  # names.args <- names(args)
  # if (!("taper" %in% names.args))
  #   args$taper <- 0
  # if (!("plot" %in% names.args))
  #   args$plot <- FALSE
  # if (!("demean" %in% names.args))
  #   args$demean <- TRUE
  # if (!("detrend" %in% names.args))
  #   args$detrend <- TRUE
  #
  # xx <- ts(window * detrend(x[start:end])$Y, frequency = fs)
  # s <- do.call(spec_func, args = c(args, list(x = xx)))
  # freq <- s$freq
  #
  # rowMeans(sapply(seq(window_len, x_len, by = window_len), function(end){
  #   start = end - window_len + 1
  #   xx <- ts(window * detrend(x[start:end])$Y, frequency = fs)
  #   args$x <- xx
  #   s <- do.call(spec_func, args = args)
  #   s$spec / normalization
  # })) ->
  #   spec
  # res <- list(freq = freq, spec = spec, method = "Welch",
  #             df = s$df * (x_len/length(window)),
  #             bandwidth = s$bandwidth, demean = FALSE, detrend = TRUE)
  # class(res) <- "spec"
  if (plot) {
    if(log == 'xy'){
      xlab = 'Log10(Frequency)'
      ylab = 'Power/Frequency (dB/Hz)'
      freq = log10(freq)
      spec = log10(spec) * 10
    }else if(log == 'y'){
      xlab = 'Frequency'
      ylab = 'Power/Frequency (dB/Hz)'
      spec = log10(spec) * 10
    }else if(log == 'x'){
      xlab = 'Log10(Frequency)'
      ylab = 'Power'
      freq = log10(freq)
    }else{
      xlab = 'Frequency'
      ylab = 'Power'
    }
    if(plot == 2){
      points(freq, spec, type = 'l', col = col)
    }else{
      plot(freq, spec, type = 'l', col = col, xlab = xlab, ylab = ylab,
           xlim = xlim, ylim = ylim, main = main, las = 1,
           cex.axis = cex * 0.7, cex.lab = cex * 0.8, cex.main = cex, cex.sub = cex)
    }
  }
  return(invisible(res))
}



#' Plot signals line by line
#' @usage plot_signals(signals, sample_rate = 1, col = 1, space = 0.999,
#' space_mode = 'quantile',start_time = 0, time_range = NULL, compress = 1,
#' channel_names = NULL, ylab = 'Channel')
#' @param signals signals to plot, with each row one signal
#' @param sample_rate sample rate
#' @param col Color, either length of 1 or number of signals. Can be numeric or color name
#' @param space space between to signals. If \code{space_mode='quantile'}, then
#' space is determined by quantile of signal (0 - 1). If \code{space_mode='absoute'}, then
#' space will be as is.
#' @param start_time Time in seconds at which time point the signal should be drawn
#' @param duration length of 1. Time in seconds the duration of time to be drawn.
#' Default is NULL (Total time span)
#' @param compress FALSE means no compression for signals, TRUE is auto-detection,
#' 2, 3, 4,... means compress signals by x and then plot. (usually compress signal to save time)
#' @param channel_names Names for each signals. Will be Y tick labels
#' @param ylab Y axis label
#' @param ... pass to matplot
#' @param plot,xlim Depricated.
#' @export
plot_signals <- function(
  signals, sample_rate = 1, col = 1, space = 0.995, space_mode = 'quantile',
  start_time = 0, duration = NULL, compress = TRUE,
  channel_names = NULL, ylab = 'Channel', time_shift = 0, lwd = 0.5,cex = 2,
  new_plot = T, plot = 'base', xlim = NULL,  ...
){
  if(space_mode == 'quantile'){
    space = quantile(signals, space, na.rm = T) *2
  }
  compress = round(compress)
  if(compress == 1){
    if(length(duration)){
      n_tp = round(duration * sample_rate)
    }else{
      n_tp = round(ncol(signals) - start_time * sample_rate)
    }
    if(n_tp > 10000){
      compress = (n_tp / 10000)
    }
  }


  if(compress > 1){
    sample_rate = sample_rate / compress
    signals = signals[, round(seq(1, ncol(signals), by = compress))]
  }
  ns = nrow(signals)       # Number of channels
  nt = ncol(signals)       # Total time points
  if(length(col) == 1){
    col = rep(col, ns)     # Color for each channels
  }
  y0 = (1:ns) * space      # Re-calculate zero lines for each channels
  r = y0 + signals         # Re-calculate channel positions
  Time = (seq_len(nt) -1) / sample_rate   # True time in seconds

  start_time = min(start_time, range(Time)[2]-10/sample_rate)
  if(is.null(duration)){
    time_range = c(start_time, range(Time)[2])      # duration is max range by default
  }else{
    time_range = c(start_time, start_time + duration)
  }

  tsl = Time %within% time_range

  r = r[,tsl]
  Time = Time[tsl]
  nt = ncol(r)

  if(is.null(channel_names)){
    channel_names = paste(1:ns)   # Assign channel names
    if(length(y0) > 30){
      ind = seq(0, length(y0), by = 5)
      ind = unique(c(1, ind[-1], length(y0)))
      if(tail(diff(ind), 1) == 1){
        ind = ind[-(length(ind) - 1)]
      }
      y0 = y0[ind]
      channel_names = channel_names[ind]
    }
  }

  if(new_plot){
    matplot(time_shift + Time, t(r), type='l', col = col, lty=1, lwd = lwd,
            frame.plot = FALSE, yaxt = 'n', xlab = 'Time(s)', ylab = '',
            cex.main = cex, cex.axis = cex * 0.7, cex.lab = cex * 0.8, ...)
    axis(2, at = y0, labels = channel_names, pos = start_time + time_shift, las=1, cex.axis = cex * 0.7)
    title(ylab = ylab, line=1, cex.lab=cex * 0.8)
  }else{
    matpoints(time_shift + Time, t(r), type='l', col = col, lty=1, lwd = lwd)
  }

  return(list(
    space = space,
    space_mode = space_mode,
    compress = compress,
    time_range = time_range
  ))

}
