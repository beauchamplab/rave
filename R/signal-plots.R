spectrum.pgram <- NULL

#' Plot and Inspect Signals in Trace, Periodogram, and Histogram
#' @param s1 Signal for inspection
#' @param s2 Signal to compare, default NULL
#' @param sc compressed signal to speedup the trace plot, if not provided, then 
#' either the original \code{s1} is used, or a compressed version will be used. 
#' See parameter \code{try_compress}.
#' @param srate Sample rate of s1, note that \code{s2} and \code{s1} must have 
#' the same sample rate
#' @param name Analysis name, for e.g. "CAR", "Notch", etc.
#' @param try_compress If length of \code{s1} is too large, it might take long 
#' to draw trace plot, my solution is to down-sample s1 first (like what Matlab 
#' does), and then plot the compressed signal. Some information will be lost 
#' during this process, however, the trade-off is the speed. 
#' \code{try_compress=FALSE} indicates that you don't want to compress signals 
#' under any situation (this might be slow).
#' @param max_freq Max frequency to plot, should be no larger than half of 
#' the sampling rate.
#' @param flim \code{log10} of frequency range to plot
#' @param window Window length to draw the Periodogram
#' @param noverlap Number of data points that each adjacent windows overlap
#' @param std Error bar (red line) be drawn at standard deviations, by default 
#' is 3, meaning the error bars represent 3 standard deviations.
#' @param nclass Number of classes for histogram
#' @param which Which sub-plot to plot
#' @param start_time When does signal starts
#' @param boundary Boundary for signal plot, default is 1 standard deviation
#' @param main Plot title
#' @param col Color for two signals, length of 2.
#' @param cex,lwd,mar,... passed to \code{\link[graphics]{plot.default}}
#' @examples
#' library(stats)
#' time <- seq(0, 100, by = 1/200)
#' s2 <- sin(2 * pi * 60 * time) + rnorm(length(time))
#' diagnose_signal(s2, srate = 200)
#'
#' # Apply notch filter
#' s1 = notch_filter(s2, 200, 58,62)
#' diagnose_signal(s1, s2, srate = 200)
#' 
#'
#' @export
diagnose_signal <- function(
  s1, s2 = NULL, sc = NULL, srate, name = '', try_compress = TRUE,
  max_freq = 300, window = ceiling(srate * 2), noverlap = window / 2, std = 3,
  cex = 1.5, lwd = 0.5, flim = NULL, nclass = 100,
  main = 'Channel Inspection', col = c('black', 'red'),
  which = NULL, start_time = 0, boundary = NULL, mar = c(5.2, 5.1, 4.1, 2.1),
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
    boundary = std* stats::sd(s1)
  }
  ylim = max(abs(s1), boundary)
  
  # Grid layout
  if(length(which) == 0){
    # grid::grid.newpage()
    lay <- rbind(c(1,1,1), c(2,3,4))
    graphics::par(mar = mar)
    graphics::layout(mat = lay)
    # mai = graphics::par('mai');
    # on.exit({graphics::par(mai = mai)}, add = T)
    # graphics::par(mai = c(1.1, 0.8 ,0.4, 0.25))
  }
  
  # First plot: plot sc directly with col[1]
  if(length(which) == 0 || 1 %in% which){
    graphics::plot(start_time + (seq_along(sc) / sratec), sc, xlab = 'Time (seconds)', ylab = 'Voltage',
         main = main, lwd = lwd,
         type = 'l', ylim = c(-ylim-1, ylim+1), yaxt="n", col = col[1],
         cex.axis = cex * 0.7, cex.lab = cex *0.8, cex.main = cex, cex.sub = cex, ...)
    graphics::abline(h = c(-1,1) * boundary, col = 'red')
    ticks<-c(-ylim, -boundary,0,boundary, ylim)
    graphics::axis(2,at=ticks,labels=round(ticks), las = 1,
         cex.axis = cex*0.7, cex.lab = cex *0.8, cex.main = cex, cex.sub = cex)
  }
  
  # plot 2, 3 too slow, need to be faster - pwelch periodogram
  if(length(which) == 0 || 2 %in% which){
    if(!is.null(s2)){
      pwelch(s2, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[2], cex = cex, ylim = flim,
             log = 'y', xlim = xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      pwelch(s1, fs = srate, window = window, noverlap = noverlap, cex = cex, ylim = flim,
             plot = 2, col = col[1], log = 'y', xlim = xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      graphics::legend('topright', sprintf('%s %s', c('Before', 'After'), name), col = rev(col), lty = 1, cex = cex * 0.7)
    }else{
      pwelch(s1, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[1], cex = cex, ylim = flim,
             log = 'y', xlim = xlim, spec_func = spectrum.pgram, max_freq = max_freq)
    }
  }
  
  
  if(length(which) == 0 || 3 %in% which){
    log_xlim = log10(sapply(xlim, max, 1))
    if(!is.null(s2)){
      pwelch(s2, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[2], cex = cex, ylim = flim,
             log = 'xy', xlim = log_xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      pwelch(s1, fs = srate, window = window, noverlap = noverlap, cex = cex, ylim = flim,
             plot = 2, col = col[1], log = 'xy', xlim = log_xlim, spec_func = spectrum.pgram, max_freq = max_freq)
      graphics::legend('topright', paste0(c('Before ', 'After '), name), col = rev(col), lty = 1, cex = cex * 0.8)
    }else{
      pwelch(s1, fs = srate, window = window,
             noverlap = noverlap, plot = 1, col = col[1], cex = cex, ylim = flim,
             log = 'xy', xlim = log_xlim, spec_func = spectrum.pgram, max_freq = max_freq)
    }
  }
  
  
  if(length(which) == 0 || 4 %in% which){
    # Plot 4:
    graphics::hist(s1, nclass = nclass,
         xlab = 'Signal Voltage Histogram', main = paste0('Histogram ', name),
         cex.axis = cex * 0.7, cex.lab = cex*0.8, cex.main = cex, cex.sub = cex)
  }
  
  return(list(
    ylim = ylim,
    boundary = boundary
  ))
}



#' Plot signals line by line
#' @param signals signals to plot, with each row one signal
#' @param sample_rate sample rate
#' @param col Color, either length of 1 or number of signals. Can be numeric or color name
#' @param space space between to signals. If \code{space_mode='quantile'}, then
#' space is determined by quantile of signal (0 - 1). If \code{space_mode='absoute'}, then
#' space will be as is.
#' @param start_time Time in seconds at which time point the signal should be drawn
#' @param duration length of 1. Time in seconds the duration of time to be drawn.
#' Default is NULL (Total time range)
#' @param compress FALSE means no compression for signals, TRUE is auto-detection,
#' 2, 3, 4,... means compress signals by x and then plot. (usually compress signal to save time)
#' @param channel_names Names for each signals. Will be Y tick labels
#' @param ylab Y axis label
#' @param ... pass to \code{\link[graphics]{matplot}}
#' @param plot,xlim,space_mode,time_shift,lwd,cex,new_plot Deprecated
#' @export
plot_signals <- function(
  signals, sample_rate = 1, col = 1, space = 0.995, space_mode = 'quantile',
  start_time = 0, duration = NULL, compress = TRUE,
  channel_names = NULL, ylab = 'Channel', time_shift = 0, lwd = 0.5,cex = 2,
  new_plot = T, plot = 'base', xlim = NULL,  ...
){
  if(space_mode == 'quantile'){
    space = stats::quantile(signals, space, na.rm = T) *2
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
      if(utils::tail(diff(ind), 1) == 1){
        ind = ind[-(length(ind) - 1)]
      }
      y0 = y0[ind]
      channel_names = channel_names[ind]
    }
  }
  
  if(new_plot){
    graphics::matplot(time_shift + Time, t(r), type='l', col = col, lty=1, lwd = lwd,
            frame.plot = FALSE, yaxt = 'n', xlab = 'Time(s)', ylab = '',
            cex.main = cex, cex.axis = cex * 0.7, cex.lab = cex * 0.8, ...)
    graphics::axis(2, at = y0, labels = channel_names, pos = start_time + time_shift, las=1, cex.axis = cex * 0.7)
    graphics::title(ylab = ylab, line=1, cex.lab=cex * 0.8)
  }else{
    graphics::matpoints(time_shift + Time, t(r), type='l', col = col, lty=1, lwd = lwd)
  }
  
  return(list(
    space = space,
    space_mode = space_mode,
    compress = compress,
    time_range = time_range
  ))
  
}
