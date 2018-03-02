#' plot signals, this is a library for visualizing signals (raw, pwelch, spectro-heatmap, etc.)
NULL

#' @description Welch periodogram
#' @details This function is modified from \code{pwelch} function from "oce"
#' package and is designed for visualizing ECoG voltage data.
#' @param x signal
#' @param fs sample rate
#' @param window window length, default 128
#' @param noverlap overlap between two adjacent windows, by default is 8
#' @param plot logical, plot the result or not
#' @param ... will be passed to plot
pwelch <- function (
  x, fs, window = 64, noverlap = 8,
  col = 'black', xlim = NULL, ylim = NULL, main = 'Welch periodogram',
  plot = TRUE, log = 'xy', spec_func = spectrum, cex = 1, ...) {

  hamming.local <- function(n) {
    n <- round(n)
    if (n < 0)
      stop("n must round to a positive integer")
    if (n == 1)
      c <- 1
    else {
      n <- n - 1
      pi <- 4 * atan2(1, 1)
      c <- 0.54 - 0.46 * cos(2 * pi * (0:n)/n)
    }
    c
  }
  detrend <- function (x, y)
  {
    if (missing(x))
      stop("must give x")
    n <- length(x)
    if (missing(y)) {
      y <- x
      x <- seq_along(y)
    }
    else {
      if (length(y) != n)
        stop("x and y must be of same length, but they are ",
             n, " and ", length(y))
    }
    first <- which(is.finite(y))[1]
    last <- 1 + length(y) - which(is.finite(rev(y)))[1]
    if (x[first] == x[last])
      stop("the first and last x values must be distinct")
    b <- (y[first] - y[[last]])/(x[first] - x[[last]])
    a <- y[first] - b * x[first]
    list(Y = y - (a + b * x), a = a, b = b)
  }

  x <- as.vector(x)
  x.len = length(x)
  window <- hamming.local(floor(x.len/window))

  normalization <- mean(window^2)
  window.len <- length(window)

  step <- max(floor(window.len - noverlap + 1), 0)


  psd <- NULL
  nrow <- 0
  start <- 1
  end <- window.len
  args <- list(...)
  names.args <- names(args)
  if (!("taper" %in% names.args))
    args$taper <- 0
  if (!("plot" %in% names.args))
    args$plot <- FALSE
  if (!("demean" %in% names.args))
    args$demean <- TRUE
  if (!("detrend" %in% names.args))
    args$detrend <- TRUE

  xx <- ts(window * detrend(x[start:end])$Y, frequency = fs)
  s <- do.call(spec_func, args = c(args, list(x = xx)))
  freq <- s$freq

  rowMeans(sapply(seq(window.len, x.len, by = window.len), function(end){
    start = end - window.len + 1
    xx <- ts(window * detrend(x[start:end])$Y, frequency = fs)
    args$x <- xx
    s <- do.call(spec_func, args = args)
    s$spec / normalization
  })) ->
    spec
  res <- list(freq = freq, spec = spec, method = "Welch",
              df = s$df * (x.len/length(window)),
              bandwidth = s$bandwidth, demean = FALSE, detrend = TRUE)
  class(res) <- "spec"
  if (plot) {
    if(log == 'xy'){
      xlab = 'Log10(frequency)'
      ylab = 'Log10(spectrum)'
      freq = log10(freq)
      spec = log10(spec)
    }else if(log == 'y'){
      xlab = 'frequency'
      ylab = 'Log10(spectrum)'
      spec = log10(spec)
    }else if(log == 'x'){
      xlab = 'Log10(frequency)'
      ylab = 'spectrum'
      freq = log10(freq)
    }else{
      xlab = 'frequency'
      ylab = 'spectrum'
    }
    if(plot == 2){
      points(freq, spec, type = 'l', col = col)
    }else{
      plot(freq, spec, type = 'l', col = col, xlab = xlab, ylab = ylab,
           xlim = xlim, ylim = ylim, main = main,
           cex.axis = cex, cex.lab = cex, cex.main = cex, cex.sub = cex)
    }
  }
  return(invisible(res))
}

#' @export
plot_signal <- function(s, sample_rate, time = NULL, boundary = 1000,
                        xbins = 1000, details = F, ...){
  if(is.null(time)){
    x = seq(0, (length(s) - 1))
  }else{
    x = seq(floor(time[1] * sample_rate), ceiling(time[2] * sample_rate))
    x = x[x >= 0 & x <= (length(s) - 1)]
  }
  time = force(x / sample_rate)
  signal = force(s[x + 1])

  args = list(...)
  if(length(args[['ylim']]) == 2){
    ybnds = args[['ylim']]
  }else{
    ybnds = range(signal)
  }

  if(!is.null(boundary)){
    outsel = abs(signal) > boundary
    data.frame(
      Time = time,
      Signal = signal,
      Outliers = outsel
    ) -> tmp
  }else{
    details = F
    data.frame(
      Time = time,
      Signal = signal,
      Outliers = F
    ) -> tmp
  }




  ggplot2::ggplot() + ggplot2::aes(
    x = Time,
    y = Signal
  ) +
    ggplot2::stat_bin2d(bins = xbins, data = tmp) ->
    p

  if(details && sum(outsel) > 0){
    p + ggplot2::geom_linerange(ggplot2::aes(ymax = Signal, ymin = 0), data = tmp[outsel,]) ->
      p
  }
  if(!is.null(boundary)){
    p + ggplot2::geom_hline(ggplot2::aes(yintercept = c(boundary, -boundary)), color = 'red') ->
      p
  }
  p +

    ggplot2::scale_fill_continuous(high = '#f7786b', low = '#92a8d1') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()) ->
    p

  return(p)

}


plot_signals <- function(
  signals, sample_rate = 1, col = 1, space = 0.999, plot = 'base', xlim = NULL,
  channel_names = NULL, start_time = 0, ylab = 'Channel', ...
){
  if(space < 1){
    space = quantile(signals, space)
  }
  ns = nrow(signals)
  nt = ncol(signals)
  if(length(col) == 1){
    col = rep(col, ns)
  }
  y0 = (1:ns) * space
  r = y0 + signals
  Time = (1:nt) / sample_rate

  if(is.null(channel_names)){
    channel_names = paste(1:ns)
  }
  if(is.null(xlim)){
    xlim = c(0, max(Time))
  }

  tsl = (Time >= xlim[1] & Time <= xlim[2])
  Time = Time[tsl]
  r = r[,tsl]
  nt = ncol(r)

  Time = Time + start_time
  if(plot == 'base'){
    matplot(Time, t(r), type='l', col = col, lty=1,
            frame.plot = FALSE, yaxt = 'n', xlab = 'Time(s)', ylab = ylab, ...)
    axis(2, at = y0, labels = channel_names, pos = NA)
    return(space)
  } else{
    tmp = data.frame(
      Time = rep(Time, ns),
      Signals = as.vector(t(r)),
      Channel = rep(channel_names, each = nt),
      ChannelType = paste(rep(col, each = nt))
    )

    ggplot2::ggplot(tmp) + ggplot2::aes(x = Time, y = Signals, group = Channel, color = ChannelType) +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            legend.position="none") +
      ggplot2::scale_y_continuous(breaks = y0, labels = channel_names) +
      ggplot2::scale_x_continuous(expand = c(0,0.5)) +
      ggplot2::scale_color_manual(values=c("#6ea4ca", "#d63d0a", '#272d5a')) ->
      p

    if(plot %in% c('plotly', 'ggplotly')){
      p = plotly::ggplotly(p, tooltip = 'group')
    }

    return(p)
  }

}



#' @description Compare two signals' spectral grams
#' @param s orignal signal (time domain)
#' @param f filtered signal (time domain)
#' @param sample_rate signal sample rate
#' @param freq_lim Frequency bands to plot
#' @param ... other params passed to "plot"
spec_inspect <- function(s, f = NULL, sample_rate = 2000, freq_lim = c(0, 400), ...){
  specx <- spec.pgram(s, plot = FALSE)

  n = length(s)

  if(is.null(freq_lim)){
    freq_lim = c(0, length(specx$freq))
  }

  freq = specx$freq * sample_rate
  sel = freq >= freq_lim[1] & freq <= freq_lim[2]

  x = cbind(freq, log10(specx$spec))[sel,]

  plot(x, type='l', xlab = "frequency (Hz)", ylab = 'Log10 spectrum', col = 'red',
       main = "Raw Periodogram", ...)

  if(!is.null(f)){
    specy <- spec.pgram(f, plot = FALSE)
    y = cbind(freq, log10(specy$spec))[sel,]
    points(y, type = 'l', col='black')
    legend('topright', c('Original', 'Filtered'), col = c('red', 'black'), lty = 1)
  }

}
