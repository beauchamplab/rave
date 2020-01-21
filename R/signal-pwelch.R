#' Plot "Welch" Periodogram
#' @param x signal
#' @param fs sample rate
#' @param window window length, default 128
#' @param nfft number of basis functions
#' @param noverlap overlap between two adjacent windows, by default is 8
#' @param log indicates which axis should be \code{\link{log10}} value. options 
#' are \code{''}, \code{'x'}, \code{'y'}, \code{'xy'}.
#' @param plot logical, plot the result or not
#' @param col,xlim,ylim,main,cex,... will be passed to plot
#' @param spec_func deprecated
#' @export
pwelch <- function (
  x, fs, window = 64, noverlap = 8, nfft = 256,
  col = 'black', xlim = NULL, ylim = NULL, main = 'Welch periodogram',
  plot = TRUE, log = 'xy', spec_func = stats::spectrum, cex = 1, ...) {
  
  
  x <- as.vector(x)
  x_len = length(x)
  
  nfft = max(min(nfft, length(x)), window)
  
  window <- hanning(window)
  
  # window_norm = norm(window, '2')
  window_len <- length(window)
  
  # normalization <- mean(window^2)
  
  step <- max(floor(window_len - noverlap + 0.99), 1)
  
  ## Average the slices
  offset = seq(1, x_len-window_len+1, by = step)
  
  N = length(offset);
  
  sapply(seq_len(N), function(i){
    a = detrend_naive(x[offset[i] - 1 + seq_len(window_len)])
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
      graphics::points(freq, spec, type = 'l', col = col)
    }else{
      graphics::plot(freq, spec, type = 'l', col = col, xlab = xlab, ylab = ylab,
           xlim = xlim, ylim = ylim, main = main, las = 1,
           cex.axis = cex * 0.7, cex.lab = cex * 0.8, cex.main = cex, cex.sub = cex)
    }
  }
  return(invisible(res))
}
