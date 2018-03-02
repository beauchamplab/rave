#' Library to re-sample data
#' decimate_fir is more recommended

#' @description Down sampling a signal after low pass filtering
#' @param s signal to be re-sampled
#' @param p target frequency
#' @param q Current signal frequency
#' @param d tolerance
#' @details This function is a wrapper for \code{signal::resample}:
#'  Note that p and q do not need to be integers since this routine does not use
#'  a polyphase rate change algorithm, but instead uses bandlimited
#'  interpolation, wherein the continuous time signal is estimated by summing
#'  the sinc functions of the nearest neighbouring points up to distance d.
#'
#'  Note that resample computes all samples up to but not including time n+1.
#'  If you are increasing the sample rate, this means that it will generate
#'  samples beyond the end of the time range of the original signal.
compress_signal <- function(s, p, q, d = 5){
  signal::resample(s, p, q, d = 5)
}





#' @description Decimate signal via FIR1 filter
#' @param x signal (vector)
#' @param q Compress rate or downsample rate
#' @param n nth order of FIR kernel will be used, default 30
#' @return Down-sampled signal
#' @export
decimate_fir <- function(x, q, n = 30){
  if (q != round(q))
    stop("decimate only works with integer q.")

  b <- fir1(n, 1/q)
  # y <- fftfilt(b, x)


  # it's time to pad x. since the original decimate in signal package doesn't pad
  if(n %% 2 == 0){
    nfilt = n+1
  }else{
    nfilt = n
  }
  l_x = length(x)
  lpad = 2*x[1] - x[(nfilt+1):2];
  rpad = 2*x[l_x] - x[l_x - (1:nfilt)];

  y = fftfilt(b, c(lpad, x, rpad))
  y = y[ceiling(nfilt + n/2) + (1:l_x)]

  y[seq(1, length(x), by = q)]
}
