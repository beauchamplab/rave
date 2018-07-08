

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
