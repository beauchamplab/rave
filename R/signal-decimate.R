fir1 <- function (n, w, type = c("low", "high", "stop", "pass", "DC-0", "DC-1"), 
                  window, scale = TRUE) {
  type <- match.arg(type)
  if (!is.logical(scale)) {
    scale <- match.arg(scale, c("scale", "noscale"))
    scale <- scale == "scale"
  }
  if(missing(window)){
    window <- signal::hamming(n + 1)
  }
  if (is.function(window)) 
    window <- window(n + 1)
  else if (is.character(window)) 
    window <- do.call(window, list(n + 1))
  ftype <- tolower(type) %in% c("low", "stop", "dc-1")
  bands <- length(w) + 1
  f <- numeric(2 * bands)
  f[2 * bands] = 1
  f[seq(2, 2 * bands - 1, by = 2)] <- w
  f[seq(3, 2 * bands - 1, by = 2)] <- w
  m <- numeric(2 * bands)
  m[seq(1, 2 * bands, by = 2)] <- (1:bands - (1 - ftype))%%2
  m[seq(2, 2 * bands, by = 2)] <- m[seq(1, 2 * bands, by = 2)]
  b <- signal::fir2(n, f, m, 512, 2, window)
  if (scale) {
    if( m[1] == 1 ){
      ## find the middle of the first band edge
      ## find the frequency of the normalizing gain
      w_o <- 0
    } else if(f[4] == 1){
      ## for a highpass filter,
      ## use the gain at half the sample frequency
      w_o <- 1
    }else{
      w_o <- f(3) + (f(4)-f(3))/2;
    }
    renorm <- 1/abs(signal::polyval(b, exp(-(0+1i) * pi * w_o)))
    b <- renorm * b
  }
  signal::Ma(b)
}

decimate <- function (x, q, n = if (ftype == "iir") 8 else 30, ftype = "iir") {
  if (q != round(q))
    stop("decimate only works with integer q.")
  l_x <- length(x)
  
  fir <- ftype == "fir"
  
  if (fir) {
    b <- fir1(n, 1/q)
    
    # it's time to pad x. since the original decimate in signal package doesn't pad
    if(n %% 2 == 0){
      nfilt <- n+1
    }else{
      nfilt <- n
    }
    lpad <- 2*x[1] - x[(nfilt+1):2]
    rpad <- 2*x[l_x] - x[l_x - (1:nfilt)]
    y <- signal::fftfilt(b, c(lpad, x, rpad))
    y = y[ceiling(nfilt + n/2) + (1:l_x)]
  }
  else {
    b <- signal::cheby1(n, 0.05, 0.8/q)
    y <- signal::filtfilt(b, x)
  }
  
  y[seq(1, l_x, by = q)]
}

#' Decimate or Down-sample a Signal using \code{FIR} Filters
#' @param x signal to be decimated
#' @param q integer factor to decimated by
#' @param n filter order used for down-sample procedure, default is 30
#' @return Down-sampled signal
#' @export
decimate_fir <- function(x, q, n = 30){
  decimate(x = x, q = q, n = n, ftype = "fir")
}








