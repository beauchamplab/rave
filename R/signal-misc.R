# Misc signal processings

postpad <- function(x, n){
  x_len <- length(x)
  if (n > x_len) {
    return(c(x, rep(0, n - x_len)))
  }
  else{
    return(x[seq_len(n)])
  }
}

# Hanning window
hanning <- function(n) {
  if(n == 1){
    return(1)
  }else{
    return(0.5 - 0.5 * cos (2 * pi * seq(0, n - 1) / (n - 1)))
  }
}

detrend_naive <- function(x, y){
  if(missing(y)){
    y = x
    x = seq_along(y)
  }else{
    stopifnot2(length(x) == length(y), msg = 'x and y must have the same length.')
  }
  n = length(y)
  b = ( y[n] - y[1] ) / ( x[n] - x[1] )
  a = y[1] - b * x[1]
  list(Y = y - (a + b * x), a = a, b = b)
}
