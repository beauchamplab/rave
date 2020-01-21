# Original script: noiseFiltData.m
# This file provides functional (library) part of notch filter
# It is irrelevant to logical processing
NULL



#' Apply Notch Filter to Analog Trace Data
#' @details This function is alternative R version of notch filter
#' @param s signal in time or frequency domain
#' @param sample_rate signal sample rate
#' @param lb filter lower bound (Hz)
#' @param ub filter upper bound (Hz)
#' @param domain 1 if the input signal is in the time domain, 0 if it is in the frequency domain
#' @return filtered signal in time domain
#' @export
notch_filter <- function(s, sample_rate, lb, ub, domain = 1){
  max_freq <- sample_rate / 2
  n = length(s)
  df <- 2 * max_freq / length(s)
  centre_freq = (lb + ub) / 2
  filter_width = (-lb + ub)
  
  x = seq(0, max_freq, by = df)
  gauss = exp(-(centre_freq - x)^2 * 10)
  cnt_gauss = round(centre_freq / df)
  
  flat_padd = 0 # flat padding at the max value of the gaussian
  padd_left = floor(flat_padd/2);
  padd_right = ceiling(flat_padd/2);
  
  gauss_left = gauss[(padd_left+1):cnt_gauss]
  gauss_right = gauss[-((padd_left+1):cnt_gauss)]
  
  our_wind = 1 - c(gauss_left, rep(0, flat_padd), gauss_right)
  
  n_r = length(our_wind)
  if(n %% 2 == 0){
    n_r = n_r - 1
    our_wind = our_wind[c(1:n_r, n_r:1)]
  }else{
    our_wind = our_wind[c(1:n_r, (n_r-1):1)]
  }
  
  
  if(domain == 1){
    s = fftwtools::fftw_r2c(s) / n
  }
  
  
  filt_signal = fftwtools::fftw_c2r(s * our_wind)
  filt_signal
  
}

#' Filter line noise out from ECoG channels
#' @param s signal, time domain
#' @param sample_rate signal sample rate
#' @param bands bands that will be filtered out
#' @param width along with bands, half of the filter width. For example,if bands
#' is 60Hz and width is 1Hz, then the notch filter lower bound is 60-1=59Hz and
#' upper bound is 60+1=61Hz.
#' @export
notch_channel <- function(s, sample_rate, bands = c(60, 120, 180), 
                          width = c(1,2,2)){
  s = as.vector(s)
  lbs = bands - width
  ubs = bands + width
  for(i in 1:length(bands)){
    s = notch_filter(s, sample_rate, lbs[i], ubs[i], domain = 1)
  }
  s
}




