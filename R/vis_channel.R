#' Visualizations for singals - raw plot + spec + histogram
NULL

#' @description Signal plots - Raw plot, Spec, Historgams
#' @param s1 Main signal (usually after being processed)
#' @param s2 Signal to compare (before process)
#' @param srate sample rate
#' @param boundary 2xstandard error of s1 if not specified (i.e. -1).
#' @param xbins speed up plot
#' @param details speed up plot
#' @param nclass histogram n-bins
#' @param window pwelch window
#' @param noverlap pwelch window overlap
#' @param max_freq max frequencies to draw
#' @return NULL
#' @import grid
vis_channel <- function(s1, s2 = NULL, srate, main = '',
                        col = c('blue', 'red'), boundary = -1, xbins = 100, details = T,
                        nclass = 100, window = 256, noverlap = 8, max_freq = 300){
  grid::grid.newpage()
  lay <- rbind(c(1,1,1),
               c(2,3,4))
  graphics::layout(mat = lay)

  plot.new()
  vps <- gridBase::baseViewports()
  grid::pushViewport(vps$figure)
  vp1 <-grid::plotViewport(c(0,0,0,0))
  if(boundary < 0){
    boundary = 2* sd(s1)
  }

  # raw plot
  plot_signal(s1, srate, boundary = boundary, xbins = xbins) +
    ggplot2::ggtitle(main) ->
    rawplot
  print(rawplot, vp = vp1)
  grid::popViewport()

  if(is.null(s2)){
    # pwelch x
    pwelch(s1, fs = srate, window = window, noverlap = noverlap, plot = 1, col = col[1], log = 'y', xlim = c(0, max_freq))
    pwelch(s1, fs = srate, window = window, noverlap = noverlap, plot = 1, col = col[1], log = 'xy',
           xlim = c(0, log10(max_freq)))
  }else{
    # pwelch x
    pwelch(s2, fs = srate, window = window, noverlap = noverlap, plot = 1, col = col[2], log = 'y', xlim = c(0, max_freq))
    pwelch(s1, fs = srate, window = window, noverlap = noverlap, plot = 2, col = col[1], log = 'y', xlim = c(0, max_freq))
    legend('topright', c('Before', 'After'), col = col, lty = 1)


    # pwelch log(x)
    pwelch(s2, fs = srate, window = window, noverlap = noverlap, plot = 1, col = col[2], log = 'xy',
           xlim = c(0, log10(max_freq)))
    pwelch(s1, fs = srate, window = window, noverlap = noverlap, plot = 2, col = col[1], log = 'xy',
           xlim = c(0, log10(max_freq)))
    legend('topright', c('Before', 'After'), col = col, lty = 1)
  }


  # Hist
  hist(s1, nclass = nclass, xlab = 'Signal Voltage', main = 'Histogram after')
}



