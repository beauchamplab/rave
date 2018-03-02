require(magrittr)
source('./adhoc/condition_explorer/plot_helpers.R')


crp <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')(1001)

group_colors <- c('orangered', 'orange', 'dodgerblue3', 'purple3', 'darkgreen', 'brown')

# allow color cycling
get_color <- function(ii) {
  group_colors[ii %% length(group_colors) + 1]
}

#' @author John Magnotti
draw_img <- function(mean_by_trial, time, frequencies,
                     yax=T, xax=T, zlim, main='', main.col='black', label.col='black', ...) {

  mean_by_trial %<>% clip_x(lim=zlim)

  image(x=time, y=frequencies,
        z=mean_by_trial, zlim=zlim,
        col=crp, xlab='Time (s)', ylab='Frequency (Hz)', cex.lab=1.7, axes=F, useRaster = FALSE, ...)

  title(main=list(main, col=main.col, cex=2))

  if(yax) axis(2, at=quantile(frequencies, 0:5/5) %>% round, las=1, tick=FALSE, cex.axis=1.5, hadj=0.65)
  if(xax) axis(1, tick = FALSE, cex.axis=1.5)


  logger(paste0('class(tr): ', class(TIME_RANGE), '      class(fr): ', class(FREQUENCY)))

  # return()

  xy <- cbind(TIME_RANGE, FREQUENCY)

  # polygon(c(xy[,1], rev(xy[,1])) , rep(xy[,2], each=2), lty=2, lwd=3, border=label.col)

  #draw baseline region
  abline(v=BASELINE, lty=3, lwd=2, col=label.col)
  # label baseline region
  text(BASELINE %>% median, quantile(frequencies, .7), 'baseline', col=label.col, cex=1.5, pos=3)
  arrows(BASELINE[1], quantile(frequencies, .7), BASELINE[2], col=label.col, length=.1, code=3)

  invisible(mean_by_trial)
}
