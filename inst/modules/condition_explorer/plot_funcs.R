require(magrittr)
source('./adhoc/condition_explorer/plot_helpers.R')

rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')

rave_heat_map_colors <- rave_color_ramp_palette(1001)

# put this hear for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('orangered', 'orange', 'dodgerblue3', 'purple3', 'darkgreen', 'brown')

rave_colors <- list('BASELINE_WINDOW'='gray70', 'ANALYSIS_WINDOW' = 'salmon2')


# allow color cycling
get_color <- function(ii) {
  group_colors[ii %% length(group_colors) + 1]
}

#' @author John Magnotti
#' @description Many parameters are just passed to the decorator function, the idea was to be able to separate the plotting of the heatmap from all the accoutrements
draw_img <- function(zmat, x, y, xlab='Time (s)',ylab='Frequency (Hz)',
                     zlim, main='', main.col='black', label.col='black',
                     DECORATOR=decorate_tf_heatmap, ...) {

  zmat %<>% clip_x(lim=zlim)

  image(x=x, y=y,
        z=zmat, zlim=zlim,
        col=crp, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, axes=F, useRaster = FALSE, ...)

  DECORATOR(x=x, y=y, main=main, main.col=main.col, label.col=label.col, ...)

  # return the clipped zmat
  invisible(zmat)
}

decorate_tf_heatmap <- function(x, y, main, main.col, label.col, ..., draw_time_baseline=TRUE) {

  title(main=list(main, col=main.col, cex=rave_cex.main))

  rave_axis(1, at=pretty(x), tcl=0)
  rave_axis(2, at=quantile(y, 0:5/5) %>% round, tcl=0)

  # this variables are set in the environment, as is BASELINE below
  xy <- cbind(TIME_RANGE, FREQUENCY)

  if(draw_time_baseline) {
    polygon(c(xy[,1], rev(xy[,1])) , rep(xy[,2], each=2), lty=2, lwd=3, border=label.col)

    #draw baseline region
    abline(v=BASELINE, lty=3, lwd=2, col=label.col)

    # label baseline region
    text(BASELINE %>% median, quantile(y, .7), 'baseline', col=label.col, cex=rave_cex.lab, pos=3)
    arrows(BASELINE[1], quantile(y, .7), BASELINE[2], col=label.col, length=.1, code=3)
  }

}


draw_color_bar <- function(rng, crp) {
  cbar <- matrix(seq(-max_zlim, max_zlim, length=length(crp))) %>% t
  par(mar=c(5.1, 5.1, 2, 2))
  image(cbar,
    col=crp, axes=F, ylab='Mean % Signal Change', main='[' %&% paste0(actual_lim, collapse=':') %&% ']',
    font=1, cex.main=rave_cex.main, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis)

  # title(main=list(paste0('[', paste0(actual_lim, collapse=':'), ']'), cex=rave_cex.main))
  rave_axis(2, at=0:2/2, labels = c(-max_zlim, 0, max_zlim), tcl=0.3); box();

}


