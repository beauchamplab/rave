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
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar. By default it is setup for time/freq, but by swapping labels and decorators you can do anything
draw_many_heat_maps <- function(hmaps, x, y, xlab='Time', ylab='Frequency', DECORATOR=tf_hm_decorator) {
  k <- hmaps %>% get_list_elements('has_t') %>% sum

  layout_heat_maps(k)

  actual_lim = get_data_range(hmaps)
  # check if there is a plot range variable set
  if(!exists('max_zlim') | max_zlim==0) {
    max_zlim <- max(abs(actual_lim))
  }

  # let's assume y is a vector that we can use
  ys <- y
  lapply(hmaps, function(map){
    if(map$has_t){
      # if y is a function, then use it to build the ys
      if(is.function(y)) {ys <- y(map$data)}

      draw_img(map$data, x = x, y = ys,
        xlab=xlab, ylab=ylab, zlim = c(-max_zlim, max_zlim), main = map$name, DECORATOR = DECORATOR)
    }
  })

  rave_color_bar(max_zlim, actual_lim)
}


#' @author John Magnotti
#' @description Many parameters are just passed to the decorator function, the idea was to be able to separate the plotting of the heatmap from all the accoutrements
draw_img <- function(zmat, x, y, xlab='Time (s)',ylab='Frequency (Hz)',
                     zlim, main='', main.col='black', label.col='black',
                     DECORATOR=tf_hm_decorator, ...) {

  zmat %<>% clip_x(lim=zlim)

  image(x=x, y=y, z=zmat,
        zlim=zlim, col=crp, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, axes=F, useRaster = FALSE, ...)

  DECORATOR(x=x, y=y, main=main, main.col=main.col, label.col=label.col, ...)

  # return the clipped zmat
  invisible(zmat)
}


# setup so that heatmaps look nice and you have enough space for the color bar
# ratio: heatmap to color bar width ratio
# k is the number of heatmaps, excluding the color bar
layout_heat_maps <- function(k, ratio=4) {
  layout(matrix(1:(k+1), nrow=1), widths=c(rep(ratio, k), 1) )
  par(mar=c(5.1, 4.5, 2, 2))
}

median_ticks <- function(k) c(1, k, ceiling(k/2))


#this function is relying on the environment-wide variable BASELINE
trial_hm_decorator <- function(x, y, main, ...) {
  rave_main(main)

  rave_axis(1, at=pretty(x), tcl=0, lwd=0)
  rave_axis(2, at=median_ticks(max(y)), tcl=0, lwd=0)

  abline(v=BASELINE, lty=3, lwd=2)
}

rave_main <- function(main, cex=rave_cex.main, col='black', font=1) {
  title(main=list(main, cex=cex, col=col, font=font))
}

tf_hm_decorator <- function(x, y, main, main.col, label.col, ..., draw_time_baseline=TRUE) {
  rave_main(main, main.col)

  rave_axis(1, at=pretty(x), tcl=0, lwd=0)
  rave_axis(2, at=quantile(y, 0:5/5) %>% round, tcl=0, lwd=0)

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

str_rng <- function(rng) sprintf('[%s]', paste0(rng, collapse=':'))

rave_color_bar <- function(zlim, actual_lim, clrs=rave_heat_map_colors, ylab='Mean % Signal Change',
  mar=c(5.1, 5.1, 2, 2)) {

  cbar <- matrix(seq(-zlim, zlim, length=length(rave_heat_map_colors))) %>% t
  par(mar=mar)
  image(cbar,
    col=clrs, axes=F, ylab=ylab, main='',
    cex.main=rave_cex.main, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis)

  rave_main(str_rng(actual_lim))
  rave_axis(2, at=0:2/2, labels = c(-zlim, 0, zlim), tcl=0.3)
  box()
}


