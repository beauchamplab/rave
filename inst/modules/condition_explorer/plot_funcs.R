require(magrittr)


#this file and plot_helpers should be merged/sorted
source('plot_helpers.R')

rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')

rave_heat_map_colors <- rave_color_ramp_palette(1001)

# put this hear for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('orangered', 'orange', 'dodgerblue3', 'purple3', 'darkgreen', 'brown')

rave_colors <- list('BASELINE_WINDOW'='gray60', 'ANALYSIS_WINDOW' = 'salmon2', 'GROUP'=group_colors)


rave_main <- function(main, cex=rave_cex.main, col='black', font=1) {
    title(main=list(main, cex=cex, col=col, font=font))
}

### FIXME
# this doesn't really work and it isn't clear where it should go
brushed = function(event, env){
    if(is.null(event)){
        msg = 'Please Choose on plot'
    } else{
        fmax = max(power$dimnames$Frequency)
        tmax = max(power$dimnames$Time)
        tmin = min(power$dimnames$Time)
        msg = sprintf('Frequency range: %.1fHz - %.1fHz',
                      event$ymin * fmax, event$ymax * fmax, event$xmin * (tmax-tmin), event$xmax * (tmax-tmin))
    }
    env$msg = msg
}

# allow color cycling
get_color <- function(ii) {
  group_colors[ii %% length(group_colors) + 1]
}

#' @author John Magnotti
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar. By default it is setup for time/freq, but by swapping labels and decorators you can do anything
draw_many_heat_maps <- function(hmaps, x, y, xlab='Time', ylab='Frequency', DECORATOR=tf_hm_decorator) {
  k <- hmaps %>% get_list_elements('has_trials') %>% sum

  layout_heat_maps(k)

  actual_lim = get_data_range(hmaps)
  # check if there is a plot range variable set
  if(!exists('max_zlim') | max_zlim==0) {
    max_zlim <- max(abs(actual_lim))
  }

  # let's assume y is a vector that we can use
  ys <- y
  lapply(hmaps, function(map){
    if(map$has_trials){
      # if y is a function, then use it to build the ys
      if(is.function(y)) {ys <- y(map$data)}

      draw_img(map$data, x = x, y = ys,
        xlab=xlab, ylab=ylab, zlim = c(-max_zlim, max_zlim), main = map$name, DECORATOR = DECORATOR)
    }
  })

  rave_color_bar(max_zlim, actual_lim)
}


#' @author John Magnotti
#' @Note We are just plotting image(zmat) rather than t(zmat) as you might expect. the rave_calculators know this so we can
#' save a few transposes along the way
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


