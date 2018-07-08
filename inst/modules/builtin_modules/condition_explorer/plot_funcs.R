require(magrittr)

# this doesn't really work
brushed = function(event, env){
    if(is.null(event)){
        msg = 'Please Choose on plot'
    } else{
        fmax = 200#max(power$dimnames$Frequency)
        tmax = 2#max(power$dimnames$Time)
        tmin = -1#min(power$dimnames$Time)
        msg = sprintf('Frequency range: %.1fHz - %.1fHz',
                      event$ymin * fmax, event$ymax * fmax, event$xmin * (tmax-tmin), event$xmax * (tmax-tmin))
    }
    logger(msg)
    # env$msg = msg
}



#this file and plot_helpers.R should be merged/sorted
source('plot_helpers.R')

rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')

rave_heat_map_colors <- rave_color_ramp_palette(1001)

# put this hear for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('purple3', 'orange', 'dodgerblue3', 'darkgreen', 'orangered', 'brown')

# allow color cycling
get_color <- function(ii) {
    group_colors[ii %% length(group_colors) + 1]
}

rave_colors <- list('BASELINE_WINDOW'='gray60', 'ANALYSIS_WINDOW' = 'salmon2', 'GROUP'=group_colors,
                    'TRIAL_TYPE_SEPARATOR'='gray40')

rave_main <- function(main, cex=rave_cex.main, col='black', font=1) {
    title(main=list(main, cex=cex, col=col, font=font))
}

### FIXME


#' @author John Magnotti
#' @description Easy way to make a bunch of heatmaps with consistent look/feel and get a colorbar. By default it is setup for time/freq, but by swapping labels and decorators you can do anything
draw_many_heat_maps <- function(hmaps, x, y, xlab='Time', ylab='Frequency',
                                allow_log_scale=TRUE, show_color_bar=TRUE, HM_DECORATOR=tf_hm_decorator, ...) {
    k <- hmaps %>% get_list_elements('has_trials') %>% sum

    layout_heat_maps(k)

    if(missing(x)) x <- time_points
    if(missing(y)) y <- frequencies

    # use the actual lim if there isn't a plot range set
    actual_lim = get_data_range(hmaps)

    max_zlim <- get0('max_zlim', ifnotfound = 0)
    if(max_zlim==0) {
        max_zlim <- max(abs(actual_lim))
    }

    log_scale <- get0('log_scale', ifnotfound = FALSE)
    if(allow_log_scale & isTRUE(log_scale)){
        log_scale <- 'y'
    } else {
        log_scale <- ''
    }

    # let's assume y is a vector that we can use
    lapply(hmaps, function(map){
        if(map$has_trials){
            # if y is a function, then use it to build the ys
            # this feels a little ridiculous, as we don't know the type of y, is there a cleaner way?
            # the problem is that draw_img() needs 'y' in order to put the image at the right location, so it is more than
            # just a decoration issue
            .y <- do_if(is.function(y), y(map$data), y)

            draw_img(map$data, x = x, y = .y, xlab=xlab, ylab=ylab, zlim = c(-max_zlim, max_zlim),
                     main = map$name, log=log_scale, ...)

            HM_DECORATOR(map, x=x, y=.y)
        }
    })

    if(show_color_bar){
        par(mai = c(0.6732, 0.5412, 0.5412, 0.2772))
        rave_color_bar(max_zlim, actual_lim)
    }

}


#' @author John Magnotti
#' @Note We are just plotting image(zmat) rather than t(zmat) as you might expect. the rave_calculators know this so we can
#' save a few transposes along the way
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators
draw_img <- function(zmat, x, y, xlab='Time (s)', ylab='Frequency (Hz)',
                     zlim, log='', ...) {

    zmat %<>% clip_x(lim=zlim)

    image(x=x, y=y, z=zmat,
          zlim=zlim, col=crp, xlab=xlab, ylab=ylab, cex.lab=rave_cex.axis, axes=F, useRaster = FALSE, log=log, ...)

    # return the clipped zmat
    invisible(zmat)
}


# setup so that heatmaps look nice and you have enough space for the color bar
# ratio: heatmap to color bar width ratio
# k is the number of heatmaps, excluding the color bar
layout_heat_maps <- function(k, ratio=4) {
    layout(matrix(1:(k+1), nrow=1), widths=c(rep(ratio, k), lcm(5)) )

    par(mar=c(5.1, 4.5, 2, 2))
}

median_ticks <- function(k) c(1, ceiling(k/2), k)

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

    invisible(zlim)
}


midpoint <- function(x) {
    sapply(seq_along(x)[-1], function(ii) {
        (x[ii] - x[ii-1])/2 + x[ii-1]
    })
}

# this is really only used by the by_trial heat map, but that gets used in multiple modules, so it's here....
reorder_trials_by_type <- function(bthmd) {
    # we want to sort but preserve the order that the conditions were added to the group, but this doesn't do that
    ind <- bthmd$trials %>% unique %>%
        sapply(function(ttype) which(ttype==bthmd$trials), simplify = FALSE)

    bthmd$data <- bthmd$data[,unlist(ind)]
    bthmd$lines <- cumsum(c(sapply(ind, length)))
    bthmd$ttypes <- names(ind)

    return(bthmd)
}

#the idea here is to allow decorators to be stacked/piped more easily while still deferring execution until
# a more opportune time. f1 %>% f2 won't pass along the ... parameters (although it will pass along x), so this format is not used here
add_decorator <- function(f1, f2, ...) {
    force(f1); force(f2)
    return (function(.IN., ...) {
        f1(.IN., ...)
        f2(.IN., ...)
    })
}



# here we are creating a new decorator that uses component-esque pattern to draw
# lines after the initial decoration is done
trial_type_boundaries_hm_decorator <- function(map, ...) {
    with(map, {
        if(length(map$lines)>1) {
            abline(h=remove_tail(map$lines) + 0.5, lwd=2, col=rave_colors$TRIAL_TYPE_SEPARATOR)
            # draw the trial type labels
            yat <- c(map$lines[1]/2, midpoint(map$lines))
        } else {
            yat <- median(y)
        }
        axis(2, tcl=0, lwd=0, at=yat, labels=map$ttypes, cex=rave_cex.main)
    })

    invisible(map)
}

#sometimes we don't need the last item
# if you give me < 1 I will return the full vector with a warning
# this is helpful as it (I think) avoids error checking and
# should be reasonable for most events
remove_tail = function(x, k=1) {

    if(k<1) {
        warning('Tried removing less than 1 element, k = ', k)
        return(x)
    }

    stopifnot(k>=1 & k<length(x))
    #seems like it's faster to rewrite as selecting from the beginning
    #rather than using negative indexing
    # x[-(length(x):(length(x) - (k-1)))]
    x[1:(length(x)-k)]
}
