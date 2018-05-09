source('plot_funcs.R')
source('rave_calculators.R')

# show power over time with MSE by condition
time_series_plot <- function(plot_data) {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    ylim <- pretty(get_data_range(plot_data), min.n=2, n=4)

    ns <- sapply(plot_data, getElement, 'N')


    title <- 'Freq ' %&% paste0(round(FREQUENCY), collapse=':') %&% ' || Ns ' %&% paste0(ns, collapse=', ')

    plot.clean(time_points, ylim, xlab='Time (s)', ylab='% Signal Change', main=title)

    # draw polys and labels for baseline and analysis ranges
    mapply(function(x, y, txt) {
            clr <- rave_colors[[toupper(txt %&% '_window')]]
            do_poly(x, range(y), col=clr)
            text(min(x), max(y), txt, col=clr, adj=c(0,1))
        },
        list(BASELINE, TIME_RANGE), list(ylim, ylim), c('baseline', 'analysis')
    )

    abline(h=0, col='gray70')

    # draw each time series
    for(ii in seq_along(plot_data)) {
        with(plot_data[[ii]], {
            if(has_trials) {
                ebar_polygon(time_points, data[,1], data[,2], add_line = TRUE, col=get_color(ii))


                #FIXME draw these in nicer locations
                ypos <- ii / length(plot_data) * .9 * max(ylim)

                text(max(BASELINE), ypos, name, col=get_color(ii), font=2)
            }
        })
    }

    rave_axis(1, pretty(time_points))
    rave_axis(2, ylim)
}

#this function is relying on the environment-wide variable BASELINE
trial_hm_decorator <- function(x, y, main, ...) {
    rave_main(main)

    rave_axis(1, at=pretty(x), tcl=0, lwd=0)
    rave_axis(2, at=median_ticks(max(y)), tcl=0, lwd=0)

    abline(v=BASELINE, lty=3, lwd=2)
}

# decorate a heatmap
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

#
# group_data is a list
# this is a list to allow different N in each group
# NB: the reason we use barplot here is that it takes care of the width between groups for us, even though by default we don't actually show the bars
trial_scatter_plot = function(group_data, ylim, bar.cols=NA, bar.borders=NA, cols, ebar.cols='gray30', ebar.lwds=3, jitr_x,
                              pchs=19, pt.alpha=175, xlab='Group', ylab='Mean % Signal Change', ebar.lend=2, ...) {

    nms <- group_data %>% get_list_elements('name')
    #
    # #yes, sometimes people use the same name for different groups, or don't give names. Let's create new names
    gnames <- paste0(LETTERS[seq_along(nms)], nms)

    #
    ns <- group_data %>% get_list_elements('N')

    yax <- do_if(missing(ylim), {
        pretty(get_data_range(scatter_bar_data), high.u.bias = 100, n=4, min.n=3)
    }, ylim)

    #there are edge cases where length(mses) != length(names), take care of this with `ind` below
    bp_names <- paste0(nms, ' (N=' %&% ns %&%')')

    # this creates space for empty groups -- is this expected behavior? It is good to preserve the color
    # mapping, but I'd rather not have the empty, space... so we need to preserve the colors but not the empty space
    ind <- which(unlist(lapply(group_data, '[[', 'has_trials')))
    mses <- sapply(ind, function(ii) group_data[[ii]]$mse)

    x <- rave_barplot(mses[1,],
                      ylim=.fast_range(yax) %>% stretch(.01), col=bar.cols, border=bar.borders,
                      ylab=ylab, names.arg=bp_names[ind], xlab=xlab, axes=F, ...)

    rave_axis(2, at=yax)

    if(min(yax) < 0) abline(h=0, col='lightgray')

    # grabbing an attribute from the group data
    rave_main(attr(group_data, 'stats')$title)

    #emphasize the means
    lsize <- (1/3)*mean(unique(diff(x)))
    # this means there is only 1 group. If there is one group, the barplot seems to get placed at 0.7, with
    # the usr range being 0.16 - 1.24.
    if(is.na(lsize)) lsize <- 1/3

    if(missing(jitr_x)) jitr_x <- 0.75*lsize

    if(missing(cols)) cols <- get_color(seq_along(group_data))


    # Ensure all parameters are sufficiently long. This feels extravagant, but needed because we're indexing into these variables
    # and we don't want to reach beyond the end
    par_rep <- function(y) rep_len(y, length(scatter_bar_data))

    cols %<>% par_rep
    pchs %<>% par_rep
    bar.cols %<>% par_rep
    ebar.cols %<>% par_rep
    bar.borders %<>% par_rep

    # x may not be the same length as group_data because we're skipping empty groups
    # we still want everything else to be based on group number
    xi <- 1
    for(ii in seq_along(group_data)) {
        if(group_data[[ii]]$has_trials) {

            lines(x[xi] + c(-lsize, lsize), rep(mses[1, xi], 2), lwd=3, lend=ebar.lend, col=ebar.cols[ii])

            add_points(x[xi], group_data[[ii]]$data,
                       col=getAlphaRGB(cols[ii], pt.alpha), pch=pchs[ii], jitr_x=jitr_x)

            ebars.y(x[xi], mses[1,xi], mses[2,xi],
                    lwd=ebar.lwds, col=ebar.cols[ii], code=0, lend=ebar.lend)
            xi <- xi+1
        }
    }

    # in case people need to further decorate
    invisible(x)
}
