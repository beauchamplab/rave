source('plot_funcs.R')
source('rave_calculators.R')

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
# expects by_trial_heat_map_data to exist
by_trial_heat_map <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    # if the user wants the data to be sorted by trial type (rather than trial number) then we
    # # need to sort the data

    decorator <- trial_hm_decorator
    # do we need to sort the trials into trial type ordering? (instead of just ascending by trial #)
    if(exists('sort_trials_by_type')) {
        if(isTRUE(sort_trials_by_type)) {
            for(ii in which(has_trials)) {
                by_trial_heat_map_data[[ii]] %<>% reorder_trials_by_type
            }
            # change the y axis and draw label boundaries
            decorator <- add_decorator(trial_type_boundaries_hm_decorator,
                                       function(map,x,y,...) trial_hm_decorator(map,x,y,yax=FALSE))
        }
    }

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data, y=function(m) seq_len(dim(m)[2L]),
                        ylab='Trials', HM_DECORATOR=decorator, allow_log_scale=FALSE)

}

window_highlighter <- function(ylim, draw_labels=TRUE) {
    mapply(function(x, y, txt) {
        clr <- rave_colors[[toupper(txt %&% '_window')]]
        do_poly(x, range(y), col=clr)
        if(draw_labels)
            text(min(x), max(y), txt, col=clr, adj=c(0,1))
    },
    list(BASELINE, TIME_RANGE), list(ylim, ylim), c('baseline', 'analysis')
    )
    abline(h=0, col='gray70')
}

# helper function for drawing vertical borders
vertical_borders <- function(x, y, lbl, clr, ..., alpha=150, lwd=4, draw_labels=TRUE) {
    abline(v=range(x), col=getAlphaRGB(clr, alpha), lwd=lwd, ...)

    if(draw_labels)
        text(median(x), max(y), '<- ' %&% lbl %&% ' ->', col=clr)
}

window_lines <- function(ylim, draw_labels=TRUE) {
    txts <- c('baseline', 'analysis')
    mapply(vertical_borders,
           list(BASELINE, TIME_RANGE),
           list(ylim, ylim),
           txts,
           rave_colors[toupper(txts %&% '_window')]
    )
    abline(h=0, col='gray70')
}

ts_labels_only <- function(plot_data) {
    ii <- which(plot_data %>% get_list_elements('has_trials'))
    nms <- plot_data %>% get_list_elements('name') %>% extract(ii)

    legend('topleft', legend=nms, ncol=ceiling(length(ii)/3),
           inset=c(.025,.075), bty='n',
           text.col=get_color(ii), cex=rave_cex.lab)
}

# show power over time with MSE by condition
time_series_plot <- function(plot_data, x, DECORATOR=ts_labels_only, SHADER=window_highlighter,
                             title, draw_labels=TRUE) {
    # validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    if(missing(x)) x <- time_points

    ylim <- pretty(get_data_range(plot_data), min.n=2, n=4)

    # the unlist here will strip out the NULLS for us

    if(missing(title)) {
        ns <- unlist(lapply(plot_data, getElement, 'N'))
        title <- 'Freq ' %&% paste0(round(FREQUENCY), collapse=':') %&%
                        ' || Ns ' %&% paste0(ns, collapse=', ')
    }

    plot.clean(x, ylim, xlab='Time (s)', ylab='% Signal Change', main='')
    rave_main(title)

    # draw polys and labels for baseline and analysis ranges
    SHADER(ylim, draw_labels)

    # draw each time series
    for(ii in seq_along(plot_data)) {
        with(plot_data[[ii]], {
            if(has_trials) {
                ebar_polygon(x, data[,1], data[,2],
                             add_line = TRUE, col=get_color(ii))
            }
        })
    }

    #if someone wants to add decorations, now is the time
    # we could consider adding this inside the above for loop, not sure which
    # is preferable
    if(is.function(DECORATOR)) DECORATOR(plot_data)

    rave_axis(1, pretty(x))
    rave_axis(2, ylim)

    invisible(plot_data)
}


heat_map_axes <- function(x, y, xax=TRUE, yax=TRUE, yntick=6) {
    if(missing(x)) x <- time_points
    if(missing(y)) y <- frequencies

    if(xax) rave_axis(1, at=pretty(x), tcl=0, lwd=0)
    if(yax) rave_axis(2, at=quantile(y, 0:(yntick-1)/(yntick-1)) %>% round, tcl=0, lwd=0)
}


#this function is relying on the environment-wide variable BASELINE
trial_hm_decorator <- function(hmap, x, y, xax=TRUE, yax=TRUE, ...) {
    abline(v=BASELINE, lty=3, lwd=2)

    heat_map_axes(x,y, xax=xax, yax=yax)
}

# decorate a heatmap
tf_hm_decorator <- function(hmap, x, y, ..., label.col='black', draw_time_baseline=TRUE, xax=TRUE, yax=TRUE) {
    heat_map_axes(x,y, xax=xax, yax=yax)

    if(draw_time_baseline) {
        # these variables are set in the environment, as is BASELINE below
        xy <- cbind(TIME_RANGE, FREQUENCY)

        polygon(c(xy[,1], rev(xy[,1])) , rep(xy[,2], each=2), lty=2, lwd=3, border=label.col, ...)

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
        pretty(get_data_range(group_data), high.u.bias = 100, n=4, min.n=3)
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
    if(not_null(attr(group_data, 'stats')))
        rave_main(as.title(pretty(attr(group_data, 'stats'))))

    #emphasize the means
    lsize <- (1/3)*mean(unique(diff(x)))
    # this means there is only 1 group. If there is one group, the barplot seems to get placed at 0.7, with
    # the usr range being 0.16 - 1.24.
    if(is.na(lsize)) lsize <- 1/3

    if(missing(jitr_x)) jitr_x <- 0.75*lsize

    if(missing(cols)) cols <- get_color(seq_along(group_data))


    # Ensure all parameters are sufficiently long. This feels extravagant, but needed because we're indexing into these variables
    # and we don't want to reach beyond the end
    par_rep <- function(y) rep_len(y, length(group_data))

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
