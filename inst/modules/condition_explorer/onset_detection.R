require(rave)

rave_options(data_dir='/Volumes/data/rave_data/data/', raw_data_dir='/Volumes/data/rave_data/raw/')

rave_prepare(subject = 'YAB_congruency1',
             electrodes = 14,
             epoch = 'YAB',
             time_range = c(1, 2))

source('plot_funcs.R')
source('plot_helpers.R')
source('onset_detection_raveui.R')


# line_plot_data should be a list
# each element of line_plot_data should have the following elements:
#       has_t: a boolean that can be queried to check for data existence
#               (this allows for lists with "holes" and causes colors to be skipped appropriately)
#       N: number of trials
#       mse: two-column matrix. col_1 = central tendency (mean), col_2 = dispersion (SE)
#       lim: a
rave_over_time_plot <- function(line_data) {
    # make plot and color the baseline / analysis windows
    ylim <- pretty(get_data_range(line_plot_data, 'lim'), min.n=2, n=4)

    ns <- sapply(line_plot_data, getElement, 'N')

    title <- 'Freq ' %&% paste0(round(FREQUENCY), collapse=':') %&% ' || Ns ' %&% paste0(ns, collapse=', ')

    plot.clean(time_points, ylim, xlab='Time (s)', ylab='% Signal Change', main=title)

    # draw polys ans labels for baseline and analysis ranges
    mapply(function(x, y, clr, txt) {
        do_poly(x, range(y), col=clr);
        text(min(x), max(y), txt, col=clr, adj=c(0,1))
    }, list(BASELINE, TIME_RANGE), list(ylim, ylim),
    rave_colors[c('BASELINE_WINDOW', 'ANALYSIS_WINDOW')],
    c('baseline', 'analysis')
    )

    abline(h=0, col='gray70')

    # draw each time series
    for(ii in seq_along(line_plot_data)) {
        with(line_plot_data[[ii]], {
            if(has_t) {
                ebar_polygon(time_points, mse[,1], mse[,2], add_line = TRUE, col=get_color(ii))

                ypos <- ii / length(line_plot_data) * .9 * max(ylim)

                text(max(BASELINE), ypos, name, col=get_color(ii), font=2)
            }
        })
    }

    rave_axis(1, pretty(time_points))
    rave_axis(2, ylim)
}


over_time_plot <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

     rave_over_time_plot(line_plot_data)
}

# the only difference between this plot and the time x freq plot
# is the data and the decoration, but use the helper function
# to enforce consistent look/feel
by_trial_heat_map <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    # the y variable is changing each time, so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data, x=time_points, y=function(m) 1:ncol(m),
                        ylab='Trials', DECORATOR = trial_hm_decorator)
}

heat_map_plot = function(){
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    draw_many_heat_maps(heat_map_data, time_points, frequencies)
}

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

msg_out = function(){
    return('Put more results here')
}

rave_execute({
    assertthat::assert_that(
        length(electrode) == 1,
        msg = 'No electrode selected'
    )
    electrode = as.integer(electrode)
    from = BASELINE[1]
    to = BASELINE[2]
    alltrials = sort(unique(unlist(lapply(GROUPS, function(x){x$GROUP}))))

    bl_power = cache(
        key = list(electrode, BASELINE, length(alltrials) > 0),
        val = baseline(from, to, electrode)
    )


    # this can be used elsewhere
    has_data = sum(unlist(lapply(heat_map_data, getElement, 'has_t')))

    lapply(GROUPS, function(comp){
        power = bl_power$subset(Trial = Trial %in% comp$GROUP)
        has_t = length(comp$GROUP) > 0
        mean_o_freq = collapse_over_freq(power)
        range = range(mean_o_freq)
        list(
            name = comp$GROUP_NAME,
            has_t = has_t,
            data = t(mean_o_freq),
            range = range
        )
    })-> by_trial_heat_map_data

    # pre-process for over time plot
    lapply(GROUPS, function(comp){
        power <- bl_power$subset(Trial = Trial %in% comp$GROUP)
        mse = collapse_over_freq_and_trial(power)

        list(
            name = comp$GROUP_NAME,
            mse = mse,
            has_t = length(comp$GROUP) > 0,
            lim = range(pm(mse[,1], mse[,2])),
            N=dim(power)[1]
        )
    }) ->
        line_plot_data

}

)

niml_default = function(){
    return(1)
}

if(FALSE) {

    module = ModuleEnvir$new('id', 'LABEL', './onset_detection.R')
    init_app(list(module))


    ls(rave:::.ui_update_repo)

    h = rave:::RAVEHistory$new(path = '~/rave_modules', name = 'John', use_yaml = T)
    h$save(
        memo = "I've done something"
    )


    # heat_map_plot()
    # windowed_comparison_plot()

}


