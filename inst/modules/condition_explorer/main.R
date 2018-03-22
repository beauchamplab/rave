# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)


# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'KC_congruency1',
             electrodes = 73,
             epoch = 'KC',
             time_range = c(1, 2))

source('rave_calculators.R')
source('condition_explorer_ui.R')
source('condition_explorer_plots.R')

rave_ignore({

    # rave_options(data_dir = '/Volumes/data/rave_data/data/',
    #              module_lookup_file = '~/Dropbox/RAVE_DEV/module_dev.csv',
    #              crayon_enabled=TRUE)

    GROUPS = list(
        list(
            GROUP_NAME = 'G1',
            GROUP = unique(trials)[-c(1:3)]
        ),
        list(
            GROUP_NAME = 'G2',
            GROUP = unique(trials)[1:3]
        )
    )

})

# time series plot
over_time_plot <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    time_series_plot(line_plot_data)
}

# by trial plot with statistics
windowed_comparison_plot <- function(){
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    trial_scatter_plot(scatter_bar_data)
}

#basic time frequency plot
heat_map_plot = function(){
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    draw_many_heat_maps(heat_map_data, time_points, frequencies)
}

# the only difference between this plot and the time x freq heat_map_plot
# is the data and the decoration. Use the core heatmap function
# to enforce consistent look/feel
by_trial_heat_map <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data, x=time_points, y=function(m) seq_len(dim(m)[2L]),
                        ylab='Trials', DECORATOR = trial_hm_decorator)
}

msg_out = function() {
    return('')#sprintf('length: %d', length(heat_map_data)))
}

async_out = function(){
    validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
    async_msg
}

rave_execute({
    assertthat::assert_that(length(electrode) == 1,msg = 'No electrode selected')

    electrode = as.integer(electrode)

    #baseline all available trials

    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    bl_power = cache(
        key = list(electrode, BASELINE, any_trials),
        val = baseline(BASELINE[1],  BASELINE[2], electrode)
    )

    # we were repeating a lot of calculations and looping over GROUPS too many times
    # let's clean that up

    has_trials <- unlist(lapply(GROUPS, function(g) length(g$GROUP) > 0))

    #helper file to build lists with common elements pre-populated
    build_list <- function() {
        ##NB: this is faster than using replicate(length(has_trials))
        lapply(seq_len(length(has_trials)), function(ii)
            list('has_trials' = has_trials[ii],
                 'name' = GROUPS[[ii]]$GROUP_NAME))
    }

    # declare all our variables with pre-populated 'has_trials' and 'name' variables
    build_list() ->
        scatter_bar_data -> line_plot_data -> by_trial_heat_map_data -> heat_map_data

    flat_data <- data.frame()
    # now we loop through only those groups with data
    for(ii in which(has_trials)) {
        power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$GROUP)

        #helper function to clean up the syntax below, value here should be a function
        # we're relying on power being defined above, so don't move this function out of this scope
        `add_data<-` <- function(x, value) {
            x[c('data', 'range', 'N')] <- list(value, .fast_range(value), dim(power)[1L])
            x
        }

        add_data(heat_map_data[[ii]]) <- collapse_over_trial(power)
        add_data(by_trial_heat_map_data[[ii]]) <- collapse_over_freq(power)

        add_data(line_plot_data[[ii]]) <- collapse_over_freq_and_trial(power)
        # we want to make a special range for the line plot data that takes into account mean +/- SE
        line_plot_data[[ii]]$range <- .fast_range(pm(line_plot_data[[ii]]$data[,1],
                                                     line_plot_data[[ii]]$data[,2]))

        add_data(scatter_bar_data[[ii]]) <- collapse_over_freq_and_time(power)

        # for the scatter_bar_data we also need to get m_se within condition
        scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)

        flat_data %<>% rbind(data.frame('group'=ii, 'y' = scatter_bar_data[[ii]]$data))
    }
    flat_data$group %<>% factor


    # this can be used elsewhere
    has_data = sum(unlist(lapply(heat_map_data, getElement, 'has_trials')))

    # calculate some statistics

    # calculate the statistics here so that we can add them to the niml_out
    # if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
    if(length(unique(flat_data$group)) > 1) {
        # we need to check if they have supplied all identical data sets

        # easy way is to check that the trials are the same?
        sapply(GROUPS, '[[', 'GROUP')

        g1 <- GROUPS[[which(has_trials)[1]]]$GROUP
        if(all(sapply(which(has_trials)[-1], function(ii) identical(GROUPS[ii]$GROUP, g1)))) {
            result_for_suma <- get_t(flat_data$y[flat_data$group==flat_data$group[1]])
            title <- bquote('Non-unique conditions.' ~H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(result_for_suma[1]) * ',' ~ t==.(result_for_suma[2]) * ',' ~ p==.(result_for_suma[3]))
        } else {
            result_for_suma <- get_f(y ~ group, flat_data) %>% pretty
            title <- bquote(H[0] ~ mu[i] == mu[j] * ';' ~ R^2 == .(result_for_suma[1]) ~ ',' ~ F == .(result_for_suma[2]) ~ ','~ p==.(result_for_suma[3]))
        }
    } else {
        result_for_suma <- flat_data$y %>% get_t %>% pretty
        title <- bquote(H[0] * ':' ~ mu == 0 * ';' ~ bar(x)==.(result_for_suma[1]) * ',' ~ t==.(result_for_suma[2]) * ',' ~ p==.(result_for_suma[3]))
    }

    attr(scatter_bar_data, 'stats') <- list('result' = result_for_suma, 'title'=title)

},{
    if(.is_async){
        async_msg = 'Running in the background'
    }
}, async = {
    logger('Aync test')
    # rm(list = ls(all.names = T))
    Sys.sleep(0.5)
    aaa = heat_map_plot
    bbb = `%>%`
    nms = ls(all.names = T)
    async_msg = paste(search(), collapse = ', ')
}
)

niml_default = function(){
    return(result_for_suma)
}

if(FALSE) {
    heat_map_plot()
    windowed_comparison_plot()
}

