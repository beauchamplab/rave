# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'KC_congruency1_sliding',
             electrodes = 23,
             epoch = 'KCa',
             time_range = c(1, 2))


source('rave_calculators.R')
source('onset_detection_raveui.R')
source('condition_explorer_plots.R')

if(F){
  m = ModuleEnvir$new(module_id = 'id', 'Test', script_path = './inst/modules/builtin_modules/onset_detection.R'); init_app(m)
}



rave_ignore({
    rave_options(data_dir = '/Volumes/data/rave_data/data/',
                 module_lookup_file = '~/Dropbox/RAVE_DEV/module_dev_john.csv.csv',
                 crayon_enabled=TRUE)

    GROUPS = list(
        list(
            GROUP_NAME = 'G1',
            GROUP = c('drive_a', 'known_v')
        )
        ,
        list(
            GROUP_NAME = 'G2',
            GROUP = c('drive_a')
        ),
        list(
            GROUP_NAME = 'G3',
            GROUP = c('known_v')
        ),
        list(
            GROUP_NAME = '',
            GROUP=c()
        )
    )

    max_zlim=100
    collapse_using_median=FALSE
})


od_trial_scatter_plot <- function() {
    validate(need((exists('od_trial_data') && not_null(od_trial_data)), "No significant onsets"))

    # call the regular trial_scatter plot
    trial_scatter_plot(od_trial_data)
}

od_by_trial_heat_map <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    # by default we'll take the usual by-trial heat map decorator
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

    # add another decorator so we can add vertical lines at the trial onsets, if detected
    .decorator <- function(map, ...) {
        if(not_NA(map$on_time)) {
            abline(v=map$on_time, lwd=3)
        }
        invisible(map)
    }

    decorator %<>% add_decorator(.decorator)

    # the y variable is changing each time,
    # so we provide a function that will be used to calculate the
    # y variable on a per map basis
    draw_many_heat_maps(by_trial_heat_map_data, x=time_points, y=function(m) seq_len(dim(m)[2L]),
                        ylab='Trials', HM_DECORATOR = decorator, allow_log_scale=FALSE)
}

# time series plot
od_over_time_plot <- function() {
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))


    #this decorator needs to be aware if the window is symmetric about onset
    # or if it is a look-ahead window
    onset_decorator = function(lpd) {
        for (ii in seq_along(lpd)) {
            clr <- get_color(ii)
            if(not_NA(lpd[[ii]]$on_time)) {
                with(lpd[[ii]], {
                    do_poly(c(on_time, on_time+window), range, getAlphaRGB(clr, 50))
                    abline(v = on_time, lwd = 1, lty = 2, col = clr)
                })
                lpd[[ii]]$name <- with(lpd[[ii]], name %&% ' (' %&% round(on_time, 2) %&% ')')
            } else{
                if (lpd[[ii]]$has_trials) {
                    lpd[[ii]]$name <- lpd[[ii]]$name %&% '(None)'
                }
            }
        }

        # call the other decorator to add labels to the plot
        # we've hijacked it's legend by adding the onset times to the names
        # so they will show up in the right place
        ts_labels_only(lpd)
    }


    # provide a decorator so we can add the onset times to the plot
    time_series_plot(line_plot_data, SHADER = window_lines,
                     DECORATOR = onset_decorator)
}

od_msg_out = function() {

    # put analysis information in here

    return(sprintf('length: %d', length(by_trial_heat_map_data)))
}

od_async_out = function(){
    validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
    async_msg
}


detect_onset.consecutive_sig <- function(ymat, consecutive=3,
                                         alpha=0.05, Halt='two.sided') {

    # turns out to be faster to just do ALL the t-tests rather than do only the ones needed
    # this function does a one-sample t-test per ROW
    ps <- .fast_one_sample_t_mat(ymat, sided=ifelse(Halt=='two.sided', 2, 1)) < alpha

    # get the run length encoding to find how many TRUEs and FALSEs we have in a row
    prle <- rle(ps)

    # find the first time the length of a TRUE sequence meets our criterion
    sigp <- which(prle$lengths[prle$values] >= consecutive)[1]

    # if we never met the criterion, bail
    if(length(sigp)<1 | is.na(sigp)) return (NA)

    # we need to move to the right point in the vector if the found location is
    # not right at the beginning
    if(sigp>1)
        sigp <- max(cumsum(prle$lengths[prle$values])[sigp-1])+1

    found <- which(ps)[sigp]

    #this will be the location of the first significant p-value in the sequence of at least consecutive long
    return (found)
}

#just take the squared correlation
get_r <- function(ti, y, k=5) {
    # y is a matrix, and we need to rep k according to the number of columns
    .fast_pearson(c(y[(ti-k):(ti+(k-1)),]),
                  rep(1:(2*k), dim(y)[2L]))
}

# because of baselining at t=0, might we get a spurious change at t=0? but it should
# be equally likely to be negative as positive under H0, right?
detect_onset.lm <- function(bsl, y, half_window=5, ALPHA=0.025, two.sided=FALSE) {
    b <- half_window + 1
    e <- dim(bsl)[1L] - (half_window-1)

    # grab a linear r^2 for all time points in baseline, then find the alpha %tile
    h0 <- vapply(b:e, get_r, y=bsl, k=half_window, FUN.VALUE = 1.0)

    h0 <- do_if(two.sided, quantile(abs(h0), 1-ALPHA),
                           quantile(h0, 1-ALPHA))

    # grab a linear r^2 for all time points in analysis window
    # the begin point is the same (half_window+1)
    e <- dim(y)[1L] - (half_window-1)
    hA <- vapply(b:e, get_r, y=y, k=half_window, FUN.VALUE = 1.0)

    #any winners?
    res <- which(hA>h0)[1]
    cat('best: ', (hA[which(hA>h0)[1]]), '\n')

    if(length(res)<1 | is.na(res)) return (NA)

    return(res)
}


rave_execute({
    assertthat::assert_that(length(electrode) == 1,msg = 'No electrode selected')

    electrode = as.integer(electrode)

    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    # I think the cache persists across modules, so may as well take advantage
    bl_power <- cache(
        key = list(subject$subject_id, electrode, BASELINE, any_trials, preload_info$reference_name),
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
                 'name' = GROUPS[[ii]]$GROUP_NAME,
                 'on_time'=NA)
        )
    }

    # declare all our variables with pre-populated 'has_trials' and 'name' variables
    build_list() ->
        line_plot_data -> by_trial_heat_map_data

    # load up our collapsers into a list, this allows us to swap them out as needed
    collapse <- rave_collapsers.mean()

    # swap out the collapsers for medians
    if (exists('collapse_using_median')) {
        if(isTRUE(collapse_using_median)) {
            collapse <- rave_collapsers.median()
        }
    }

    # now we loop through only those groups with data
    for(ii in which(has_trials)) {
        power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$GROUP)

        #helper function to clean up the syntax below, value here should be a function
        # we're relying on power being defined above, so don't move this function out of this scope
        `add_data<-` <- function(x, value) {
            x[c('data', 'range', 'N', 'trials')] <- list(value, .fast_range(value), dim(power)[1L], power$dimnames$Trial)
            x
        }

        add_data(by_trial_heat_map_data[[ii]]) <- collapse$over_frequency(power)

        add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(power)
        # we want to make a special range for the line plot data that takes into account mean +/- SE
        line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                             line_plot_data[[ii]]$data[,2]))

        # this should be set to a particular function based on the users checkbox selector
        baseline_window <- time_points %within% BASELINE
        analysis_window <- time_points %within% TIME_RANGE

        y <- by_trial_heat_map_data[[ii]]$data[analysis_window,]
        bsl <- by_trial_heat_map_data[[ii]]$data[baseline_window,]

        .alpha=0.05
        if(exists('od_alpha')) {
            .alpha = od_alpha
        }
        .window = 5
        if(exists('od_window_size')) {
            .window = od_window_size# / 2
            #we do size / 2 for symmetric methods, but not for look ahead methods
        }

        warning('hard coded time-step')
        time_step <- 0.01 #time_points[2]-time_ponts[1]

        #find the onset -- here we switch between algorithms based on the drop-down
        on <- detect_onset.consecutive_sig(y, consecutive = .window, alpha = .alpha)

        # backout to the original time series to find the onset if it exists
        line_plot_data[[ii]]$on_time <- do_if(is.na(on), NA,
                                                        time_points[which(analysis_window)[on]])

        # copy over the on_time variable to the by_trial data so we can plot that on the graph
        by_trial_heat_map_data[[ii]]$on_time = line_plot_data[[ii]]$on_time

        # we need to take into account the 0th position
        # if win=3, then we go from 1:3 => 1:(win-1), then convert to time units
        line_plot_data[[ii]]$window <- (.window-1)*time_step
    }
    # this can be used elsewhere
    has_data = sum(unlist(lapply(by_trial_heat_map_data, getElement, 'has_trials')))

    # how do we compare the trial onsets?

    #proposal: take the window around the first onset and do a linear model
    fastest <- which.min(line_plot_data %>% get_list_elements('on_time'))

    if(length(fastest)<1) {
        warning('nobody wins -- just compare whole window?')
        od_trial_data <- NULL
    } else {
        compare_window <- with(line_plot_data[[fastest]], on_time + c(0, window))
        od_trial_data <- lapply(seq_along(GROUPS), function(ii){
            if(has_trials[ii]) {
                power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$GROUP)
                data=collapse$over_frequency_and_time(power, trange = compare_window)
                mse <- .fast_mse(data)
                list(
                    'data' = data,
                    'mse' = mse,
                    'N' = length(data),
                    'name' = GROUPS[[ii]]$GROUP_NAME,
                    'has_trials' = has_trials[ii],
                    'range' = .fast_range(data)
                )
            } else {
                list('has_trials'=FALSE, 'N'=0)
            }
        })

        ll <- sapply(od_trial_data, '[[', 'N')
        if(length(ll) == 1) {
            attr(od_trial_data, 'stats') <- get_t(od_trial_data[[1]]$data)
        } else {
            group <- factor(rep(letters[seq_len(length(ll))], times=ll))
            group %<>% relevel(ref=letters[fastest])
            y <- unlist(lapply(od_trial_data, '[[', 'data'))
            attr(od_trial_data, 'stats') <- get_f(y ~ group, data=data.frame())
        }
    }

},{
    if(.is_async){
        async_msg = 'Running in the background'
    }
}, async = {
    logger('Async test')
    # rm(list = ls(all.names = T))
    Sys.sleep(0.15)
    nms = ls(all.names = T)
    async_msg = paste(search(), collapse = ', ')
}
)

niml_default = function(){
    return(1)
}

if(FALSE) {
#
#     module = ModuleEnvir$new('id', 'LABEL', './condition_explorer.R')
#     init_app(list(module))


    ls(rave:::.ui_update_repo)

    # h = rave:::RAVEHistory$new(path = '~/rave_modules', name = 'John', use_yaml = T)
    # h$save(
    #     memo = "I've done something"
    # )

     by_trial_heat_map()
     od_over_time_plot()
}


