# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'congruency/YAB',
             electrodes = 23,
             epoch = 'YABaOutlier',reference = 'default',
             time_range = c(2, 2))

if(FALSE){
    m = ModuleEnvir$new(module_id = 'id', label_name = 'CE',
                        script_path = './condition_explorer.R'); init_app(m)
 rave:::attachDefaultDataRepository()
    # debug_module(m)
#
#  load_modules() %>% unlist %>% lapply(function(x) {
#
#  })

    # m = unlist(load_modules())[[4]]
    # init_app(m)
    # debug_module(m)
}

source('rave_calculators.R')
source('condition_explorer_ui.R')
source('condition_explorer_plots.R')
source('3d_viewer.R')


get_by <- function(x, FUN, ...) {
    x[FUN(x, ...)]
}

rave_ignore({

    # rave_options(data_dir = '/Volumes/data/rave_data/data/',
    #              module_lookup_file = '~/Dropbox/RAVE_DEV/module_dev.csv',
    #              crayon_enabled=TRUE)

    GROUPS = list(
        list(
            GROUP_NAME = 'Aonly',
            GROUP = epoch_data$Condition %>% get_by(endsWith, '_a') %>% unique
         ),
        list(
            GROUP_NAME = 'AV',
            GROUP = epoch_data$Condition %>% get_by(function(x) x!='X' & !endsWith(x, '_a')) %>% unique
        )#,
        # list(
        #     GROUP_NAME = 'G3',
        #     GROUP = c('moth_D_a')
        # ),
        # list(
        #     GROUP_NAME = '',
        #     GROUP=c()
        # )
    )

    # electrode=electrodes[2]

    max_zlim=500
    log_scale=FALSE
    sort_trials_by_type=TRUE
    collapse_using_median=FALSE
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

    # here we need to pass in the decorator because dmhm is going to loop over the heatmaps
    # and take care of drawing a color bar for us
    draw_many_heat_maps(heat_map_data, allow_log_scale = TRUE)
}

voltage_over_time_plot = function(){
    validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

    # here we need to pass in the decorator because dmhm is going to loop over the heatmaps
    # and take care of drawing a color bar for us
    # draw_many_heat_maps(heat_map_data, allow_log_scale = TRUE)
}


msg_out = function() {
    # put analysis information in here
# msg
    # return(sprintf('length: %d', length(heat_map_data)))
}

async_out = function(){
    validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
    async_msg
}

# do_execute <- function(.electrode) {
#     re <- cache(
#         key = list(.electrode, subject$id, preload_info, BASELINE, FREQUENCY),
#         value = {
#             #
#         }
#     )
#
#     return(re)
# }


get_summary <- function() {
    # here we just want an estimate of the power at each trial for each electrode
    # get the labels for each trial


    ..g_index <- 1
    GROUPS = lapply(GROUPS, function(g){
        g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]

        if(g$GROUP_NAME == '') {
            g$GROUP_NAME <-  LETTERS[..g_index]
            ..g_index <<- ..g_index + 1
        }

        return(g)
    })
    rm(..g_index)


    tnum_by_condition <- sapply(GROUPS, function(g) {
        list(g$Trial_num)
    }) %>% set_names(sapply(GROUPS, '[[', 'GROUP_NAME'))

    all_trials <- unlist(tnum_by_condition)
    .bl_power <- cache(
        key = list(subject$id, preload_info$electrodes, BASELINE, preload_info),
        val = baseline(power, BASELINE[1],  BASELINE[2], hybrid = FALSE, mem_optimize = FALSE)
        #having problems with rutabaga?
        # val = .local_baseline(tmp, BASELINE)
    )

    # subset out the trials, frequencies, and time rane
    .power <- .bl_power$subset(Frequency = Frequency %within% FREQUENCY,
                               Time = Time %within% TIME_RANGE,
                               Trial = Trial %in% all_trials, data_only = FALSE)

    stimulus <- epoch_data$Condition[as.numeric(.power$dimnames$Trial)]

    condition <- .power$dimnames$Trial %>% as.numeric %>% sapply(function(tnum) {
        #ensure only one group is ever selected? or we could throw an error on length > 1
        sapply(tnum_by_condition, `%in%`, x=tnum) %>% which %>% extract(1)
    }) %>% names

    # rutabaga over Freq and Time
    # by_elec <- rutabaga::collapse(.power$data, keep=c(1,4)) / prod(.power$dim[2:3])
    by_elec <- .power$collapse(keep = c(1,4), method = 'mean')

    data.frame('subject_id' = subject$subject_code,
                     'elec' = rep(preload_info$electrodes, each=length(condition)),
                     'trial' = rep(seq_along(condition), times=length(preload_info$electrodes)),
                     'condition' = rep(condition, length(preload_info$electrodes)),
                     'power' = c(by_elec)
    )
}

rave_execute({
    if(FALSE) {
        BASELINE=c(-1.5, -0.5)
        electrode=1
        FREQUENCY=c(75, 150)
        TIME_RANGE <- c(-.2,0.75)
    }

    requested_electrodes = parse_selections(electrode_text %>% str_replace_all(':', '-'))
    requested_electrodes %<>% get_by(`%in%`, electrodes)

    # this will be NA if the only requested electrodes are not available
    # electrode <- requested_electrodes[1]
    assertthat::assert_that(length(requested_electrodes) >= 1 && all(not_NA(requested_electrodes)),msg = 'No electrode selected')

    #baseline all available trials
    GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]; g })
    has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
    any_trials = any(has_trials)

    bl_power <- cache(
        key = list(subject$id, requested_electrodes, BASELINE, any_trials, preload_info),
        val = baseline(power$subset(Electrode = Electrode %in% requested_electrodes),
                       BASELINE[1],  BASELINE[2], hybrid = FALSE, mem_optimize = FALSE)# module_tools$baseline(BASELINE[1],  BASELINE[2], requested_electrodes)
        #having problems with rutabaga?
        # val = .local_baseline(tmp, BASELINE)
    )

    if(FALSE) {
        dim(power$data)
        dim(volt)

        vsub <- volt$subset(Trial = Trial %in% GROUPS[[1]]$Trial_num)
        tind <- vsub$dimnames$Time[round(seq(1, length(vsub$dimnames$Time),
                                       length.out = length(power$dimnames$Time)))] %>% round(4)

        # index and take the same number of time points that we have for power?
        g1 <- vsub$subset(Time = round(Time,4) %in% tind)
        # .r <- range(volt$dimnames$Time) %>% diff %>% divide_by(dim(volt)[2L]) %>% raise_to_power(-1)

        plot.clean(volt$dimnames$Time, c(-1,1)*100)
        abline(h=mean(g1$data), col='lightgray')
        ebar_polygon(x=g1$dimnames$Time,
                     colMeans(g1$data[,,1]),
                     .fast_column_se(g1$data[,,1]),
                     col=get_color(1))
        abline(v=TIME_RANGE)
        rave_axis(1,at=pretty(volt$dimnames$Time))

        .v <- volt$subset(Trial = Trial %in% GROUPS[[2]]$Trial_num, Time = round(Time,4) %in% tind)
        # lines(.v$dimnames$Time, .m, col='orange')
        ebar_polygon(.v$dimnames$Time, colMeans(.v$data[,,1]), .fast_column_se(.v$data[,,1]),
                     col='dodgerblue3')

        dim(volt$data)
        par(mfrow=1:2)
        .pw1 <- rave::pwelch(colMeans(volt$data[,1:1e3,1]), fs=1375,
                             nfft = 256, noverlap = 8, window = 128, log='xy')
        .pw <- rave::pwelch(colMeans(volt$data[,1e3:2e3,1]), fs=1375,
                            nfft = 256, noverlap = 8, window = 128, plot = F, log='xy')
        lines(log10(.pw$freq), 10*log10(.pw$spec), col='orange')
        ind <- .pw$freq %within% c(2,200)
        plot(.pw$freq[ind], (10 * (log10(.pw$spec)-log10(.pw1$spec)))[ind], type='l')
    }

    # we were repeating a lot of calculations and looping over GROUPS too many times
    # let's clean that up

    #helper file to build lists with common elements pre-populated
    build_list <- function() {
        ##NB: this is faster than using replicate(length(has_trials))
        lapply(seq_len(length(has_trials)), function(ii)
            list('has_trials' = has_trials[ii],
                 'name' = GROUPS[[ii]]$GROUP_NAME))
    }

    # declare all our variables with pre-populated 'has_trials' and 'name' variables
    build_list() ->
        scatter_bar_data -> line_plot_data ->
        by_trial_heat_map_data -> heat_map_data ->
        voltage_data -> voltage_welch

    flat_data <- data.frame()

    # load up our collapsers into a list, this allows us to swap them out as needed
    collapse <- rave_collapsers.mean()

    # swap out the collapsers for medians
    if (exists('collapse_using_median')) {
        if(isTRUE(collapse_using_median)) {
            collapse <- rave_collapsers.median()
        }
    }

    # how should we collapse across electrodes?
    .transform <- electrode_transform(combine_method)
    #relies on transform as defined above
    do_row_transform <- function(.tens) {
        vapply(seq_len(dim(.tens)[3]), function(ei) {
            t(apply(.tens[,,ei], 1, .transform))
        }, FUN.VALUE = .tens[,,1])
    }

    # now we loop through only those groups with data
    # voltage.time_ind <- which(volt$dimnames$Time %within% TIME_RANGE)
    # system.time( {
      for(ii in which(has_trials)) {

        .power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num)

        #helper function to clean up the syntax below, value here should be a function
        # we're relying on power being defined above, so don't move this function out of this scope
        `add_data<-` <- function(x, value) {
            x[c('data', 'range', 'N', 'trials')] <-
                list(value, .fast_range(value), dim(.power)[1L], epoch_data$Condition)
            return(x)
        }

        if(identical(.transform, IDENTITY_TRANSFORM)) {

            # first build the heat map data
            add_data(heat_map_data[[ii]]) <- .power$collapse(keep = c(3,2), method = 'mean')
              # rutabaga::collapse(.power$data, keep=3:2)/ prod(dim(.power)[c(1,4)])

            # by trial data. Set drop to FALSE b/c we want to keep the electrode dim even if #e ==1
            bt_dat <- .power$subset(Frequency=Frequency %within% FREQUENCY, data_only = FALSE, drop=FALSE)
            add_data(by_trial_heat_map_data[[ii]]) <- bt_dat$collapse(keep = c(3,1), method = 'mean')
              # rutabaga::collapse(bt_dat, c(3,1)) / prod(dim(bt_dat)[c(2,4)])

            # coll freq and trial for line plot w/ ebar. Because we're doing error bars, we have to know whether we have 1 vs. >1 electrodes
            if(dim(.power)[4] == 1) {
                add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(.power)
            } else {
                # here we want the error bars to be across electrodes, rather than across trials
                #NB: take advantage of bt_dat having already subset'd FREQ
                oft <- bt_dat$collapse(keep = c(3,4), method = 'mean')
                # rutabaga::collapse(bt_dat, keep=3:4) / prod(dim(bt_dat)[1:2])
                add_data(line_plot_data[[ii]]) <- apply(oft, 1, .fast_mse) %>% t
                                                # cbind(.rowMeans(oft, nrow(oft), ncol(oft)), .fast_column_se(t(oft)))
            }

            # we want to make a special range for the line plot data that takes into account mean +/- SE
            line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                                 line_plot_data[[ii]]$data[,2]))

            # scatter bar data
                add_data(scatter_bar_data[[ii]]) <- collapse$over_frequency_and_time(.power, TIME_RANGE, FREQUENCY)

                # for the scatter_bar_data we also need to get m_se within condition
                scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)

        } else {
            # we need to transform the data before combining

            # collapse over trial then row transform
                hmd <- do_row_transform(
                    .power$collapse(keep = 2:4, method = 'mean')
                    # rutabaga::collapse(.power$data, keep=2:4) / dim(.power)[1]
                )
                add_data(heat_map_data[[ii]]) <- rutabaga::collapse(hmd, keep=2:1) / dim(hmd)[3]

            #collapse over frequency then row transform
                bt_dat <- .power$subset(Frequency=Frequency %within% FREQUENCY, data_only = FALSE, drop=FALSE)
                bthmd <- do_row_transform(
                    bt_dat$collapse(keep = c(1,3,4), method = 'mean')
                    # rutabaga::collapse(bt_dat, c(1,3:4)) / dim(bt_dat)[2]
                )
                add_data(by_trial_heat_map_data[[ii]]) <-  rutabaga::collapse(bthmd, keep=2:1) / dim(bthmd)[3]

            # collapse over frequency and trial. here we have to consider #elec b/c of the error bars
                if(dim(.power)[4] == 1) {
                    add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(.power)
                } else {
                    # here we want the error bars to be across electrodes, rather than across trials
                    oft <- apply(
                      # rutabaga::collapse(bt_dat, keep=3:4) / prod(dim(bt_dat)[1:2]), #????
                      bt_dat$collapse(keep = c(3,4), method = 'mean'),
                      2, .transform)
                    add_data(line_plot_data[[ii]]) <- apply(oft, 1, .fast_mse) %>% t
                }

                # we want to make a special range for the line plot data that takes into account mean +/- SE
                line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                                     line_plot_data[[ii]]$data[,2]))

            # collapse over freq and time so we get response per trial for scatter bar data.
                # use the bthmd that is already frequency selected and transformed per trial (across time)

                # now we want the summary across time \in TIME_RANGE and electrode. one mean per trial
                ind.t <- .power$dimnames$Time %within% TIME_RANGE
                add_data(scatter_bar_data[[ii]]) <- rowMeans(bthmd[,ind.t,])

                # for the scatter_bar_data we also need to get m_se within condition
                scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)
        }

        # for the scatter_bar_data we also need to get m_se within condition
        scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)

        flat_data %<>% rbind(data.frame('group'=ii, 'y' = scatter_bar_data[[ii]]$data))

        ## Voltage data
        # vsub <- volt$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num, data_only = TRUE, drop = TRUE)
        # add_data(voltage_data[[ii]]) <-  t(vapply(seq_len(dim(vsub)[2L]),function(i) .fast_mse(vsub[,i]), rep(0,2)))

        # add_data(voltage_data[[ii]]) <-  t(vapply(seq_len(dim(vsub)[2L]),function(i) .fast_mse(vsub[,i]), rep(0,2)))
        # for the welch periodogram, grab the power only within the analysis range
        # voltage_welch[[ii]]$welch <- pwelch(vsub[,voltage.time_ind], module_tools$get_sample_rate(original = TRUE), plot=FALSE)
      }
    # }) #####

    # add a baseline welch

    # voltage_baseline <- vapply(volt$data[,baseline.ind,1]

    # for baseline you want to have only the baseline times
    flat_data$group %<>% factor


    # this can be used elsewhere
    has_data = sum(has_trials)

    # calculate some statistics

    # calculate the statistics here so that we can add them to the niml_out
    # if there are > 1 groups in the data, then do linear model, otherwise one-sample t-test
    if(length(unique(flat_data$group)) > 1) {
        # we need to check if they have supplied all identical data sets
        # easy way is to check that the trials are the same?
        g1 <- GROUPS[[which(has_trials)[1]]]$GROUP
        if(all(sapply(which(has_trials)[-1],
                      function(ii) identical(GROUPS[ii]$GROUP, g1)))) {
            result_for_suma <-
                get_t(flat_data$y[flat_data$group==flat_data$group[1]])
        } else {
            result_for_suma <- get_f(y ~ group, flat_data)
        }
    } else {
        result_for_suma <- flat_data$y %>% get_t
    }

    attr(scatter_bar_data, 'stats') <- result_for_suma

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
},
#####
auto = TRUE
)

export_stats = function(conn=NA, lbl='stat_out', dir, ...){
    out_dir <- dir #module_tools$get_subject_dirs()$module_data_dir %&% '/condition_explorer/'

    if(!dir.exists(out_dir))    {
        dir.create(out_dir, recursive = TRUE)
    }

    if(is.na(conn)) {
        fout <- out_dir %&% lbl %&% '.RDS'
    } else {
        fout <- conn #out_dir %&% conn
    }


    # run through all the active electrodes and get the data
    # out_data <- lapply_async(electrodes, process_for_stats)

    out_data <- get_summary()

    saveRDS(out_data, file = fout)
    return(data.frame('unsure' = 'junk'))
}

if(FALSE) {
    heat_map_plot()
    windowed_comparison_plot()
    by_trial_heat_map()
    over_time_plot()
}

