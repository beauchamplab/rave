# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)


# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'Words/PAA',
             electrodes = 1:5,
             epoch = 'PAAaudonset',
             time_range = c(2, 4))

rave_prepare(subject = 'Complete/YAB',
             electrodes = 13:15,
             epoch = 'YABa',
             time_range = c(1, 2))

if(F){
    m = ModuleEnvir$new(module_id = 'id', label_name = 'CE',
                        script_path = './condition_explorer.R'); init_app(m)
# rave:::attachDefaultDataRepository()
}

source('../utils/rave_calculators.R')
source('condition_explorer_ui.R')
source('condition_explorer_plots.R')

rave_ignore({

    # rave_options(data_dir = '/Volumes/data/rave_data/data/',
    #              module_lookup_file = '~/Dropbox/RAVE_DEV/module_dev.csv',
    #              crayon_enabled=TRUE)

    GROUPS = list(
        list(
            GROUP_NAME = 'G1',
            GROUP = unique(epoch_data$Condition)#c('moth_D', 'moth_D_a')
         )#,
        # list(
        #     GROUP_NAME = 'G2',
        #     GROUP = c('moth_D')
        # ),
        # list(
        #     GROUP_NAME = 'G3',
        #     GROUP = c('moth_D_a')
        # ),
        # list(
        #     GROUP_NAME = '',
        #     GROUP=c()
        # )
    )

    electrode=electrodes[1]

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

main_function = function(electrode, use_cache = F){
  assertthat::assert_that(length(electrode) == 1,msg = 'No electrode selected')
  electrode = as.integer(electrode)

  #baseline all available trials
  GROUPS = lapply(GROUPS, function(g){ g$Trial_num = epoch_data$Trial[epoch_data$Condition %in% g$GROUP]; g })
  has_trials <- vapply(GROUPS, function(g) length(g$GROUP) > 0, TRUE)
  any_trials = any(has_trials)
  # tmp <- power$subset(Electrode = Electrode == electrode, drop = F)

  # if(use_cache){
  #   # We are in rave_execute
  #   bl_power <- cache(
  #     key = list(subject$id, electrode, BASELINE, any_trials, preload_info),
  #     #having problems with rutabaga
  #     val = module_tools$baseline(BASELINE[1],  BASELINE[2], electrode)
  #     # val = .local_baseline(tmp, BASELINE)
  #   )
  # }else{
  #   bl_power = module_tools$baseline(BASELINE[1],  BASELINE[2], electrode)
  # }
  bl_power = module_tools$baseline(BASELINE[1],  BASELINE[2], electrode)

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
    scatter_bar_data -> line_plot_data -> by_trial_heat_map_data -> heat_map_data -> voltage_data -> voltage_welch

  flat_data <- data.frame()

  # load up our collapsers into a list, this allows us to swap them out as needed
  collapse <- rave_collapsers.mean()

  # swap out the collapsers for medians
  if (exists('collapse_using_median')) {
    if(isTRUE(collapse_using_median)) {
      collapse <- rave_collapsers.median()
    }
  }

  # now we loop through only those groups with data
  # voltage.time_ind <- which(volt$dimnames$Time %within% TIME_RANGE)
  system.time( {
    for(ii in which(has_trials)) {
      # logger('ii')

      .power = bl_power$subset(Trial = Trial %in% GROUPS[[ii]]$Trial_num)

      #helper function to clean up the syntax below, value here should be a function
      # we're relying on power being defined above, so don't move this function out of this scope
      `add_data<-` <- function(x, value) {
        x[c('data', 'range', 'N', 'trials')] <-
          list(value, .fast_range(value), dim(.power)[1L], epoch_data$Condition)
        return(x)
      }
      heat_map_data[[ii]]$data %>% dim


      collapse$over_frequency(.power)

      add_data(heat_map_data[[ii]]) <- .power$collapse(keep = c(3,2), method = 'mean')
        #rutabaga::collapse(.power$data, c(3,2))/ dim(.power)[1] #collapse$over_trial(.power)
      add_data(by_trial_heat_map_data[[ii]]) <-
        rutabaga::collapse(.power$subset(Frequency=Frequency %within% FREQUENCY, data_only = TRUE, drop=TRUE),
                           c(3,1)) / dim(.power)[2]
      #collapse$over_frequency(.power)

      add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(.power)
      # we want to make a special range for the line plot data that takes into account mean +/- SE
      line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                           line_plot_data[[ii]]$data[,2]))

      add_data(scatter_bar_data[[ii]]) <- collapse$over_frequency_and_time(.power)

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
  }) #####

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

  return(list(
    bl_power = bl_power,
    has_data = has_data,
    result_for_suma = result_for_suma,
    heat_map_data = heat_map_data,
    line_plot_data = line_plot_data,
    scatter_bar_data = scatter_bar_data,
    by_trial_heat_map_data = by_trial_heat_map_data
  ))
}

rave_execute({
  if(FALSE) {
    BASELINE=c(-1.5, -0.5)
    electrode=1
    FREQUENCY=c(75,150)
    TIME_RANGE <- c(-.2,0.75)
  }

  re = main_function(electrode = electrode, use_cache = T)


  if(FALSE) {
    dim(power$data)
    dim(volt)

    range(volt$dimnames$Time) %>% diff %>% divide_by(dim(volt)[2L]) %>% raise_to_power(-1)

    vsub <- volt$subset(Trial = Trial %in% GROUPS[[1]]$Trial_num)

    plot.clean(volt$dimnames$Time, -150:150)
    abline(h=mean(vsub$data), col='lightgray')
    ebar_polygon(x=vsub$dimnames$Time,
                 colMeans(vsub$data[,,1]),
                 .fast_column_se(vsub$data[,,1]),
                 col=get_color(1))

    abline(v=TIME_RANGE)
    rave_axis(1,at=pretty(volt$dimnames$Time))

    .v <- volt$subset(Trial = Trial %in% GROUPS[[2]]$Trial_num)
    # lines(.v$dimnames$Time, .m, col='orange')
    ebar_polygon(.v$dimnames$Time, colMeans(.v$data[,,1]), .fast_column_se(.v$data[,,1]),
                 col='dodgerblue3')

    dim(volt$data)
    par(mfrow=1:2)
    .pw1 <- rave::pwelch(colMeans(volt$data[,1:1e3,1]), fs=1375, nfft = 256, noverlap = 8, window = 128, log='xy')
    .pw <- rave::pwelch(colMeans(volt$data[,1e3:2e3,1]), fs=1375, nfft = 256, noverlap = 8, window = 128, plot = F, log='xy')
    lines(log10(.pw$freq), 10*log10(.pw$spec), col='orange')
    # rutabaga::collapser
    ind <- .pw$freq %within% c(2,200)
    plot(.pw$freq[ind], (10 * (log10(.pw$spec)-log10(.pw1$spec)))[ind], type='l')

  }

  bl_power = re$bl_power
  has_data = re$has_data
  result_for_suma = re$result_for_suma
  heat_map_data = re$heat_map_data
  line_plot_data = re$line_plot_data
  scatter_bar_data = re$scatter_bar_data
  by_trial_heat_map_data = re$by_trial_heat_map_data

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

export_NIML = function(){
  progress = progress(title = 'Exporting to NIML', max = length(preload_info$electrodes))
  on.exit({progress$close()})

  lapply_async(preload_info$electrodes, function(e){
    re = main_function(electrode = e, use_cache = F)
    write(re$bl_power$dimnames[[4]], file = '~/Desktop/junk.txt', append = T)
  }, .call_back = function(i){
    progress$inc(message = sprintf('Calculating Electrode %d', preload_info$electrodes[i]))
  })
}

niml_default = function(){
    return(result_for_suma)
}

if(FALSE) {
    heat_map_plot()
    windowed_comparison_plot()
    by_trial_heat_map()
    over_time_plot()
}

