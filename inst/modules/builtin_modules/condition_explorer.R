# rm(list=ls()); gc()
require(rave)
require(magrittr)
require(stringr)
require(shiny)

# give us some defaults to play with while we're working on the module code
rave_prepare(subject = 'congruency1_sliding/YAB',
             electrodes = 38,
             epoch = 'YABa',
             time_range = c(1, 2))

source('rave_calculators.R')
source('condition_explorer_ui.R')
source('condition_explorer_plots.R')


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

msg_out = function() {

    # put analysis information in here

    return(sprintf('length: %d', length(heat_map_data)))
}

async_out = function(){
    async_msg
}



condition_explorer_main = function(){
  # TODO: change adhoc vars definition - Zhengjia
  power = module_tools$get_power(force = T)
  electrodes = power$dimnames$Electrode
  trials = power$dimnames$Trial
  frequencies = power$dimnames$Frequency
  time_points = power$dimnames$Time


  electrode = as.integer(electrode)

  #baseline all available trials

  has_trials <- vapply(GROUPS_CMPD, function(g) length(g$GROUP) > 0, TRUE)
  any_trials = any(has_trials)

  bl_power <- cache(
    key = list(subject$subject_id, electrode, BASELINE, any_trials),
    val = module_tools$baseline(BASELINE[1],  BASELINE[2], electrode)
  )

  # TODO: change GROUPS definition - Zhengjia
  trials_ = module_tools$get_meta('trials')
  GROUPS = lapply(GROUPS_CMPD, function(g){
    g[['GROUP_NAME']] %?<-% ''
    cond = g[['GROUP']]
    tls = trials_$Trial[trials_$Condition %in% cond]
    if(length(tls)){
      g[['GROUP']] = tls
    }else{
      g[['GROUP']] = NULL
    }
    g
  })

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

    add_data(heat_map_data[[ii]]) <- collapse$over_trial(power)
    add_data(by_trial_heat_map_data[[ii]]) <- collapse$over_frequency(power)

    add_data(line_plot_data[[ii]]) <- collapse$over_frequency_and_trial(power)
    # we want to make a special range for the line plot data that takes into account mean +/- SE
    line_plot_data[[ii]]$range <- .fast_range(plus_minus(line_plot_data[[ii]]$data[,1],
                                                         line_plot_data[[ii]]$data[,2]))

    add_data(scatter_bar_data[[ii]]) <- collapse$over_frequency_and_time(power)

    # for the scatter_bar_data we also need to get m_se within condition
    scatter_bar_data[[ii]]$mse <- .fast_mse(scatter_bar_data[[ii]]$data)

    flat_data %<>% rbind(data.frame('group'=ii, 'y' = scatter_bar_data[[ii]]$data))
  }
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
}


rave_execute({
  assertthat::assert_that(length(electrode) == 1, msg = 'No electrode selected')

  eval_dirty(body(condition_explorer_main), env = environment())


},{
  async_msg = async_var(async_msg, 'Press "Async run" Button.')
}, async = {
    nms = ls(all.names = T)
    async_msg = paste(search(), collapse = ', ') %&% '   ' %&% Sys.getpid()
}
)

export_NIML = function(){
  export_report({
    return(result_for_suma)
  }, inputId = 'electrode', electrodes = subject$valid_electrodes) ->
    dat

  tbl = t(sapply(rave:::dropNulls(dat), I))
  colnames(tbl) = c('mean', 't', 'p')
  rave:::write.niml(
    tbl,
    electrode_numbers = 1:10,
    value_labels = subject$valid_electrodes,
    prefix = 'CE',
    add_electrodes_as_column = TRUE,
    value_file = '__vals.dat',
    index_file = '__ind.dat',
    work_dir = module_tools$get_subject_dirs()$suma_out_dir) ->
    cmd
  return('NIML File Generated! Pease launch SUMA.')
}

if(FALSE) {
    heat_map_plot()
    windowed_comparison_plot()
    by_trial_heat_map()
    over_time_plot()
}

