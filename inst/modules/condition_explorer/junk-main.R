require(rave)
require(fields)
require(shiny)
require(magrittr)
require(stringr)
require(shiny)
rave_prepare(subject = 'YAB_congruency1',
             electrodes = 14,
             epoch = 'YAB',
             time_range = c(1, 2))

source('plot_funcs.R')

rave_inputs(
  compoundInput(
    inputId = 'GROUPS',
    label = 'Group',
    components = {
      textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
      selectInput('GROUP', ' ', choices = '', multiple = TRUE)
    }, inital_ncomp = 1
  ),

  sliderInput('FREQUENCY', 'Frequencies', min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
  sliderInput('BASELINE', 'Baseline Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
  sliderInput('TIME_RANGE', 'Analysis Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

  selectInput('electrode', 'Electrode', choices = '', multiple = F),
  numericInput('max_zlim', 'Maximum Plot Range', value = 0, min = 0, step = 1L),

  .tabsets = list(
    'Global Variables' = c(
      'GROUPS', 'FREQUENCY', 'BASELINE', 'TIME_RANGE'
    )
  )
)

rave_outputs(
  'Heat Map (Mean over Trials)' = plotOutput('heat_map_plot', brush = 'brushed', width = 12),
  'Activity over time (Mean over freq and trial)' = plotOutput('over_time_plot', width = 8),
  'Windowed Comparison (Mean over time and freq)' = plotOutput('windowed_comparison_plot', width = 4),
  'Activity over time per trial (Mean over frequency)' = plotOutput('by_trial_heat_map', width = 12),
  'Side Message' = textOutput('msg_out', width = 4),
  'Async Message' = textOutput('async_out', width = 4)
)

rave_updates(
  electrode = list(
    choices = electrodes,
    selected = electrodes[1]
  ),
  GROUPS = list(
    initialize = list(
      GROUP = list(
        choices = unique(trials)
      )
    ),
    value = cache_input('GROUPS', list(
      list(
        GROUP = trials[1]
      )
    ))
  ),

  BASELINE = local({
    list(
      min = min(time_points),
      max = max(time_points),
      value = cache_input('BASELINE', c(min(time_points), 0))
    )
  }),
  FREQUENCY = local({
    list(
      min = min(round(frequencies)),
      max = max(round(frequencies)),
      value = cache_input('FREQUENCY', range(round(frequencies)))
    )
  }),
  TIME_RANGE = local({
    list(
      min = min(time_points),
      max = max(time_points),
      value = cache_input('TIME_RANGE', c(0, max(time_points)))
    )
  })
)


mean_o_trial <- function(el){
  apply(el$data[,,,1], 2, colMeans)
}

collapse_over_freq_and_time <- function(el, FUN=mean) {
  if(prod(dim(el))<1) return(NULL)

  (el$subset(Frequency = (Frequency %within% FREQUENCY), Time = (Time %within% TIME_RANGE))) %>%
    content %>% apply(1, FUN)
}

collapse_over_freq <- function(el) {
  if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

  (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    content %>%
    apply(1, colMeans) %>%
    t
}

collapse_over_freq_and_trial <- function(el) {
  if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

  (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    content %>%
    apply(3, rowMeans) %>%
    apply(2, m_se) %>%
    t
}

over_time_plot <- function() {
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

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

windowed_comparison_plot <- function(){
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

  barplot.scatter(scatter_bar_data)
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
  #env$msg
}

async_out = function(){
  validate(need(exists('async_msg', envir = environment()), 'Press "Force run" Button.'))
  async_msg
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

  lapply(GROUPS, function(comp){
    power = bl_power$subset(Trial = Trial %in% comp$GROUP)

    has_t = length(comp$GROUP) > 0
    mean_o_trials = mean_o_trial(power)
    list(
      name = comp$GROUP_NAME,
      has_t = has_t,
      data = mean_o_trials,
      range = range(mean_o_trials)
    )
  }) ->
    heat_map_data

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

  lapply(GROUPS, function(comp) {
    has_t <- length(comp$GROUP)
    trials <- collapse_over_freq_and_time(bl_power$subset(Trial = Trial %in% comp$GROUP))
    lim = range(trials)
    list(
      name = comp$GROUP_NAME,
      trials = trials,
      N = length(trials),
      has_t = has_t,
      lim = lim
    )
  }) ->
    scatter_bar_data
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

if(FALSE) {
  heat_map_plot()

  windowed_comparison_plot()

}

niml_default = function(){
  return(1:3)
}

rave_prepare()
