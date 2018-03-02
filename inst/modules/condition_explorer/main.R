require(rave)
require(fields)
require(shiny)
require(magrittr)
require(stringr)
require(shiny)
rave_prepare(subject = 'YAB_Congruency_test',
             electrodes = 14,
             epoch = 'YAB',
             time_range = c(1, 2))

source('./adhoc/condition_explorer/plot_funcs.R')

rave_inputs(
  compoundInput(
    inputId = 'GROUPS',
    label = 'Group',
    components = {
      textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
      selectInput('GROUP', ' ', choices = '', multiple = TRUE)
    },
    inital_ncomp = 1
  ),

  sliderInput('FREQUENCY', 'Frequencies', min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
  sliderInput('BASELINE', 'Baseline Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
  sliderInput('TIME_RANGE', 'Analysis Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

  selectInput('electrode', 'Electrode', choices = '', multiple = F),
  numericInput('max_zlim', 'Maximum Plot Range', value = 0, min = 0, step = 1),

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

  do_poly <- function(x,y,col, alpha=50) {
    polygon(c(x,rev(x)), rep(y, each=2), col=getAlphaRGB(col, alpha), border=NA)
  }

  # make plot and color the baseline / analysis windows
  ylim <- pretty(range(sapply(line_plot_data, getElement, 'lim')))

  ns <- sapply(line_plot_data, getElement, 'N')

  `%&%` <- function(s1,s2) paste0(s1,s2)

  title <- 'Freq ' %&% paste0(round(FREQUENCY), collapse=':') %&% ' || Ns ' %&% paste0(ns, collapse=', ')

  plot.clean(time_points, ylim, xlab='Time (s)', ylab='% Signal Change', main=title)

  do_poly(BASELINE, range(ylim), col='gray80'); text(min(BASELINE), max(ylim), 'baseline', col='gray60', adj=c(0,1))
  do_poly(TIME_RANGE, range(ylim), col='salmon2'); text(min(TIME_RANGE), max(ylim), 'analysis window', col='salmon2', adj=c(0,1))
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

  axis(1, pretty(time_points))
  axis(2, ylim, las=1)
}

windowed_comparison_plot <- function(){
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

  barplot.scatter(scatter_bar_data)
}


by_trial_heat_map <- function() {
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

  # check if there is a plot range variable set, otherwise:
  actual_lim = sapply(by_trial_heat_map_data, getElement, 'range') %>% c %>% range %>% round
  if(!exists('max_zlim') | max_zlim==0) {
    max_zlim <- max(abs(actual_lim))
  }

  par(cex.lab=1.4, cex.axis=1.4, las=1)
  layout(matrix(1:(has_data+1), nrow=1), widths=c(rep(4, has_data), 1) )
  par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.4, cex.axis=1.4, las=1)

  get_ticks <- function(k) c(1, k, ceiling(k/2))

  lapply(by_trial_heat_map_data, function(comp){
    if(comp$has_t){
      with(comp, {
        dim(mean_o_freq)
        draw_img(t(mean_o_freq), x = time_points, y = 1:nrow(mean_o_freq), ylab='Trials',
          zlim = c(-max_zlim, max_zlim), main = name, DECORATOR = function(...){
            rave_axis(1, at=pretty(time_points), mgpx=c(3, 0.75, 0))
            rave_axis(2, at=get_ticks(nrow(mean_o_freq)))
            abline(v=BASELINE, lty=3, lwd=2, col='black')
          })
        box()
      })
    }
  })

  cbar <- matrix(seq(-max_zlim, max_zlim, length=length(crp))) %>% t
  par(mar=c(5.1, 5.1, 2, 2), cex.lab=1.6, cex.axis=1.6, las=1)
  image(cbar, col=crp, axes=F, ylab='Mean % Signal Change',main='')
  # title(main=list(paste0('[', paste0(actual_lim, collapse=':'), ']'), cex=rave_cex.main))
  rave_axis(2, at=0:2/2, labels = c(-max_zlim, 0, max_zlim), tcl=0.3); box();
}


heat_map_plot = function(){
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))

  par(cex.lab=1.4, cex.axis=1.4, las=1)
  layout(matrix(1:(has_data+1), nrow=1), widths=c(rep(4, has_data), 1) )
  par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.4, cex.axis=1.4, las=1)

  # check if there is a plot range variable set, otherwise:
  actual_lim = sapply(heat_map_data, getElement, 'range') %>% c %>% range %>% round
  if(!exists('max_zlim') | max_zlim==0) {
    max_zlim <- max(abs(actual_lim))
  }

  lapply(heat_map_data, function(comp){
    if(comp$has_t){
      with(comp, {
        draw_img(clip_x(mean_o_trials, c(-max_zlim, max_zlim)), x = time_points, y = frequencies,
          zlim = c(-max_zlim, max_zlim), main = name)
      })
    }
  })

  cbar <- matrix(seq(-max_zlim, max_zlim, length=length(crp))) %>% t

  par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.6, cex.axis=1.6, las=1)
  image(cbar, col=crp, axes=F, ylab='Mean % Signal Change',main='')
  hadj <- 1
  if(has_data) {
    main.cex <- 1.4
  } else {
    par(cex.lab=1.1, cex.axis=1.1)
    hadj <- 0.65
    main.cex <- 1.2

  }
  title(main=list(paste0('[', paste0(actual_lim, collapse=':'), ']'), font=1, cex=main.cex))
  axis(2, at = 0:4/4, labels=quantile(cbar, 0:4/4) %>% format(digits=1), las=1, tick = FALSE, hadj=hadj)
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
    val = .repository$fast_baseline(from, to, electrode)
  )

  lapply(GROUPS, function(comp){
    power = bl_power$subset(Trial = Trial %in% comp$GROUP)

    has_t = length(comp$GROUP) > 0
    mean_o_trials = mean_o_trial(power)
    range = range(mean_o_trials)
    list(
      name = comp$GROUP_NAME,
      # power = power,
      has_t = has_t,
      mean_o_trials = mean_o_trials,
      range = range
    )
  }) ->
    heat_map_data

  # this can be used elsewhere
  has_data = sum(unlist(lapply(heat_map_data, function(x){x$has_t})))

  lapply(GROUPS, function(comp){
    power = bl_power$subset(Trial = Trial %in% comp$GROUP)
    has_t = length(comp$GROUP) > 0
    mean_o_freq = collapse_over_freq(power)
    range = range(mean_o_freq)
    list(
      name = comp$GROUP_NAME,
      has_t = has_t,
      mean_o_freq = mean_o_freq,
      range = range
    )
  })-> by_trial_heat_map_data

  pm <- function(x,d)c(x-d,x+d)

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
    has_t <- length(comp$group)
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



rave_prepare()
