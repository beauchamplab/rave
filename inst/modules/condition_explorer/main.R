require(rave)
require(fields)
require(shiny)
require(magrittr)
require(stringr)
require(shiny)
rave_prepare(subject = 'YAB_congruency1',
             electrodes = 13:14,
             epoch = 'YAB',
             time_range = c(0.5, 2.5))

source('./adhoc/condition_explorer/plot_funcs.R')
# electrode=13;from=-1;to=0;self=dat
# d = self$fast_baseline(-100, 0, 13, print.time = T)

# dat = ECoGRepository$new('YAB_Congruency', autoload = F)
# dat$load_electrodes(51:53)
# dat$epoch('test', 0.5, 2.50)
# power = dat$power$power

# bl = dat$baseline(-1,0, print.time = T)
###########
rave_inputs(
  compoundInput(
    inputId = 'GROUPS',
    label = 'Group',
    value = {
      textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
      selectInput('GROUP', ' ', choices = '', multiple = TRUE)
    }, inital_ncomp = 1
  ),
  # textInput('GROUP_A_NAME', 'Group A Name', value = 'Group A', placeholder = 'Group A'),
  # selectInput('GROUP_A', 'Group A', choices = '', multiple = TRUE),
  # textInput('GROUP_B_NAME', 'Group B Name', placeholder = 'Group B'),
  # selectInput('GROUP_B', 'Group B', choices = '', multiple = TRUE),
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
  # 'Activity over time by trial (Mean over frequency)' = plotOutput('windowed_comparison_plot', width = 3),
  'Side Message' = textOutput('msg_out', width = 4),
  'Async Message' = textOutput('async_out', width = 4)
)

rave_updates(
  electrode = list(
    choices = electrodes,
    selected = electrodes[1]
  ),
  GROUPS = list(
    value = list(
      GROUP = list(
        choices = unique(trials),
        selected = unique(trials)
      )
    )
  ),
  # GROUP_A = list(
  #   choices = unique(trials),
  #   selected = trials[1]
  # ),
  # GROUP_B = list(
  #   choices = unique(trials),
  #   selected = NULL
  # ),
  BASELINE = local({
    list(
      min = min(time_points),
      max = max(time_points),
      value = c(min(time_points), 0)
    )
  }),
  FREQUENCY = local({
    list(
      min = min(round(frequencies)),
      max = max(round(frequencies)),
      value = range(round(frequencies))
    )
  }),
  TIME_RANGE = local({
    list(
      min = min(time_points),
      max = max(time_points),
      value = c(0, max(time_points))
    )
  })
)

rave_ignore({

  GROUPS = list(
    list(
      GROUP_NAME = 'G1',
      GROUP = unique(trials)[-c(1:3)]
    )#,
    # list(
    #   GROUP_NAME = 'G2',
    #   GROUP = unique(trials)[1:3]
    # )
  )

})


`%within%` <- function(a,b) {
  is_within(a,b)
}

mean_o_trial <- function(el){
  apply(el$data[,,,1], 2,colMeans)
}

collapse_over_freq_and_time <- function(el) {
  if(prod(dim(el))<1) return(NULL)

  (el$subset(Frequency = (Frequency %within% FREQUENCY), Time = (Time %within% TIME_RANGE))) %>%
    content %>% apply(1, mean)
}

collapse_over_freq_and_trial <- function(el) {
  if(prod(dim(el))<1) return(matrix(NA, ncol=2, nrow=1))

  (el$subset(Frequency = (Frequency %within% FREQUENCY))) %>%
    content %>% apply(3, rowMeans) %>%
    apply(2, function(x) c(mean(x), sd(x)/length(x))) %>% t
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


heat_map_plot = function(){
  validate(need((exists('has_data') && (has_data)), "No Condition Specified"))


  par(cex.lab=1.4, cex.axis=1.4, las=1)
  layout(matrix(1:(has_data+1), nrow=1), widths=c(rep(4, has_data), 1) )
  par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.4, cex.axis=1.4, las=1)

  # check if their is a plot range variable set, otherwise:
  actual_lim = sapply(heat_map_data, getElement, 'range') %>% c %>% range %>% round
  if(!exists('max_zlim') | max_zlim==0) {
    max_zlim <- max(abs(actual_lim))
  }

  lapply(heat_map_data, function(comp){
    if(comp$has_t){
      with(comp, {
        draw_img(clip_x(mean_o_trials, c(-max_zlim, max_zlim)), time = time_points, frequencies = frequencies,
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
  # bl_power <- .repository$fast_baseline(from, to, electrode)

  lapply(GROUPS, function(comp){
    power = bl_power$subset(Trial = Trial %in% comp$GROUP)
    has_t = length(comp$GROUP) > 0
    mean_o_trials = mean_o_trial(power)
    range = range(mean_o_trials)
    list(
      name = comp$GROUP_NAME,
      power = power,
      has_t = has_t,
      mean_o_trials = mean_o_trials,
      range = range
    )
  }) ->
    heat_map_data

  has_data = sum(unlist(lapply(heat_map_data, function(x){x$has_t})))

  pm <- function(x,d)c(x-d,x+d)

  # pre-process for over time plot
  lapply(GROUPS, function(comp){
    has_t = length(comp$GROUP) > 0
    pow <- bl_power$subset(Trial = Trial %in% comp$GROUP)
    mse_o_trials = collapse_over_freq_and_trial(pow)
    lim = range(pm(mse_o_trials[,1], mse_o_trials[,2]))
    N = dim(pow)[1]
    list(
      name = comp$GROUP_NAME,
      mse = mse_o_trials,
      has_t = has_t,
      lim = lim,
      N=N
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
  Sys.sleep(5)
  aaa = heat_map_plot
  bbb = `%>%`
  nms = ls(all.names = T)
  async_msg = paste(search(), collapse = ', ')
}
)

if(FALSE) {
  heat_map_plot()
}



rave_prepare()
