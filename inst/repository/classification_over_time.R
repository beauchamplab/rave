###################################################
# By default, you'll be provided with 7 variables:
# 
# SEVEN Ecog data: 
# "channels", "time", "frequencies", "trials", "content", "subject", "electrode"
# One hidden variable is your script name (`SCRIPT_NAME`), reserved, do not override it!
#
# To know how does these data look like, run:
# 
# # Code BEGIN, NOT RUN
# source('./scripts/env.R')
# source('./scripts/importer/load_ecog.R')
# source('./scripts/utils/cache.R')
# subject = 'subject_APX1604'
# electrode = 1
# 
# 
# data = get_or_create_cache(
#   func_name = 'load_ecog_raw', cache_name = 'electrode', # Default cache name: ..../default.RData
#   refresh = F, # if refresh is True, do not load cache, create or replace a new one
#   cache_root_dir = file.path(config$data_dir, subject, 'ecog', 'cache'),
#   path_dirs = 'electrode',
#   electrode = electrode,
#   subject = subject
# )
# attach(data)
# # END

# Load packages



###################################################
# If you want to make it shiny reactive, you can specify and wrap
# your options for control panel with certain variables
# Here is an example,
###################################################
SHINY_DESCRIPTION = shiny::HTML(
  '
  This script is designed to look find regions in time that distinguish amongst trial types <br />
  location - <strong>user/public/repository/classification_over_time.R</strong><br />
  To remove this module, go to <strong>./module.csv</strong>.
  <br>
  ')
# 
# You wanna ebable viewer to subset `controls` as reactive inputs
# The result is one plot
##### BLOCK STARTS
# SHINY_UPDATES = list()
# INPUT selection
SHINY_INPUT = 
  list(
    name_a = list(
      global_var = 'name_a',
      type = 'textInput',
      argList = list(
        placeholder = 'Name of Group A',
        label = 'Condition A Name',
        value = 'All Conditions'
      )
    ),
    control_subsetA = list(        # `control_subset` is lower case.
      global_var = 'group_a',          # Indicating whether it should be a global variable,
      # NULL is only local
      # c(...) means those variables will be affected
      # If not indicated, by default is NULL (local)
      type = 'selectInput',       # enter `?shiny::selectInput` for other args
      argList = list(
        choices = '',
        selected = NULL,
        label = 'Choose Group A Conditions',
        multiple = TRUE
      ),
      init = function(){
        all_trials = unique(trials[,1])
        list(
          choices = unique(trials[,1]),
          selected = get_global_var('group_a', unique(trials[,1]))
        )
      }
    ),
    name_b = list(
      global_var = 'name_b',
      type = 'textInput',
      argList = list(
        placeholder = 'Name of Group B',
        label = 'Condition B Name',
        value = ''
      )
    ),
    control_subsetB = list(        # `control_subset` is lower case.
      global_var = 'group_b',          # Indicating whether it should be a global variable,
      # NULL is only local
      # c(...) means those variables will be affected
      # If not indicated, by default is NULL (local)
      type = 'selectInput',       # enter `?shiny::selectInput` for other args
      argList = list(
        choices = '',
        selected = NULL,
        label = 'Choose Group B Conditions: ',
        multiple = TRUE
      ),
      init = function(){
        all_trials = unique(trials[,1])
        list(
          choices = unique(trials[,1]),
          selected = get_global_var('group_b', c())
        )
      }
    ), window_size = list(
      global_var = NULL,
      type = 'numericInput',
      argList = list(
        min=0.001,
        max=1000,
        value=.050,
        label = 'Window size (s): '
      ),
      init = function(){
        utils$logger(SHINY_UPDATES)
        list(
          value = get_local_var('window_size', 0.050)
        )
      }
    ),
    freq_subset = list(
      global_var = 'freq_subset',
      type = 'sliderInput',
      argList = list(
        min = 0,
        max = 250,
        value = c(50, 150),
        label = 'Frequencies: ',
        round = 1
      ),
      init = function(){
        rg = range(frequencies)
        list(
          min = floor(rg[1]),
          max = ceiling(rg[2]),
          value = get_global_var('freq_subset', c(0, 250))
        )
      }
    ),
    baseline_subset = list(
      global_var = 'baseline_subset',
      type = 'sliderInput',
      argList = list(
        min=-1,
        max=2,
        value=c(-1,0),
        step = 0.01,
        label = 'Baseline Range: '
      ),
      init = function(){
        list(
          value = get_global_var('baseline_subset', c(-1,0))
        )
      }
    )
  )



# The shiny app will now make `SHINY_INPUT` reactive, you'll get a variable `params` containing
# all the inputs as a named list. To run this file without shiny environment, you might want to specify `params` by your self
params = list(
  control_subsetA = c('Aclear_Vclear_rain', 'Aclear_Vclear_rock'),
  control_subsetB = c('Anoisy_Vclear_rain', 'Anoisy_Vclear_rock'),
  freq_subset = c(50,150),
  baseline_subset = c(-2, 0)
)
# shiny will ignore whatever is inside of `params` and use the user inputs to replace these parameters. Your parameter name should be the same as SHINY_INPUT (in this case, is `control_subset`)

# Specify what kind of data you might want to use
# Now, wrap your code inside of a function, this is the body function that shiny will execute once users trigger the `Run` button.
SHINY_EXECUTE = function(params){
  t_ <- Sys.time()
  
  if(length(dim(content)) == 4)
    content <- content[,,,1]
  
  require(magrittr)
  
  # if you have multiple files and want to source them,
  # use relative path to the root dir `RAFE`
  source('./user/public/repository/plot_helpers.R', local = TRUE)
  
  # if we have no conditions, bail
  if(length(params$control_subsetA) < 1 & length(params$control_subsetB) < 1) {
    return (list(heatmap_plot=default_plot))
  }
  
  return (list(heatmap_plot=default_plot))
  
  
  # as a courtesy, we're rounding the imported values of time and frequency
  # at some point we need to just specify the units for these things to make rounding safer
  frequencies %<>% round
  time %<>% round(3)
  
  time_sel <- time %>% is_within(params$time_subset)
  freq_sel <- frequencies %>% is_within(params$freq_subset)
  # if(not_null(params$control_subsetA)){
  # <- trials$stimuli %in% params$control_subsetA
  # }
  
  # do baseline correction on all the frequencies for each trial separately (high variance, low bias situation)
  # need an option to change the kind of baselining done
  baseline_sel <- time %>% is_within(params$baseline_subset)
  
  trial_selA <- trials$stimuli %in% params$control_subsetA
  trial_selB <- trials$stimuli %in% params$control_subsetB
  trial_sel = trial_selA | trial_selB
  
  # utils$logger(trial_sel %>% which)
  
  #  if(!is.null(params[['.IS_SUMA']])){
  # put batch-specific code here, only do the bare minimum
  # don't worry about duplication at this point
  
  # the result of this module is the t-test between the conditions
  
  # 1. baseline correct
  # Calculating baseline using matrix/vector operations larger chunk cuts execution time in half
  trial.ind <- which(trial_sel)
  
  baseline_ind <- which(baseline_sel)
  n_b <- length(baseline_ind)
  
  # we need to fix the off by one indexing that occurs when determining the mean using the cumulative array
  if(baseline_ind[1] == 1) {
    cbt_baseline <- repo$summary$cumsum_by_trial[trial.ind,,baseline_ind[n_b],1] / n_b
  } else {
    cbt_baseline <- (repo$summary$cumsum_by_trial[trial.ind,, baseline_ind[n_b],1] - repo$summary$cumsum_by_trial[trial.ind,, baseline_ind[1]-1,1]) / n_b
  }
  
  content_bc = 100 * vapply(1:length(time), function(ii) {
      (content[trial_sel,,ii] - cbt_baseline) / cbt_baseline
  },cbt_baseline)

  # both conditions are available
  # trialsA <- apply(content[trial_selA,freq_sel,time_sel], 1, mean)
  # trialsB <- apply(content[trial_selB,freq_sel,time_sel], 1, mean)
  
  
  #    return(list(
  #      SUMA_RESULT = suma_res
  #    ))
  #  }
  
  # utils$logger(paste('233: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))
  
  # Calculating baseline using matrix/vector operations larger chunk cuts execution time in half
  # content_bc <- content
  # for(ii in 1:nrow(content_bc)) {
  #   b <- rowMeans(content[ii,,baseline])
  #   content_bc[ii,,] = 100 * (content[ii,,] - b) / b
  # }
  
  # TODO: check if the "scale region" checkbox is selected
  # if (yes)  
  # freq = (frequencies <= params$freq_subset[2]) & (frequencies >= params$freq_subset[1])
  # if (no)
  #freq <- rep(TRUE, length(frequencies))
  # utils$logger(paste('247: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))
  
  # subset ecog data, using the baseline-corrected version
  data_from_selection <- function(stim) {
    content_bc[trials$stimuli %in% stim, , ]
  }
  
  # average over frequencies, then get mean +/- SE across trials
  
  freq_mse <- function(m) {
    t(sapply(1:dim(m)[3],function(ii) {
      m_se(rowMeans(m[,,ii]))
    }))
  }
  
  dataA <- NULL
  dataA_fmse <- NULL
  trialsA <- NULL
  n1 <- 0
  if(length(params$control_subsetA) > 0) {
    dataA <- data_from_selection(params$control_subsetA)
    n1 <- nrow(dataA)
    
    dataA_fmse <- freq_mse(dataA[,freq_sel,])  
    trialsA <- apply(dataA[,freq_sel,time_sel], 1, mean)
  }
  
  dataB <- NULL
  dataB_fmse <- NULL
  trialsB <- NULL
  n2 <- 0
  if(length(params$control_subsetB) > 0){
    dataB <- data_from_selection(params$control_subsetB)
    n2 <- nrow(dataB)
    
    dataB_fmse <- freq_mse(dataB[,freq_sel,])
    trialsB <- apply(dataB[,freq_sel,time_sel], 1, mean)
  }
  
  # utils$logger(paste('275: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))
  group_colors <- c('purple3', 'darkgreen')
  
  suma_res <- if(is.null(trialsB)){
    # B is null, so A must be good
    wilcox.test(trialsA, mu=0)$p.value
  } else if(is.null(trialsA)) {
    #A is null, but B is not
    wilcox.test(trialsB, mu=0)$p.value
  } else {
    #neither is null, so do two-sample test
    wilcox.test(trialsA, trialsB)$p.value
  }
  
  
  # utils$logger('P: ', suma_res)
  
  utils$logger(paste('308: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))
  
  return(list(
    # For base  images, it will evaluate immediately,  store them as a function
    heatmap_plot = function(){
      mbtA <- do_if(not_null(dataA), apply(dataA, 2, colMeans))
      mbtB <- do_if(not_null(dataB), apply(dataB, 2, colMeans))
      
      par(cex.lab=1.4, cex.axis=1.4, las=1)
      
      # do we have one or two heatmaps to display?
      showA <- not_null(dataA)
      showB <- not_null(dataB)
      
      if(showA & showB){
        layout(matrix(1:3, nrow=1), widths=c(4, 4,1))
      } else {
        layout(matrix(1:2, nrow=1), widths=c(6,1))
      }
      
      crp <- colorRampPalette(c('navy', 'steelblue3', 'orange', 'red'), interpolate='linear', space='Lab')(1001)
      
      draw_img <- function(mean_by_trial, yax=T, xax=T, zlim, main='', main.col='black', ...) {
        
        mean_by_trial %<>% clip_x(lim=zlim)
        
        image(x=time, y=frequencies,
          z=mean_by_trial, zlim=zlim,
          col=crp, xlab='Time (s)', ylab='Frequency (Hz)', cex.lab=1.7, axes=F, useRaster = TRUE, ...)
        
        title(main=list(main, col=main.col, cex=2))
        
        if(yax) axis(2, at=quantile(frequencies, 0:5/5) %>% round, las=1, tick=FALSE, cex.axis=1.5, hadj=0.65)
        if(xax) axis(1, tick = FALSE, cex.axis=1.5)
        
        xy <- cbind(params$time_subset, params$freq_subset)
        polygon(c(xy[,1], rev(xy[,1])) , rep(xy[,2], each=2), lty=2, lwd=3, border='white')
        
        #draw baseline region
        abline(v=params$baseline_subset, lty=3, lwd=2, col='white')
        # label baseline region
        text(params$baseline_subset %>% median, quantile(frequencies, .7), 'baseline', col='white', cex=1.5, pos=3)
        arrows(params$baseline_subset[1], quantile(frequencies, .7), params$baseline_subset[2], col='white', length=.1, code=3)
        # for(ii in c(1,3)) axis(ii, at=xy[,1], col='white', lwd=3, labels = FALSE)
        # for(ii in c(2,4)) axis(ii, at=xy[,2], col='white', lwd=3)
        
        invisible(mean_by_trial)
      }
      
      conditional_rbind <- function(a,b) {
        m <- a
        
        if(is.null(m))       { m <- b }
        else if(not_null(b)) { m %<>% rbind(b) }
        
        return (m)
      }
      
      combined_data <- conditional_rbind(mbtA, mbtB)
      
      # if(showA)
      #   mbtA <- mbtA %>% clip_x(lim=quantile(combined_data, params$ecog_value_subset))
      # if(showB)
      #   mbtB <- mbtB %>% clip_x(lim=quantile(combined_data, params$ecog_value_subset))
      
      # now get the newly recombined data
      combined_data <- conditional_rbind(mbtA, mbtB)
      
      #zlim <- combined_data %>% range
      if(is.na(params$ecog_value_subset)){
        zlim <- combined_data %>% range %>% abs %>% max %>% {c(-(.), (.))}
      } else {
        zlim <- abs(params$ecog_value_subset) %>% {c(-(.), (.))}
      }
      
      # set plot margins
      par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.4, cex.axis=1.4, las=1)
      if(showA) {
        draw_img(mbtA, main=params$name_a, zlim=zlim)
      }
      
      if(showA & showB)  {
        par(mar=c(5.1, 2.5, 2, 3), cex.lab=1.4, cex.axis=1.4, las=1)
        draw_img(mbtB, main=params$name_b, yax=FALSE, zlim=zlim)
      } else if(showB) {
        draw_img(mbtB, main=params$name_b, zlim=zlim)
      }
      
      # now draw the color bar, this should show the actual range of the data
      cbar <- matrix(seq(min(zlim), max(zlim), length=length(crp))) %>% t
      
      #utils$logger('cbar range: ', range(cbar))
      #utils$logger('cbar range: ', quantile(clipped_data, 0:5/5) %>% format(digits=1))
      par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.6, cex.axis=1.6, las=1)
      image(cbar, col=crp, axes=F, ylab='Mean % Signal Change',main='')
      hadj <- 1
      if((showA || showB) & (!(showA & showB))) {
        par(cex.lab=1.1, cex.axis=1.1)
        hadj <- 0.65
        main.cex <- 1.2
      } else {
        main.cex <- 1.4
      }
      title(main=list(paste0("RNG: ", paste0(round(range(combined_data)), collapse=', ')), font=1, cex=main.cex))
      axis(2, at = 0:4/4, labels=quantile(cbar, 0:4/4) %>% format(digits=1), las=1, tick = FALSE, hadj=hadj)
      
      #abline(h=0:2/2, col='white')
    },
    
    SUMA_RESULT = 1
    # blablabla = 'biubiuasdbiu'
  ))
}
# Now, define shiny output
SHINY_OUTPUT = list(
  
  # plot = list(`plotname` = parameters, ....) will render as shiny::plotoutput
  # I'll implement `verbatimTextOutput`, `dataTableOutput`, `plotlyOutput`, `plotOutput`
  `Basic Visualization` = list(
    heatmap_plot = list(
      type = 'plot',
      title = 'Trials sorted by peak Map (Mean over Frequency)',  # `?shinydashboard::box` for input params
      width = 12  # 1-12
    )
  )#, # plot, plotly, print, table
  # print = list(
  #   blablabla = list(
  #     title = 'This is an example'
  #   )
  # )
  
)


# for standalone script, usually for debug use, 

if(!exists('NOT_RUN')){
  source('./scripts/env.R') # Run this first if you haven't
  data_envir = utils$create_data_env()
  subject = 'subject_APX1604'
  electrode = 76
  repo = scripts$load_ecog(subject = subject, electrode = electrode) 
  data = repo$data
  summary = repo$summary
  data_envir$set_data_env(subject = subject, electrode = electrode, 
    data = data, summary = summary)
  module_env = data_envir$init_module_env(
    module_id = 'classification_over_time'
  )
  
  str(summary)
  
  attach(data)
  
  params <- module_env$params
  
  # Now, do whatever you want to do with the results
  
  results = module_env$SHINY_EXECUTE(module_env$params)
  
  
}
