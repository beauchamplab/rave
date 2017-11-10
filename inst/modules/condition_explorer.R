
SHINY_DESCRIPTION = shiny::HTML(
  '
  This is a demo script<br />
  location - <strong>scripts/summarise/example_1.R</strong><br />
  To remove this module, go to <strong>./module.csv</strong> and remove the second row.
  <br>
  ')
#
# You want to enable viewer to subset `controls` as reactive inputs
# The result is one plot
##### BLOCK STARTS
# SHINY_UPDATES = list()
# INPUT selection

SHINY_INPUT = list(
  rave:::TextInput$new(
    inputId = 'control_nameA',
    global_var = 'name_a',
    placeholder = 'Name of Group A',
    label = 'Condition A Name',
    value = 'All Conditions',
    init = function(){
      list(
        value = get_global_var('name_a', 'All')
      )
    }
  ),
  rave:::SelectInput$new(
    inputId = 'control_subsetA',
    global_var = 'group_a',
    choices = '',
    label = 'Choose Group A Conditions',
    multiple = TRUE,
    init = function(){
      all_trials = unique(paste0(data_env$subject$trials$Stimulus, '_', data_env$subject$trials$Type))
      list(
        choices = all_trials,
        selected = get_global_var('group_a', all_trials)
      )
    }
  ),
  rave:::TextInput$new(
    inputId = 'control_nameB',
    global_var = 'name_b',
    placeholder = 'Name of Group B',
    label = 'Condition B Name',
    value = '',
    init = function(){
      list(
        value = get_global_var('name_b', '')
      )
    }
  ),
  rave:::SelectInput$new(
    inputId = 'control_subsetB',
    global_var = 'group_b',
    choices = '',
    label = 'Choose Group B Conditions',
    multiple = TRUE,
    init = function(){
      all_trials = unique(paste0(data_env$subject$trials$Stimulus, '_', data_env$subject$trials$Type))
      list(
        choices = all_trials,
        selected = get_global_var('group_b', c())
      )
    }
  ),
  rave:::SliderInput$new(
    inputId = 'freq_subset',
    global_var = 'freq_subset',
    min = 0,
    max = 250,
    value = c(50, 150),
    label = 'Frequencies: ',
    round = 1,
    init = function(){
      rg = range(data_env$subject$frequencies)
      rg[1] = floor(rg[1])
      rg[2] = ceiling(rg[2])
      list(
        min = rg[1],
        max = rg[2],
        value = get_global_var('freq_subset', rg)
      )
    }
  ),
  rave:::SliderInput$new(
    inputId = 'baseline_subset',
    global_var = 'baseline_subset',
    min=-1,
    max=2,
    value=c(-1,0),
    step = 0.01,
    label = 'Baseline Range: ',
    init = function(){
	  time = data_env$subject$time_points
      list(
	    min = min(time),
		max = max(time),
        value = get_global_var('baseline_subset', c(-1,0))
      )
    }
  ),
  rave:::SliderInput$new(
    inputId = 'time_subset',
    global_var = 'time_subset',
    min=-1,
    max=2,
    value=c(0,1),
    step = 0.01,
    round = 2,
    label = 'Analysis Range: ',
    init = function(){
      time = data_env$subject$time_points
      list(
        min = min(time),
        max = max(time),
        value = get_global_var('time_subset', c(0,1))
      )
    }
  ),
  rave:::SelectInput$new(
    inputId = 'selected_electrode',
    choices = "",
    selected = NULL,
    label = 'Electrode: ',
    init = function(){
      electrode = data_env$electrodes
      selected <- get_local_var('selected_electrode', electrode[1])
      if(!selected %in% electrode){
        selected <- electrode[1]
      }
      list(
        choices = electrode,
        selected = selected
      )
    }
  ),
  rave:::NumericInput$new(
    inputId = 'ecog_value_subset',
    label = 'Max Plot Range: ',
    min = 0,
    value=NA,
    max=10000,
    init = function() {
      value = get_local_var('ecog_value_subset', 0)
      return( list(value=value))
    }
  )
)

ENABLE_SUMA = TRUE

UNIVARIATE_MODE = 'selected_electrode'

params = list(
  control_subsetA = c('Aclear_Vclear_rain', 'Aclear_Vclear_rock'),
  control_subsetB = c('Anoisy_Vclear_rain', 'Anoisy_Vclear_rock'),
  time_subset = c(.5, 1.2),
  freq_subset = c(50,150),
  ecog_value_subset = 100,
  baseline_subset = c(-2, 0),
  selected_electrode = 73
)
# shiny will ignore whatever is inside of `params` and use the user inputs to replace these parameters. Your parameter name should be the same as SHINY_INPUT (in this case, is `control_subset`)

# Specify what kind of data you might want to use
# Now, wrap your code inside of a function, this is the body function that shiny will execute once users trigger the `Run` button.

source(system.file('repository/plot_helpers.R', package = 'rave'), local = TRUE)
source(system.file('utils/baselines.R', package = 'rave'), local = TRUE)


SHINY_EXECUTE = function(params, use_cache = TRUE, ...){
  t_ <- Sys.time()


  # Inputs
  sel_electrodes = params$selected_electrode == data_env$electrodes
  print(which(sel_electrodes))

  frequencies <- data_env$subject$frequencies$Frequency %>% round
  time <- data_env$subject$time_points$Time %>% round(3)

  time_sel <- time %>% is_within(params$time_subset)
  freq_sel <- frequencies %>% is_within(params$freq_subset)

  baseline <- time %>% is_within(params$baseline_subset)
  baseline_ind <- which(baseline)
  n_b <- length(baseline_ind)

  trials <- data_env$subject$trials
  trials$stimuli = paste0(trials$Stimulus, '_', trials$Type)
  trial_selA <- trials$stimuli %in% params$control_subsetA
  trial_selB <- trials$stimuli %in% params$control_subsetB
  trial_sel = trial_selA | trial_selB

  trial_ind <- which(trial_sel)

  if(!is.null(params[['.IS_SUMA']])){
    trial_ind <- which(trial_sel)

    if(baseline_ind[1] == 1) {
      cbt_baseline <- data_env$cumsum[trial_ind,,baseline_ind[n_b], sel_electrodes] / n_b
    } else {
      cbt_baseline <- (data_env$cumsum[trial_ind,, baseline_ind[n_b], sel_electrodes] -
                         data_env$cumsum[trial_ind,, baseline_ind[1]-1, sel_electrodes]) / n_b
    }

    content_bc = 100 * vapply(1:length(time), function(ii) {
      (data_env$data[trial_sel,,ii, sel_electrodes] - cbt_baseline) / cbt_baseline
    }, cbt_baseline)
    #
    # print(dim(content_bc))

    #
    # microbenchmark(
    #   calc_baseline(data_env, time_intv = params$baseline_subset,
    #     trial_ind = trial_ind,
    #     use_cache = F),times = 10
    # )

    # content_bc = calc_baseline(data_env, time_intv = params$baseline_subset,
    #                            trial_ind = trial_ind,
    #                            use_cache = use_cache)
    # content_bc = content_bc[ , , , sel_electrodes]



    if(length(params$control_subsetB) < 1) {
      # are we in one-condition mode with A
      trialsA <- apply(content_bc[trial_ind %in% which(trial_selA),freq_sel,time_sel], 1, mean)
      suma_res <- t.test(trialsA, mu=0)$statistic

    } else if (length(params$control_subsetA) < 1){
      # are we in one-condition mode with A
      trialsB <- apply(content_bc[trial_ind %in% which(trial_selB),freq_sel,time_sel], 1, mean)
      suma_res <- t.test(trialsB, mu=0)$statistic

    } else {
      # both conditions are available
      trialsA <- apply(content_bc[trial_ind %in% which(trial_selA),freq_sel,time_sel], 1, mean)
      trialsB <- apply(content_bc[trial_ind %in% which(trial_selB),freq_sel,time_sel], 1, mean)

      suma_res <- t.test(trialsA, trialsB)$statistic
    }

    logger('short circuit to SUMA')
    return(list(
      SUMA_RESULT = suma_res
    ))
  }

  # logger(paste('233: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))

  # Calculating baseline using matrix/vector operations larger chunk cuts execution time in half
  # content_bc <- content
  # for(ii in 1:nrow(content_bc)) {
  #   b <- rowMeans(content[ii,,baseline])
  #   content_bc[ii,,] = 100 * (content[ii,,] - b) / b
  # }
  #
  # currently we aren't doing individual trials, do that to make this faster

  # logger(params$selected_electrode)


   # logger(paste(dim(cumsum_by_trial), collapse = ' '))

  if(baseline_ind[1] == 1) {
    cbt_baseline <- data_env$cumsum[trial_sel,,baseline_ind[n_b], sel_electrodes] / n_b
  } else {
    cbt_baseline <- (data_env$cumsum[trial_sel,, baseline_ind[n_b], sel_electrodes] -
                       data_env$cumsum[trial_sel,, baseline_ind[1]-1, sel_electrodes]) / n_b
  }

  # tmp = as.vector(cbt_baseline)
  # content_bc = 100 * (content - tmp) / cbt_baseline

  content_bc = 100 * vapply(1:length(time), function(ii) {
    (data_env$data[trial_sel,,ii, sel_electrodes] - cbt_baseline) / cbt_baseline
  },cbt_baseline)

  # content_bc <- content[trial_sel,,]
  # b <- content[trial_sel,,1]
  # for(ii in 1:nrow(content_bc)) {
  #   b[ii,] <- rowMeans(content[trial_ind[ii],,baseline_sel])
  #   content_bc[ii,,] = 100 * (content[trial.ind[ii],,] - b[ii,]) / b[ii,]
  # }

  # TODO: check if the "scale region" checkbox is selected
  # if (yes)
  # freq = (frequencies <= params$freq_subset[2]) & (frequencies >= params$freq_subset[1])
  # if (no)
  #freq <- rep(TRUE, length(frequencies))
  # logger(paste('247: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))

  # subset ecog data, using the baseline-corrected version
  # data_from_selection <- function(stim) {
  #   content_bc[trials$stimuli %in% stim, , ]
  # }

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
    # dataA <- data_from_selection(params$control_subsetA)
    dataA <- content_bc[trial_ind %in% which(trial_selA),,]
    n1 <- nrow(dataA)

    dataA_fmse <- freq_mse(dataA[,freq_sel,])
    trialsA <- apply(dataA[,freq_sel,time_sel], 1, mean)

    trials_over_timeA <- vapply(1:length(trialsA), function(ii) {
      colMeans(dataA[ii,freq_sel,])
    }, time) %>% t
  }

  dataB <- NULL
  dataB_fmse <- NULL
  trialsB <- NULL
  n2 <- 0
  if(length(params$control_subsetB) > 0){
    dataB <- content_bc[trial_ind %in% which(trial_selB),,]
    n2 <- nrow(dataB)

    dataB_fmse <- freq_mse(dataB[,freq_sel,])
    trialsB <- apply(dataB[,freq_sel,time_sel], 1, mean)

    trials_over_timeB <- vapply(1:length(trialsB), function(ii) {
      colMeans(dataB[ii,freq_sel,])
    }, time) %>% t
  }

  # logger(paste('275: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))
  group_colors <- c('purple3', 'darkgreen')

  # logger('P: ', suma_res)

  logger(paste('308: Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))

  crp <- colorRampPalette(c('navy', 'steelblue3', 'orange', 'red'), interpolate='linear', space='Lab')(1001)

  # multiple functions are now using this
  conditional_rbind <- function(a,b) {
    m <- a

    if(is.null(m))       { m <- b }
    else if(not_null(b)) { m %<>% rbind(b) }

    return (m)
  }


  if(is.null(dataA)){
    mbtA <- NULL
  }else{
    mbtA <- apply(dataA, 2, colMeans)
  }
  if(is.null(dataB)){
    mbtB <- NULL
  }else{
    mbtB <- apply(dataB, 2, colMeans)
  }


  combined_data <- conditional_rbind(mbtA, mbtB)

  # if(showA)
  #   mbtA <- mbtA %>% clip_x(lim=quantile(combined_data, params$ecog_value_subset))
  # if(showB)
  #   mbtB <- mbtB %>% clip_x(lim=quantile(combined_data, params$ecog_value_subset))

  # now get the newly recombined data
  combined_data <- conditional_rbind(mbtA, mbtB)

  logger('val: ', params$ecog_value_subset)


  pm <- function(x) c(-x, x)

  # set the plotting range for the heatmaps
  if(is.na(params$ecog_value_subset) || is.null(params$ecog_value_subset) || params$ecog_value_subset <= 0){
    zlim <- pm(max(abs(range(combined_data))))
  } else {
    zlim <- pm(abs(params$ecog_value_subset))
  }


  logger('zlim: ', paste(zlim))

  # if zlim is (-Inf, Inf) we don't have data yet?
  if(any(zlim == c(-Inf, Inf))) {
    return (list(heatmap_plot = default_plot, line_plot = default_plot, trial_plot = default_plot, bar_plot = default_plot))
  }

  return(list(
    # For base  images, it will evaluate immediately,  store them as a function
    heatmap_plot = function(){

      par(cex.lab=1.4, cex.axis=1.4, las=1)

      # do we have one or two heatmaps to display?
      showA <- not_null(dataA)
      showB <- not_null(dataB)

      if(showA & showB){
        layout(matrix(1:3, nrow=1), widths=c(4, 4,1))
      } else {
        layout(matrix(1:2, nrow=1), widths=c(6,1))
      }


      draw_img <- function(mean_by_trial, yax=T, xax=T, zlim, main='', main.col='black', ...) {

        mean_by_trial %<>% clip_x(lim=zlim)

        image(x=time, y=frequencies,
          z=mean_by_trial, zlim=zlim,
          col=crp, xlab='Time (s)', ylab='Frequency (Hz)', cex.lab=1.7, axes=F, useRaster = FALSE, ...)

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



      # set plot margins
      par(mar=c(5.1, 4.5, 2, 2), cex.lab=1.4, cex.axis=1.4, las=1)
      if(showA) {
        draw_img(mbtA, main=params$control_nameA, zlim=zlim)
      }

      if(showA & showB)  {
        par(mar=c(5.1, 2.5, 2, 3), cex.lab=1.4, cex.axis=1.4, las=1)
        draw_img(mbtB, main=params$control_nameB, yax=FALSE, zlim=zlim)
      } else if(showB) {
        draw_img(mbtB, main=params$control_nameB, zlim=zlim)
      }

      # now draw the color bar, this should show the actual range of the data
      cbar <- matrix(seq(min(zlim), max(zlim), length=length(crp))) %>% t

      #logger('cbar range: ', range(cbar))
      #logger('cbar range: ', quantile(clipped_data, 0:5/5) %>% format(digits=1))
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

    line_plot = function(){

      freq_range <- frequencies[freq_sel] %>% range %>% paste(collapse='-')

      # determine the range of the data
      # think of a smarter way to handle NA -- maybe call particular function if only one is NULL vs. both are available
      data_range = do_if(not_null(dataA_fmse), {
        c(rowSums(dataA_fmse), colDiff(dataA_fmse))
      }, NA)

      data_range %<>% c(
        do_if(not_null(dataB_fmse), {
          c(rowSums(dataB_fmse), colDiff(dataB_fmse))
        }, NA)
      )

      data_range %<>% range(na.rm=TRUE)

      par(mar=c(5.1, 4.1, 2, 0), cex.lab=1.4, cex.axis=1.4, las=1)

      plot_title <- paste0('N1: ', n1, ', N2: ', n2, '  Freq Range: ', freq_range)

      plot.clean(time, data_range, main= bquote(paste('Freq Range = ', .(freq_range), ' || N'['A']*' = ', .(n1), ', N'['B']*' = ', .(n2))),
        xlab='Time (s)', ylab='Mean % Signal Change', cex.main=1.4)

      if(min(data_range) < 0) abline(h=0, col='lightgray')

      do_poly <- function(x, y, col, lbl='', poly.alpha=50,
        lbl.cex=1.5, lbl.col=col, lbl.x=min(x), lbl.y=0.9*y[2], lbl.adj=c(0, NA), show_label=TRUE) {

        polygon(c(x, rev(x)), rep(y, each=2), border=NA, col=getAlphaRGB(col, poly.alpha))

        if(show_label) text(lbl.x, lbl.y, lbl, cex=lbl.cex, col=lbl.col, adj=lbl.adj)
      }

      #draw poly indicating baseline
      do_poly(params$baseline_subset, data_range, col='gray30', lbl='baseline')

      #draw poly indicating analysis window
      do_poly(params$time_subset, data_range, col='salmon2', lbl='target')

      do_text <- function(x, y, lbl, col, cex=1.5, pos=2, font=2) {
        text(x, y, lbl, col=col, cex=cex, pos=pos, font=font)
      }

      do_line_with_error <- function(x, y, sem, col, lbl, lbl.y, lbl.x=params$baseline_subset %>% max, lwd=2, poly.alpha=100, lbl.pos=2) {
        ebar_polygon(x, y, sem, getAlphaRGB(col, poly.alpha))
        lines(x, y, lwd=lwd, col=col)
        do_text(lbl.x, lbl.y, lbl, col=col, pos=lbl.pos)
      }

      if(!is.null(dataA_fmse)){
        do_line_with_error(time, dataA_fmse[,1], dataA_fmse[,2], group_colors[1],
          params$control_nameA, lbl.y=diff(data_range)  * .7  + data_range[1])
      }

      if(!is.null(dataB_fmse)){
        do_line_with_error(time, dataB_fmse[,1], dataB_fmse[,2], group_colors[2],
          params$control_nameB, lbl.y=diff(data_range)  * .6  + data_range[1])
      }

      #show the axes
      axis(1)
      axis(2, las=1)
    },

    trial_plot = function () {
      # return (plot(1,1))

      do_img <- function(mat, y=1:nrow(mat), ...) {
        image(mat %>% t %>% clip_x(zlim),
          x=time, y=y, las=1, ylab='Trial #', xlab='Time (s)',
          col=crp, zlim=zlim, axes=F, ...)
        axis(1, tcl=0)
        q_axis(y)
        abline(v=0, col='black', lty=3, lwd=2)

        abline(v=params$time_subset, col='white', lty=2, lwd=2)

      }

      par(cex.lab=1.4, cex.axis=1.4, las=1)

      q_axis <- function(x, at=round(quantile(x, 0:4/4)), label=at, ...) {
        axis(2, at=at, label=label, ...)
      }

     elec_str <- paste0('Elec: ', params$selected_electrode,' | ')
      if(is.null(dataB)) {
        do_img(trials_over_timeA, main=paste0(elec_str, params$control_nameA))
      } else if (is.null(dataA)) {
        do_img(trials_over_timeB, main=paste0(elec_str, params$control_nameB))
      } else {
        par(mfrow=1:2)
        do_img(trials_over_timeA, main=paste0(elec_str, params$control_nameA))
        do_img(trials_over_timeB, main=paste0(elec_str, params$control_nameB))
      }

    },

    bar_plot = function () {
      par(mar=c(5.1, 4.1, 1, 1), cex.lab=1.4, cex.axis=1.4, las=1)

      group_data=list(trialsA, trialsB) %>% set_names(c(params$control_nameA, params$control_nameB))

      barplot.scatter(group_data, cols=group_colors, ebar.cols='black')
    }
  ))
}
# Now, define shiny output
SHINY_OUTPUT = list(

  # plot = list(`plotname` = parameters, ....) will render as shiny::plotoutput
  # I'll implement `verbatimTextOutput`, `dataTableOutput`, `plotlyOutput`, `plotOutput`
  `Basic Visualization` = list(
    rave:::PlotOutput$new(
      outputId = 'heatmap_plot',
      title = 'Heat Map (Mean over Trials)',
      # class = 'ratio5to2',
      width = 12  # 1-12
    ),
    rave:::PlotOutput$new(
      outputId = 'line_plot',
      title = 'Mean over Frequency, then Mean +/- SE over Trial',
      width = 8  # 1-12
    ),
    rave:::PlotOutput$new(
      outputId = 'bar_plot',
      title = 'Windowed Comparison',
      width = 4  # 1-12
    ),
    rave:::PlotOutput$new(
      outputId = 'trial_plot',
      title = 'Trials (mean over frequency)',
      # class = 'ratio2to1',
      width = 11  # 1-12
    )
  )
)


# for standalone script, usually for debug use,

if(!exists('NOT_RUN')){
  require(shiny)
  require(plotly)
  require(tidyverse)
  require(magrittr)
  require(stringr)

  rave_opts$set_options(
    # data_dir = 'D:/Documents/Dropbox/Dropbox/rave/data',
    # data_dir = '/Users/beauchamplab/Dropbox/rave/data',
    data_dir = '/Users/jmagnotti/Dropbox/MultisensoryIntegration/ecog/rave/data',
    # module_lookup_file = '/Users/beauchamplab/Dropbox/Researches/rave/inst/modules_dev.csv',
    module_lookup_file = '/Users/jmagnotti/Dropbox/rave/inst/modules.csv',
    debug = 'FALSE',
    delay_input = '10'
  )

  subject_id = 'subject_lij118_ChBr'
  electrodes = 70:71
  module_path = '/Users/jmagnotti/Dropbox/rave/inst/modules/condition_explorer.R'
  attach_virtualenv(subject_id, electrodes, module_path)

  params$selected_electrode = 77
  results <- SHINY_EXECUTE(params)
  results$heatmap_plot()
  results$line_plot()




  detach_virtualenv()
}




