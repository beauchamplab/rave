# Set default color schemes
rave_color_ramp_palette <- colorRampPalette(c('navy', 'white', 'red'), interpolate='linear', space='Lab')

rave_heat_map_colors <- rave_color_ramp_palette(1001)

# put this hear for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <- c('purple3', 'orange', 'dodgerblue3', 'darkgreen', 'orangered', 'brown')

# allow color cycling
get_color <- function(ii) {
  group_colors[ii %% length(group_colors) + 1]
}

rave_colors <- list('BASELINE_WINDOW'='gray60', 'ANALYSIS_WINDOW' = 'salmon2', 'GROUP'=group_colors,
                    'TRIAL_TYPE_SEPARATOR'='gray40')

rave_main <- function(main, cex=rave_cex.main, col='black', font=1) {
  title(main=list(main, cex=cex, col=col, font=font))
}


itpc_plot = function(plot_data = NULL){


  # legend
  if(exists('calc_result')){
    plot_data %?<-% calc_result
  }
  has_groups %?<-% FALSE

  validate(need(
    has_groups &&
    is.list(plot_data) && !is.null(plot_data$n_groups) && plot_data$n_groups > 0,
    message = 'No Condition/Data Found'
  ))

  mar = c(5.1, 5.1, 2, 2)
  lmar = c(5.1, 5.1, 2, 2)

  n_groups = plot_data$n_groups
  actual_lim = plot_data$actual_lim
  time = plot_data$time
  frequency = plot_data$frequency

  easy_layout(n_groups, nrows = (n_groups>=4) + 1,s_margin = mar, legend = {
    rave_color_bar(zlim = c(0, max(0.001, max_zlim)), actual_lim = actual_lim,
                   ylab = 'Inter-Trial Coherence', sym = F, ticks = actual_lim, mar = lmar)
  }, legend_size = ifelse(n_groups > 1, lcm(3.5), lcm(4.5)))

  # Plots
  lapply(plot_data$results, function(x){
    x$has_trials %?<-% FALSE
    if(x$has_trials){
      draw_img(x$data, x = time, y = frequency, xlab='Time (s)', ylab='Frequency (Hz)', zlim = c(-max_zlim, max_zlim),
               main = x$name)

      heat_map_axes(time,frequency, xax=T, yax=T)
      abline(v = 0, lwd = 3, lty = 2)
    }
  })
}

itpc_time_plot = function(plot_data = NULL, merge_plots){
  if(exists('calc_result')){
    plot_data %?<-% calc_result
  }
  if(exists('MERGE_PLOTS')){
    merge_plots %?<-% MERGE_PLOTS
  }
  if(!exists('has_groups')){
    has_groups = FALSE
  }

  validate(need(
    has_groups && is.list(plot_data) && !is.null(plot_data$n_groups) && plot_data$n_groups > 0,
    message = 'No Condition/Data Found'
  ))

  n_groups = plot_data$n_groups
  actual_lim = plot_data$actual_lim
  time = plot_data$time
  frequency = plot_data$frequency

  # Layout
  cols = lapply(seq_along(plot_data$results), function(ii){
    x = plot_data$results[[ii]]
    if(x$has_trials){
      list(
        group_name = ifelse(is.blank(x$group_name), sprintf('Group %d', ii), x$group_name),
        color = get_color(ii)
      )
    }else{
      NULL
    }
  })

  cols = dropNulls(cols)
  mar = c(5.1, 5.1, 2, 2)
  par(mar = mar)

  if(merge_plots){
    # use one big plot
    plot.clean(time, c(0,1), xlab = 'Time (s)', ylab = 'Inter-Trial Coherence')

    rave_axis(1, at=pretty(time), tcl=0, lwd=1)
    rave_axis(2, at=pretty(seq(0, 1, length.out = 11)), tcl=0, lwd=1)
    abline(v = 0, lwd = 3, lty = 2)
    # legend at 0, 0.9
    col = sapply(cols, '[[', 'color')
    legend(x = 0, y = 1, legend = sapply(cols, '[[', 'group_name'), lty = 1,
           col = col, cex = 1, bty = 'n', bg = c('#FFFFFFAA'),
           ncol = 2, text.col = col)
  }else{
    nrows = (n_groups>=4) + 1
    ncols = ceiling(n_groups / nrows)
    layout(
      matrix(seq_len(nrows * ncols), nrow = nrows, byrow = T)
    )
  }

  par(mar = mar)
  lapply(seq_along(plot_data$results), function(ii){
    x = plot_data$results[[ii]]
    if(!x$has_trials){
      return(NULL)
    }
    apply(x$data, 1, function(x){
      c(mean(x), sd(x))
    }) ->
      a
    if(!merge_plots){
      plot.clean(time, c(0,1), xlab = 'Time (s)', ylab = 'Inter-Trial Coherence')
      rave_axis(1, at=pretty(time), tcl=0, lwd=1)
      rave_axis(2, at=pretty(seq(0, 1, length.out = 11)), tcl=0, lwd=1)
      abline(v = 0, lwd = 3, lty = 2)

      legend(x = 0, y = 1, legend = ifelse(is.blank(x$group_name), sprintf('Group %d', ii),
                                           x$group_name),
             lty = 1,
             col = get_color(ii), cex = 1, bty = 'n', bg = c('#FFFFFFAA'),
             ncol = 2, text.col = get_color(ii))
    }
    ebar_polygon(time, a[1, ], a[2, ],
                 add_line = TRUE, col=get_color(ii))
  })




}
