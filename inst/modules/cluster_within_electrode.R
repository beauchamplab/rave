SHINY_DESCRIPTION = shiny::HTML(
'Cluster on trials for single electrode<br />')
# for standalone script, usually for debug use,
if(!exists('NOT_RUN')){

}

# Pre-process data
calc_baseline = function(){
  # params = params[c('freq_subset', 'baseline_subset', 'time_subset')]
  # re = get_cache(name = 'BASELINE')
  # if(is.list(re) && assertthat::are_equal(
  #   names(re), c('target', 'baseline', 'baselined')
  # )){
  #   return(re)
  # }

  # check if I'm using cache for dev use

  dimnames = list(
    paste0(data_env$subject$trials$Stimulus, '_', data_env$subject$trials$Type),
    data_env$subject$frequencies$Frequency,
    data_env$subject$time_points$Time,
    data_env$subject$electrodes$Label
  )

  names(dimnames) = c('Trials', 'Frequancies', 'Time', 'Electrode')
  dimnames$Frequancies = as.numeric(dimnames$Frequancies)
  dimnames$Time = as.numeric(dimnames$Time)
  dimnames$Electrode = electrode
  tbl_cube(
    dimnames,
    list(
      Ecog = content,
      Cumsum = cumsum_by_trial
    )
  ) ->
    content_tbl
  # Step 1: prepare data
  # 1. subset by frequencies
  freq_sub =
    frequencies >= params$freq_subset[1] &
    frequencies < params$freq_subset[2]

  # 2: calculate baseline
  sel <- which(time <= params$baseline_subset[2] &
                 time >= params$baseline_subset[1])
  start = ifelse(
    1 %in% sel,
    0,
    content_tbl$mets$Cumsum[, freq_sub, min(sel) - 1,]
  )
  end = content_tbl$mets$Cumsum[, freq_sub, max(sel), ]
  baseline = (end - start) / length(sel)
  dim_baseline = dim(content_tbl$mets$Cumsum)[c(1,2,4)]; dim_baseline[2] = sum(freq_sub)
  dim(baseline) = dim_baseline

  # 3: calculate target time
  sel <- which(time < params$time_subset[2] &
                 time >= params$time_subset[1])
  target = content_tbl$mets$Ecog[, freq_sub, sel, ]
  dim_target = dim(content_tbl$mets$Ecog); dim_target[3] = length(sel); dim_target[2] = sum(freq_sub)
  dim(target) = dim_target

  #
  vapply(1:(dim(target)[3]), function(x){
    target[,,x,] / as.numeric(baseline) -1
  }, as.array(baseline)) ->
    baselined

  baselined = aperm(baselined, c(1,2,4,3))

  re = list(
    target = target,
    baseline = baseline,
    baselined = baselined
  )
  # set_cache(name = 'BASELINE', val = re, params = params)
  return(re)
}



SHINY_EXECUTE = function(params, ...){
  t_ <- Sys.time()

  # Step 2: prepare control group (colors)
  group_a = params$control_subsetA
  name_a = get_global_var('name_a', 'Group A')

  group_b = params$control_subsetB
  name_b = get_global_var('name_b', 'Group B')

  trials %>%
    mutate(
      a = stimuli %in% group_a,
      b = stimuli %in% group_b,
      Group = "Ungrouped"
    ) %>%
    mutate(
      Group = replace(Group, b, name_b)
    ) %>%
    mutate(
      Group = replace(Group, a, name_a)
    ) %>%
    mutate(
      Group = as.factor(Group)
    ) %>%
    tbl_df() ->
    trials

  order = order(trials$Group, trials$stimuli)



  cache = calc_baseline()



  tmp = cache$baselined[,,,electrode == params$selected_electrode]
  dim(tmp) = dim(cache$baselined)[-4]

  apply(tmp, 1, function(x){
    apply(x, 2, mean)
  }) ->
    X




  X_grouped = X[, order]



  index = trials[order,]
  X_grouped %>%
    cor(method='spearman') ->
    sigma



  n = nrow(trials)
  n_a = sum(trials$Group == name_a)
  n_b = sum(trials$Group == name_b)

  logger(paste('Calc time: ', (Sys.time() - t_) %>% round(3), ' (s)'))



  return(list(
    spearman_plot = function(){
      require(fields)
      sigma %>%
        {.[, ncol(.):1]} %>%
        image.plot(zlim = c(-1, 1), xlab = 'Col', ylab = 'Row',
                   xaxt = "n", yaxt = "n")
    },
    spearman_plotly = function () {

      data_frame(
        x = rep(1:n, n),
        y = rep(1:n, each = n),
        z = as.numeric(sigma)
      ) %>%
      plot_ly(
        x = ~x,
        y = ~y,
        z = ~z, colorscale = "Greys", type = "heatmap",
        zmin = -1,
        zmax = 1,
        colorbar = list(title = "Spearman Corr"),
        text = ~(
          sprintf('Row: %s (#%s, %s)<br>Col: %s (#%s, %s)<br>Corr: %.2f',
            index$stimuli[y],
            index$trial[y],
            index$Group[y],
            index$stimuli[x],
            index$trial[x],
            index$Group[x],
            z
          )
        ),
        hoverinfo = 'text'
      ) %>%
        add_annotations(x = c(n_a / 2, n_a + n_b/2),
                        y = c(n_a, n_a + n_b) - 10,
                        text = c(name_a, name_b),
                        xref = "x",
                        yref = "y",
                        showarrow = FALSE,
                        bordercolor = 'rgba(255,255,255,1)',
                        # Styling annotations' text:
                        font = list(color = '#FFF',
                                    size = 14)) %>%
        layout(
          shapes = list(
            list(type = "rect",
                 fillcolor = "transparent", line = list(color = "white"),
                 x0 = 0, x1 = n_a, xref = "x",
                 y0 = 0, y1 = n_a, yref = "y"
            ),
            list(type = "rect",
                 fillcolor = "transparent", line = list(color = "white"),
                 x0 = n_a, x1 = n_a + n_b, xref = "x",
                 y0 = n_a, y1 = n_a + n_b, yref = "y"
            )
          ),
          xaxis = list(
            title = "Col"
          ),
          yaxis = list(
            autorange = "reversed",
            title = "Row"
          )
        )
    }
  ))
}

SHINY_VALIDATE = function(params){
  # step 0: check input params
  checks = c(
    need(length(unique(params$freq_subset)) == 2,
         'No frequency range selected.'),
    need(length(unique(params$baseline_subset)) == 2,
         'Baseline range should be an interval'),
    need(length(unique(params$time_subset)) == 2,
         'Target range should be an interval')
  )

  return(checks)
}



SHINY_OUTPUT = list(
  `Visualization` = list(
    rave:::PlotOutput$new(
      outputId = 'spearman_plot',
      class = 'ratio5to4',
      title = 'Spearman Ranked Correlation',  # `?shinydashboard::box` for input params
      width = 12  # 1-12
    )
  ),
  `Interactive Plot` = list(
    rave:::PlotlyOutput$new(
      outputId = 'spearman_plotly',
      class = 'ratio5to4',
      title = 'Spearman Ranked Correlation',  # `?shinydashboard::box` for input params
      width = 12  # 1-12
    )
  )
)








SHINY_INPUT = list(
  # control_nameA = list(
  #   global_var = 'name_a',
  #   placeholder = 'Name of Group A',
  #   label = 'Condition A Name',
  #   value = 'All Conditions',
  #   init = function(){
  #     list(
  #       value = get_global_var('name_a', 'All')
  #     )
  #   }
  # ),
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
      all_trials = unique(trials[,1])
      list(
        choices = unique(trials[,1]),
        selected = get_global_var('group_a', unique(trials[,1]))
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
      all_trials = unique(trials[,1])
      list(
        choices = unique(trials[,1]),
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
      rg = range(frequencies)
      list(
        min = floor(rg[1]),
        max = ceiling(rg[2]),
        value = get_global_var('freq_subset', c(0, 250))
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
      list(
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
  rave:::RadioButtons$new(
    inputId = 'cluster_method',
    choices = c(
      "Spearman's Correlation" = 'ranked_corr',
      'K-means' = 'kmeans'
    ),
    selected = 'ranked_corr',
    label = 'Clustering Method: ',
    init = function(){
      list(
        selected = get_local_var('cluster_method', 'ranked_corr')
      )
    }
  )
)




params = list(
  control_subsetA = c('Aclear_Vclear_rain',
                      'Aclear_Vclear_rock',
                      'Aclear_Vnoisy_rain',
                      'Aclear_Vnoisy_rock'
                      ),
  control_subsetB = c('Anoisy_Vclear_rain',
                      'Anoisy_Vclear_rock',
                      'Anoisy_Vnoisy_rain',
                      'Anoisy_Vnoisy_rock'),
  time_subset = c(.72, 0.84),
  freq_subset = c(50,150),
  baseline_subset = c(-2, 0),
  cluster_method = 'ranked_corr',
  selected_electrode = 73
)
