SHINY_INPUT = list(
  slider_input('baseline_subset', label = 'Baseline Rage', min = 0, max = 1, round = -2, value = c(0,1), init = function(){
    time = data_env$subject$time_points$Time

    return(list(
      min = min(time),
      max = max(time),
      value = get_global_var('baseline_subset')
    ))
  }, global_var = 'baseline_subset'),

  select_input('selected_electrode', 'Electrode', choices = '', init = function(){
    list(
      choices = data_env$electrodes,
      selected = get_local_var('selected_electrode')
    )
  }),

  slider_input('alpha', 'Alpha Band', min = 0, max = 1, value = c(0, 1), round = TRUE, init = function(){
    freq = data_env$subject$frequencies$Frequency
    # 7 - 13
    return(list(
      min = min(freq),
      max = max(freq),
      value = get_local_var('alpha', c(7, 13))
    ))
  }),
  slider_input('beta', 'Beta Band', min = 0, max = 1, value = c(0, 1), round = TRUE, init = function(){
    freq = data_env$subject$frequencies$Frequency
    # 7 - 13
    return(list(
      min = min(freq),
      max = max(freq),
      value = get_local_var('alpha', c(14, 25))
    ))
  })
)

params = list(
  baseline_subset = c(-1.5, 0),
  selected_electrode = 72,
  alpha = c(7, 13)
)

# you might want to init_app() before running the following line
# attach_virtualenv('subject_lij118_ChBr', 72:75)

SHINY_VALIDATE = function(params, ...){
  return(c(
    need(
      params$baseline_subset[1] != params$baseline_subset[2], 'Wrong baseline range'
    ),
    need(
      params$alpha[1] != params$alpha[2], 'Wrong alpha range'
    ),
    need(
      params$beta[1] != params$beta[2], 'Wrong beta range'
    )
  ))
}

SHINY_EXECUTE = function(params, ...){
  # Calculate baseline
  sel_electrodes = params$selected_electrode == data_env$electrodes

  time = data_env$subject$time_points$Time

  time_ind = which(
    time >= params$baseline_subset[1] &
    time <= params$baseline_subset[2]
  )

  if(1 %in% time_ind){
    start = 0
  }else{
    start = data_env$cumsum[,, min(time_ind),sel_electrodes]
  }
  end = data_env$cumsum[,, max(time_ind), sel_electrodes]

  cbt_baseline = (end - start) / length(time_ind)

  content_bc = 100 * vapply(1:length(time), function(ii) {
    (data_env$data[,,ii, sel_electrodes] - cbt_baseline) / cbt_baseline
  },cbt_baseline)

  # END, content_bc is the baselined data

  # Re-order trials
  trials = data_env$subject$trials
  trial_order = order(str_c(trials$Type, trials$Stimulus))

  content_bc[trial_order,,]

  # NEXT, subset freq
  freq = data_env$subject$frequencies$Frequency

  freq_subset =
    freq >= params$alpha[1] &
    freq <= params$alpha[2]

  t(apply(content_bc[, freq_subset, ], 1, function(x){
    colMeans(x)
  })) ->
    alpha_band



  return(list(
    alpha_band_plot = function(){
      image.plot(t(alpha_band))
    }
  ))
}

SHINY_OUTPUT = list(
  Text = list(
    plot_output('alpha_band_plot', 'Alpha Band for each Trials')
  )
)
