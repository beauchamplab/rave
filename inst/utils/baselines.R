
calc_baseline = function(data_env, freq_intv = NULL, time_intv,
                         target_intv = NULL, trial_ind = NULL, use_cache = TRUE){

  if(use_cache){
    # try from cache
    cache = get_cache('BASELINED')
    if(!is.null(cache)){
      try({
        check =
          (cache$params$subject_id == data_env$subject$id) &&
          (assertthat::are_equal(cache$params$freq_intv, freq_intv)) &&
          (assertthat::are_equal(cache$params$time_intv, time_intv)) &&
          (assertthat::are_equal(cache$params$target_intv, target_intv)) &&
          (assertthat::are_equal(cache$params$trial_ind, trial_ind))

        if(check){
          return(cache$value)
        }
      })
    }
  }

  # dim:
  # Trial Freq Time Elec
  # For each trial

  dimnames = dimnames(data_env$data)
  dims = dim(data_env$data)
  names(dimnames) = c('Trials', 'Frequencies', 'Time', 'Electrode')
  if(is.null(trial_ind)){
    trial_sel = 1:length(dimnames$Trials)
  }else{
    trial_sel = trial_ind
  }
  dimnames$Frequencies = as.numeric(dimnames$Frequencies)
  dimnames$Time = as.numeric(dimnames$Time)
  dimnames$Trials = dimnames$Trials[trial_sel]; dims[1] = length(trial_sel)
  data = data_env$data[trial_sel,,,]; dim(data) = dims
  cumsum = data_env$cumsum[trial_sel,,,]; dim(cumsum) = dims
  # dimnames$Electrode = electrode
  tbl_cube(
    dimnames,
    list(
      Ecog = data,
      Cumsum = cumsum
    )
  ) ->
    content
  rm(data, cumsum)
  # Step 1: prepare data
  # 1. subset by frequencies
  if(is.null(freq_intv)){
    freq_sub = rep(TRUE, length(content$dims$Frequencies))
  }else{
    freq_sub =
      content$dims$Frequencies >= freq_intv[1] &
      content$dims$Frequencies < freq_intv[2]
  }


  # 2: calculate baseline
  sel <- which(content$dims$Time <= time_intv[2] &
                 content$dims$Time >= time_intv[1])
  start = ifelse(
    1 %in% sel,
    0,
    content$mets$Cumsum[, freq_sub, min(sel) - 1,]
  )
  end = content$mets$Cumsum[, freq_sub, max(sel), ]
  baseline = (end - start) / length(sel)
  dim_baseline = c(
    length(content$dims$Trials),
    sum(freq_sub),
    length(content$dims$Electrode)
  )

  dim(baseline) = dim_baseline

  # 3: calculate target time
  if(is.null(target_intv)){
    target_ind = 1:length(content$dims$Time)
  }else{
    target_ind = which(
      target_intv[1] <= content$dims$Time &
        target_intv[2] >= content$dims$Time
    )
  }
  target = content$mets$Ecog[, freq_sub, target_ind, ]
  dim_target = dim(content$mets$Ecog);
  dim_target[2] = sum(freq_sub);
  dim_target[3] = length(target_ind)
  dim(target) = dim_target

  vapply(1:dim_target[3], function(x){
    target[,,x,] / as.numeric(baseline) -1
  }, baseline) ->
    baselined

  baselined = 100 * aperm(baselined, c(1,2,4,3))

  # re = list(
  #   target = target,
  #   baseline = baseline,
  #   baselined = baselined
  # )

  if(use_cache){
    set_cache('BASELINED', val = baselined,
              params = list(
                subject_id = data_env$subject$id,
                freq_intv = freq_intv,
                time_intv = time_intv,
                target_intv = target_intv,
                trial_ind = trial_ind))
  }

  return(baselined)
}
