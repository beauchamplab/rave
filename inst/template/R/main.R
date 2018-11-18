#' Main body part for algorithms
main_module <- function(ELECTRODE, BASELINE_RANGE, GROUP, ...){

  result = list()

  # -------------------------- main --------------------------
  # -- This is an example, replace this chunk with your own code

  ELECTRODE = as.integer(ELECTRODE)

  # Task 0: store inputs
  result$start_time = Sys.time() # Timer
  result$user_inputs = c(list(
    ELECTRODE = ELECTRODE,
    BASELINE_RANGE = BASELINE_RANGE,
    GROUP = GROUP
  ))

  # Task 1: check input validity:
  # We need at least one group with condition
  group_data = lapply(GROUP, function(g){
    g$CONDITION = unlist(g$CONDITION)
    if(!length(g$CONDITION)){
      return(FALSE)
    }else{
      return(g)
    }
  })

  # Returns TRUE if a group is invalid
  invalid = vapply(group_data, isFALSE, logical(1))

  # We need at least one group to be valid
  validate(need(length(invalid) && !all(invalid), message = 'No Valid Condition Group'))
  group_data = group_data[!invalid]

  # Task 2: Subset amplitude, baseline and collapse by frequency
  # subset selected electrode, only subset trial by the first group
  el <- power$subset(
    Trial = Trial %in% epochs$Trial[epochs$Condition %in% group_data[[1]]$CONDITION],
    Electrode = Electrode == ELECTRODE
  )
  # baseline
  el <- baseline(el, from = BASELINE_RANGE[1], to = BASELINE_RANGE[2], method = 'mean',
                 # If baseline type is "Percentage", then unit = '%'
                 # Otherwise unit = 'dB', i.e. decibel unit
                 unit = ifelse(BASELINE_TYPE == 'Percentage', '%', 'dB'),
                 mem_optimize = F, hybrid = F)

  # collapse over frequencies, keep trial, time, i.e. 1, 3 dimensions
  el_collapsed <- el$collapse(keep = c(1,3))
  # stores result
  result$el_collapsed = el_collapsed

  # Task 2: Store useful information to result, as this variable will be the
  # inputs for all output functions.
  # You need to pass some variables from __init__ since output functions can't
  # access these variabled
  result$epochs = epochs
  result$time_points = preload_info$time_points

  result$end_time = Sys.time()


  # -------------------------- End of chunk --------------------------

  return(result)
}


