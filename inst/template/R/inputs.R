# Defines inputs initializations

#' Default initialization, run before any input initializations
#' @details
#' \code{__init__XYZ} is a function (with no arguments) defines some global
#' variables that can be used by others. Usually it declairs data to be loaded,
#' restores previous inputs etc.
#'
#' For example, here we define a variable "power" that contains subject
#' power data (wavelet coefficient^2). This variable can be used across the
#' module (no scoping issue)
init_module <- function(){

  # load data
  power = module_tools$get_power(referenced = T)

  # load condition (from epoch file)
  epochs = module_tools$get_meta('trials')
}


#' Function to update select input "electrode"
#' @details
#' In the rave.yaml, we define a select input:
#'
#' variable: 'electrode'
#' type: 'selectInput'
#' label: 'Electrode: '
#' init: 'init_electrode'
#'
#' The input ID is 'electrode', initialize function is 'init_electrode' and its
#' type of 'selectInput', which will be a dropdown menu
#'
#' For type "selectInput", we will use shiny::selectInput as UI element.
#' Please return a named list with format:
#'
#' list(label = ???, choices = ???, selected = ???)
init_electrode <- function(){

  # Get loaded electrodes (use print(preload_info) to obtain what's loaded)
  choices = preload_info$electrodes

  # Obtain user's last input
  selected = as.integer(

    # cache_input is a rave built-in function that stores input variables.
    # key: variable name
    cache_input(inputId = 'ELECTRODE', val = choices[1])
  )

  if(!selected %in% choices){
    selected = choices[1]
  }

  list(
    label = sprintf('Electrode: (%s)', deparse_selections(choices)),
    choices = choices,
    selected = selected
  )
}




init_baseline <- function(){
  time_range = range(preload_info$time_points)

  value = cache_input(inputId = 'BASELINE_RANGE', val = c(time_range[1], 0))

  re = list(
    min = time_range[1],
    max = time_range[2],
    value = value
  )
  print(re)
  re
}



init_baseline_type <- function(){
  list(selected = cache_input('BASELINE_TYPE', 'Decibel'))
}



#' Initialization of compound input "GROUP"
#' @details
#' compoundInput is a little bit complicated compared to other input types.
#' Basically it returns a list containing two keys:
#' 1. initialize
#' 2. value
init_group <- function(){
  cond = unique(preload_info$condition)

  last_value = cache_input(inputId = 'GROUP', list(
    list(
      CONDITION = list(cond),
      GROUP_NAME = 'All Conditions'
    )
  ))


  return(list(
    initialize = list(
      CONDITION = list(
        choices = cond
      )
    ),
    value = last_value
  ))
}
