###########################
# Example module
# Author: Zhengjia Wang
###########################


# Definitions of input
# There are more than seven types of inputs, for "text_input", type "?text_input" to see documentations
#
# For all inputs, "inputId" are important since they are local names for your inputs
# For example, "text_input(inputId = 'textA',..."
# Here we have a variable "params$textA" that you can use as a *string* in `SHINY_EXECUTE`
SHINY_INPUT = list(
  text_input(inputId = 'textA', label = 'Please enter a text', init = function(){
    return(list(
      value = get_local_var('textA', paste('Default text', Sys.time()))
    ))
  }),
  numeric_input(inputId = 'numB', label = 'This is a random number', value = 0, init = function(){
    return(list(
      value = rnorm(1)
    ))
  })
)

# (optional) parameters for local debug
# We have two input IDs, "textA", and "numB"
params = list(
  textA = 'debug text',
  numB = 1
)

# Algorithm part, process data here
SHINY_EXECUTE = function(params, ...){

  # Pre-process data

  s = paste(params$textA, '|', params$numB)

  # Return a named list of functions
  return(list(

    # The name "output_text" will be used as output ID in "SHINY_OUTPUT"
    output_text = function(){
      return(s)
    }
  ))
}

# Visualization settings, a named list of output tabs and components
SHINY_OUTPUT = list(

  # "First tab" is the name of the tab
  `First tab` = list(

    # "output_text" is output ID defined in "SHINY_EXECUTE"
    verbatimtext_output(outputId = 'output_text', title = 'Output Text')
  ),
  `Second tab` = list()
)
