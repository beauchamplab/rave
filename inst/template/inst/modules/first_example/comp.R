# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(shiny)
require(rave)
require(${{PACKAGE}})

env = dev_${{PACKAGE}}(TRUE)

#' Load subject for debugging
#' Make sure this is executed before developing the module to make sure
#' at least one subject is loaded
mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------

#' define_initialization is executed every time when:
#'   *** a subject is loaded
#'
#' You can use this function to do:
#'   1. Check if data is complete
#'   2. Define some "global" variables
#'
#' Check package vignette to see the explanation

define_initialization({
  # Enter code to handle data when a new subject is loaded
})




#  ---------------------------------  Inputs -----------------------------------
#' Define inputs
#'
#' Use function `define_input` to define inputs.
#' Make sure rave toolbox (dev_[your_package_name]) is loaded
#'
#' @Usage: define_input(definition, init_args, init_expr)
#'
#' @param definition defines input types, for example:
#'     textInput(inputId = 'text_electrode', label = 'Electrode')
#'   defines a character variable `text_electrode` as one of the inputs
#'
#' Here are some possible types
#'   1. textInput is an input for characters
#'   2. numericInput is for numbers
#'   3. sliderInput is for number or number ranges
#'   4. selectInput is to provide choices
#'   5. checkboxInput is for Yes/No options
#'   6. compoundInput is for multiple group inputs
#'   7. customizedUI is for advanced UI controls
#'
#'   Most of basic UI widgets can be found at:
#'       https://shiny.rstudio.com/gallery/widget-gallery.html
#'
#'   The easiest way to look for usage is using `help` function:
#'   help('textInput'), or ??textInput
#'
#'
#' @param init_args,init_expr define the action when a subject is loaded
#'
#' Use the definition
#'
#'     textInput(inputId = 'text_electrode', label = 'Electrode')
#'
#' as an example. You might want label to update according to electrodes loaded.
#' In this case, you can assign
#'     init_args = 'label'
#' indicating the label of this input will update according to actual data.
#'
#' You may also change multiple arguments. The following code is an extended example
#'
define_input(
  definition = textInput(inputId = 'text_electrode', label = 'Electrode'),

  # Use help('textInput') to see the definition, or help('updateTextInput')
  # to see which input element(s) can be changed. In this case, they are:
  #   label, value, placeholder

  # We will change label and value
  # label will tell users which electrodes are loaded
  # value will be the first electrode

  init_args = c('label', 'value'),
  init_expr = {

    # check ?rave_prepare for what kind of data are loaded
    loaded_electrodes = preload_info$electrodes

    # Generate text for loaded electrodes
    text = dipsaus::deparse_svec(loaded_electrodes)

    # Update
    label = paste0('Electrode (', text, ')')
    value = loaded_electrodes[1]
  }
)

define_input(
  selectInput(inputId = 'requested_conditions', multiple = TRUE, choices = character(0), label='Conditions to include'),
  
  init_args = c('choices', 'selected'),
  
  init_expr = {
    # the module_tools object allows access to the meta data, including the "trial label" variable called Condition
    # The trial numbers and condition labels, are in the epoch file
    # rstudioapi::navigateToFile(file.path(rave::rave_options('data_dir'), 'demo','DemoSubject','rave', 'meta','epoch_auditory_onset.csv'))
    
    choices = unique(module_tools$get_meta('trials')$Condition)
    selected = choices
  }
)


define_input(
  sliderInput(inputId = 'requested_frequencies', label='Frequency Window', min = 0, max=1, value=0:1, step = 1, round=TRUE),
  
  init_args = c('min', 'max', 'value'),
  
  init_expr = {
    # The frequency labels are defined in:
    # rstudioapi::navigateToFile(file.path(rave::rave_options('data_dir'), 'demo','DemoSubject','rave', 'meta','frequencies.csv'))
    
    min = min(preload_info$frequencies)
    max = max(preload_info$frequencies)
    value = c(min,max)
  }
)

define_input(
  sliderInput(inputId = 'requested_baseline', label='Baseline Window', min = 0, max=1, value=0:1),
  
  init_args = c('min', 'max', 'value'),
  
  init_expr = {
    min = min(preload_info$time_points)
    max = max(preload_info$time_points)
    value = c(min,0)
  }
)

# the input_layout list is used by rave to determine order and grouping of layouts
input_layout <- list(
  'Select electrodes' = list('text_electrode'
  ),
  '[-]Analysis parameters' = list(
    'requested_baseline',
    'requested_frequencies'
  ),
  '[-]Select conditions' = list(
    'requested_conditions'
  )
)

# End of input
# ----------------------------------  Outputs ----------------------------------
#' Define Outputs
#'
#' Use function `define_output` to define outputs.
#' Make sure rave toolbox (dev_[your_package_name]) is loaded.
#'
#' @Usage: define_output(definition, title, width, order)
#'
#' @param definition defines output types, for example:
#'     verbatimTextOutput('text_result')
#'   defines output type that dumps whatever is printed by function `text_result`
#'
#' Here are some possible types
#'   1. textOutput is an output for characters
#'   2. verbatimTextOutput is for console print
#'   3. plotOutput is for figures
#'   4. tableOutput is to tables
#'   5. customizedUI is for advanced UI controls
#'
#'   There are lots of output types and R packages such as DT, threejsr can provide
#'   very useful output types. Please check vignettes.
#'
#'   The easiest way to look for usage is using `help` function:
#'   help('verbatimTextOutput'), or ??verbatimTextOutput
#'
#'
#' @param title is the title for output
#'
#' @param width an integer from 1 to 12, defines the percentage of output width
#'   12 means 100% width, 6 means 50% and 4 means 33% width.
#'
#' @param order numeric order of outputs. Outputs will be re-ordered by this argument
#'
define_output(
  definition = verbatimTextOutput('text_result'),
  title = 'This Output Shows all the Printed Results',
  width = 12,
  order = 1
)

define_output(
  plotOutput('plot_result'),
  title = 'My Plot result',
  width = 12,
  order = 2
)

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# -------------------------------- View layout ---------------------------------

# Preview

view_layout('${{MODULEID}}')
