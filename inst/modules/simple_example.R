
SHINY_DESCRIPTION = shiny::HTML(
  '
  Test Module
  ')

SHINY_INPUT = list(
  rafe:::SelectInput$new(
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
  )
)

ENABLE_SUMA = TRUE

# DEBUG
params = list(
  selected_electrode = 73
)

SHINY_EXECUTE = function(params, use_cache = TRUE, ...){

  return(list(
    conclusion = function(){
      cat('Hello the other world ', params$selected_electrode, data_env$subject$electrode_label_by_index(params$selected_electrode))
    }
  ))
}
# Now, define shiny output
SHINY_OUTPUT = list(

  # plot = list(`plotname` = parameters, ....) will render as shiny::plotoutput
  # I'll implement `verbatimTextOutput`, `dataTableOutput`, `plotlyOutput`, `plotOutput`
  `Basic Visualization` = list(
    verbatimtext_output('conclusion', 'Test result')
  )
)

