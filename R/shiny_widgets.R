# Construction of rave-shiny Components

NULL


#' Tabulize Input Groups
#' @description Input component for different tabs/categories
#' @param inputId ID (unique), characters, then in the module, you can use param$this_id to get selected tab name.
#' @param label Label name for this component
#' @param selected Which tab should be selected when lauch the module. By default NULL means the first tab.
#' @param ... name=value list, see example
#' @export
section_input <- function(inputId, label, selected = NULL, ...){
  return(SectionInput$new(inputId, label, selected = NULL, ...))
}

#' @export
text_input <- function(inputId, label, ...){
  return(TextInput$new(inputId, label, ...))
}

#' @export
numeric_input <- function(inputId, label, value, ...){
  return(NumericInput$new(inputId, label, value, ...))
}

#' @export
checkbox_input <- function(inputId, label, ...){
  return(CheckboxInput$new(inputId, label, ...))
}

#' @export
select_input <- function(inputId, label, choices, ...){
  return(SelectInput$new(inputId, label, choices, ...))
}

#' @export
slider_input <- function(inputId, label, min, max, value, ...){
  return(SliderInput$new(inputId, label, min, max, value, ...))
}

#' @export
action_button <- function(inputId, label, ...){
  return(ActionButton$new(inputId, label, ...))
}

#' @export
file_input <- function(inputId, label, ...){
  return(FileInput$new(inputId, label, ...))
}




#' @export
plot_output <- function(outputId, title, ...){
  return(PlotOutput$new(outputId, title, ...))
}

#' @export
plotly_output <- function(outputId, title, ...){
  return(PlotlyOutput$new(outputId, title, ...))
}

#' @export
datatable_output <- function(outputId, title, ...){
  return(DataTableOutput$new(outputId, title, ...))
}

#' @export
verbatimtext_output <- function(outputId, title, ...){
  return(VerbatimTextOutput$new(outputId, title, ...))
}


#UiOutput
