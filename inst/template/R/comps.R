#' A util function to match formals
match_formals <- function(args, fun, dots = FALSE){
  if(!is.function(fun)){
    stop('fun must be a function')
  }

  params = formalArgs(fun)

  if(('...' %in% params) && dots){
    # This function has dots and we allow dots, so nothing to subset
    return(args)
  }

  # We subset arg to match with call and remove additional args
  params = params[!params %in% c('', '...')]
  return(args[names(args) %in% params])
}

#' Function returns UI inputs (Advanced)
to_input_component <- function(type, inputId, label, ...){
  args = list(...)

  # Ignore width. You can remove this line if you are familiar with shiny
  # and don't want to use RAVE input layouts
  args[['width']] = NULL
  switch(
    type,

    #---- selectInput ----
    'selectInput' = {
      args[['selected']] %?<-% ''
      args[['choices']] %?<-% ''
      args = match_formals(args,shiny::selectInput)
      rlang::quo(shiny::selectInput(
        inputId = !!inputId, label = !!label, !!!args))
    },



    #---- sliderInput ----
    'sliderInput' = {
      args[['min']] %?<-% 0
      args[['max']] %?<-% 1
      args[['value']] %?<-% 0
      args = match_formals(args,shiny::sliderInput)
      rlang::quo(shiny::sliderInput(
        inputId = !!inputId, label = !!label, !!!args))
    },



    #---- textInput ----
    'textInput' = {
      args = match_formals(args,shiny::textInput)
      rlang::quo(shiny::textInput(
        inputId = !!inputId, label = !!label, !!!args))
    },



    #---- numericInput ----
    'numericInput' = {
      args[['value']] %?<-% 0
      args = match_formals(args,shiny::numericInput)
      rlang::quo(shiny::numericInput(
        inputId = !!inputId, label = !!label, !!!args))
    },


    #---- checkboxInput ----
    'checkboxInput' = {
      args = match_formals(args,shiny::checkboxInput)
      rlang::quo(shiny::checkboxInput(
        inputId = !!inputId, label = !!label, !!!args))
    },



    #---- checkboxGroupInput ----
    'checkboxGroupInput' = {
      args[['selected']] %?<-% ''
      args[['choices']] %?<-% ''
      args = match_formals(args,shiny::checkboxGroupInput)
      rlang::quo(shiny::checkboxGroupInput(
        inputId = !!inputId, label = !!label, !!!args))
    },




    #---- radioButtons ----
    'radioButtons' = {
      args[['selected']] %?<-% ''
      args[['choices']] %?<-% ''
      args = match_formals(args, shiny::radioButtons)
      rlang::quo(shiny::radioButtons(
        inputId = !!inputId, label = !!label, !!!args))
    },




    #---- customizedInput ----
    'customizedInput' = {
      args = match_formals(args,rave::customizedUI)
      rlang::quo(rave::customizedUI(
        inputId = !!inputId, !!!args))
    },



    #---- compoundInput ----
    'compoundInput' = {
      args = match_formals(args, rave::compoundInput)
      # Now for each components, get expression
      ids = names(args$components)
      components = lapply(ids, function(inputId){
        comp = args$components[[inputId]]
        comp$inputId = inputId
        do.call('to_input_component', comp)
      })
      args[['components']] = NULL

      rlang::quo(rave::compoundInput(
        inputId = !!inputId, label = !!label, !!!args, components = {!!!components}))
    }

  )
}

#' Function returns UI outputs (Advanced)
to_output_component <- function(type, outputId, width, ...){
  args = list(...)
  switch (
    type,


    #---- textOutput, display text only ----
    'textOutput' = {
      args = match_formals(args,shiny::textOutput)
      rlang::quo(shiny::textOutput(outputId = !!outputId, width = !!width, !!!args))
    },




    #---- plotOutput, display a plot ----
    'plotOutput' = {
      args = match_formals(args,shiny::plotOutput)
      rlang::quo(shiny::plotOutput(outputId = !!outputId, width = !!width, !!!args))
    },



    #---- verbatimTextOutput, display raw print ----
    'verbatimTextOutput' = {
      args = match_formals(args,shiny::verbatimTextOutput)
      rlang::quo(shiny::verbatimTextOutput(outputId = !!outputId, width = !!width, !!!args))
    },


    #---- imageOutput, display a image given script path ----
    'imageOutput' = {
      args = match_formals(args,shiny::imageOutput)
      rlang::quo(shiny::imageOutput(outputId = !!outputId, width = !!width, !!!args))
    },




    #---- tableOutput, displays a table ----
    'tableOutput' = {
      args = match_formals(args, shiny::tableOutput)
      rlang::quo(shiny::tableOutput(outputId = !!outputId, width = !!width, !!!args))
    },



    #---- CustomizedOutput ----
    'customizedOutput' = {
      args = match_formals(args,shiny::uiOutput)
      rlang::quo(shiny::uiOutput(outputId = !!outputId, width = !!width, !!!args))
    }

  )
}


