`__reactive__` <- function(){

  # This is a special function designed for shiny reactive functions or RAVE
  # customized inputs/outputs. Expressions within this function will be evaluated
  # under runtime environment, hence here you can access any varaibles generated
  # within __init__. In addition, you can also use "result", generated from
  # __main__ and use this variable as if it's a list.
  # In addition, you may access rave toolboxes here (like preload_info, module_tools,
  # subject, etc.)
  #
  # This environment is super convenient. However, f you want your package
  # running as a toolbox instead of some GUI application, please only run
  # reactive functions here.

  # To start, we declare some special reactive values here. They have fixed usages

  # Session, input and output are created for you and to be used as global variables
  # session <- getDefaultReactiveDomain()
  # input <- getDefaultReactiveInput()
  # output <- getDefaultReactiveOutput()
  local_data <- reactiveValues()

  # ----------------------- Modify the chunk below -----------------------
  input_customized <- function(){
    tagList(
      h3('Title'),
      p('You can add some text here. Basically you need to learn a little bit HTML ',
        span(a('shiny', href = 'https://shiny.rstudio.com/articles/tag-glossary.html', target = '_blank')), '. Use ',
        pre('?shiny::builder'), ' for available HTML builders', br(), br(),
        "Here is a 3D viewer"),
      threejsOutput(ns('viewer_3d'), height = '300px'),
      p('Here\'s an example of textInput'),
      textInput(ns('text_id'), 'Enter a Text', placeholder = 'Enter anything!'),
      verbatimTextOutput(ns('text_out'))
    )
  }

  output$text_out <- renderPrint({
    cat(input$text_id)
  })

  output$viewer_3d <- renderThreejs({
    module_tools$plot_3d_electrodes(show_mesh = FALSE)
  })



  # ----------------------- End of the chunk -----------------------

}
