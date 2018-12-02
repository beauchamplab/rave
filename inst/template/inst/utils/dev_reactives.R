getDefaultReactiveDomain = function(){
  'Usage: session <- getDefaultReactiveDomain()
    This is not often used, unless you want to update some input elements in
    customized inputs/outputs. For example in a customized UI, we define a
    textInput:
        ...
        # customized input
        textInput(inputId = ns("text_id"), label = "Input Text: ")
        ...
    Later in other places, you want to update the input label and change
    "Input Text: " to "Updated Label: ", you need to use shiny input update
    function `updateTextInput` (use help function to see the details):
        updateTextInput(session, inputId = "text_id", label = "Updated Label: ")

    Usually this is the only place to use session object. For more details,
    please refer to
        1. shiny input widgets:
              https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
        2. A shint input demo:
              https://shiny.rstudio.com/gallery/update-input-demo.html
    '
  
  rave::logger('A fake session is created. Do NOT read, assign any value to this object')
  fakesession = new.env()
  fakesession$sendInputMessage = function(inputId, message){
    key = NULL
    if('value' %in% names(message)){ key = 'value' }
    if('selected' %in% names(message)){ key = 'selected' }
    
    if(!is.null(key)){
      old_val = localenv$reactive_outputs[[inputId]]
      if(!is.null(old_val)){
        msg = attr(old_val, 'msg')
      }else{
        msg = NULL
      }
      
      localenv$reactive_outputs[[inputId]] = message[[key]]
      attr(localenv$reactive_outputs[[inputId]], 'msg') = msg
      class(localenv$reactive_outputs[[inputId]]) = 'msg'
    }
    
    localenv$reactive_outputs[inputId] = message
    return(message)
  }
  fakesession$userData = new.env(parent = emptyenv())
  fakesession$userData$rave_id = '__fake_session__'
  
  class(fakesession) = c('msg', 'environment')
  attr(fakesession, 'msg') = 'This is a fake session for dev use'
  lockEnvironment(fakesession, bindings = T)
  fakesession
}

session = getDefaultReactiveDomain()

print.reactive_dev <- function(x, ...){
  print(x())
}

`[[.reactive_dev` = `$.reactive_dev` = function(x, k){
  y = x()
  y[[k]]
}
`[.reactive_dev` = `[.reactive_dev<-` = function(...){
  stop('No `[`, `[<-` methods for reactive values')
}

# Reactives
get_inputs = function(){
  ids = names(config$inputs)
  inputId = lapply(ids, function(inputId){
    comp = config$inputs[[inputId]]
    comp$inputId = inputId
    if(comp$type != 'customizedInput'){
      comp$inputId
    }
  })
  
  ids = unlist(inputId)
  g = globalenv()
  ip = sapply(ids, function(id){
    get0(id, envir = g, inherits = F)
  })
  
  c(ip, localenv$reactive_inputs)
}
class(get_inputs) = c('reactive_dev', 'function')
makeActiveBinding('input', get_inputs, toolbox)





`[[<-.reactive_output_dev` = `$<-.reactive_output_dev` = function(x, k, value){
  tmp = localenv$reactive_outputs[[k]]
  if(is.null(tmp)){
    tmp = ''
    class(tmp) = 'msg'
    msg = ''
  }else{
    msg = attr(tmp, 'msg')
  }
  if(length(msg) < 2){
    msg = c(msg, '')
  }
  msg[2] = paste('Render: ', paste(capture.output(print(value)), collapse = '\n\t'))
  
  attr(tmp, 'msg') = msg
  localenv$reactive_outputs[[k]] = tmp
  return(invisible(get_outputs))
}


get_outputs = function(){
  localenv$reactive_outputs
}
class(get_outputs) = c('reactive_output_dev', 'reactive_dev', 'function')
makeActiveBinding('output', get_outputs, toolbox)

getDefaultReactiveInput = function(){
  'Usage: input <- getDefaultReactiveInput()
    This is not often used. Actually when launching the package, an reactive
    input object has been created and ready to use. So, use "input" directly!

    Example 1:
          observeEvent(input$ELECTRODE, {
            print("Electrode is changed!")
          })

    Example 2:
          # A customized UI, register outputId "customized_output" in
          # config.yaml - outputs and specify type "customizedOutput"
          customized_output <- function(){
            plotOutput(ns("plot_id"))
          }

          # Render it in `__reactives__`
          output$plot_id <- renderPlot({
            # Plot something here
            plot(1:10)
          })
    '
  
  toolbox$input
}


getDefaultReactiveOutput = function(){
  'Usage: output <- getDefaultReactiveOutput()
    This is not often used. Actually when launching the package, an reactive
    output object has been created and ready to use. So, use "output" directly!

    Example:
          # A customized UI, register outputId "customized_output" in
          # config.yaml - outputs and specify type "customizedOutput"
          customized_output <- function(){
            plotOutput(ns("plot_id"))
          }

          # Render it in `__reactives__`
          output$plot_id <- renderPlot({
            # Plot something here
            plot(1:10)
          })
    '
  
  toolbox$output
}




ns = function(id){
  'Definition: ns(id)
    Used in customized UIs and wrap up input/output ids so that different
    modules won\'t interrupt each others.

    Usage:
          # Example as inputs. Here we use numeric input as an example
          # See ?numericInput for more example
          numericInput(ns("my_age"), "Enter a Number:", value = 20)

    WARNING:
          1. Any input components defined in comfig.yaml is automatically
          wrapped up by ns(.)
          2. When updating inputs, just use its raw IDs. For this example,
          if you want to update input "my_age" defined in the `usage` above
          to 30, just use:
              updateNumericInput(session, "my_age", value = 30)



    See alse < http://shiny.rstudio.com/articles/modules.html >
    '
  
  shiny::NS(package)(id)
}



rave_updates = function(..., .env = globalenv()){
  rave::logger('Updating inputs')
  res = rlang::quos(...)
  nms = names(res)
  if(length(nms) == 0){
    return()
  }
  lapply(res[nms == ''], function(quo){
    rave::eval_dirty(quo, env = .env)
  })
  
  nms = nms[nms != '']
  
  for(nm in nms){
    val = rave::eval_dirty(res[[nm]], env = .env)
    try({
      re = val$value
      re %?<-% val$selected
      .env[[nm]] = re
    })
  }
  
  invisible(res)
}
