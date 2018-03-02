progress <- function(
  title, max = 1,
  session = shiny::getDefaultReactiveDomain()
){
  env = environment()
  if(is.null(session)){
    # progress = txtProgressBar(title = title, initial = 0, max = max)
    #
    # val = 0
    # inc = function(message){
    #   env$val = env$val + 1
    #   setTxtProgressBar(progress, 0, label=message)
    #   setTxtProgressBar(progress, env$val, label = message)
    # }
    # close = function(){
    #   options(rave.logger.disabled = FALSE)
    # }
    progress = NULL
    inc = function(...){}
    close = function(){}
  }else{
    progress = Progress$new(
      session = session,
      max = max
    )
    inc = function(message){
      progress$inc(amount = 1, detail = message, message = title)
    }
    close = function(){
      progress$close()
    }
  }

  return(list(
    .progress = progress,
    inc = inc,
    close = close
  ))
}
