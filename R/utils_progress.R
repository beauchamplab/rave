#' A wrapper for shiny Progress object
#' @details shiny::Progress class cannot be used under non-reactive environment.
#' rave::progress function wrap it up so that you can use it in non-reactive settings.
#' @param title Main title for progress bar
#' @param max How many steps you have for this process
#' @param session shiny session, default is getDefaultReactiveDomain()
#' @param quiet nonreactive-only mode? default is FALSE. If TRUE, then progress bar will be hidden in shiny app
#' @param ... other parameters passing to \code{dipsaus::progress2}
#' @examples
#' \dontrun{
#' # case 1: non-reactive settings
#' p = progress('This is title', max = 10)
#' for(i in 1:10) p$inc(i)
#'
#' # case 2: shiny app
#' shinyApp(ui = fluidPage(actionLink('click', 'Click Me')),
#'   server = function(input, output, session){
#'     observeEvent(input$click, {
#'     p = progress('This is Title', max = 10)
#'     on.exit({p$close()})
#'     for(i in 1:10) { p$inc(i); Sys.sleep(0.3); }
#'   })
#' })
#' }
#' @export
progress <- function(
  title, max = 1,
  session = getDefaultReactiveDomain(),
  quiet = FALSE, ...
){
  
  return(dipsaus::progress2(title = title, max = max, session = session, quiet = quiet, ...))
  
  env = environment()
  if(is.null(title)){
    title = ''
  }

  is_shiny_session = local({
    if(is.null(session)){
      return(FALSE)
    }else{
      if(any(
        is(session, 'ShinySession'),
        is(session, 'session_proxy'),
        is(session, 'R6')
      )){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }
  })

  if(quiet || !is_shiny_session){
    progress = NULL
    inc = function(message, amount = 1, ...){if(!quiet) logger(message)}
    close = function(){}
    reset = function(message = NULL, detail = NULL){}
  }else{
    progress = Progress$new(
      session = session,
      max = max
    )
    # progress$set(value = 0, message = title)
    inc = function(message, amount = 1, ...){
      progress$inc(amount = amount, detail = message, message = title)
    }
    close = function(){
      progress$close()
    }
    reset = function(message = NULL, detail = NULL){
      if(is.null(message)){
        env$title = ''
      }else{
        env$title = message
      }
      if(is.null(detail)){
        detail = ''
      }
      progress$set(value = 0, message = env$title, detail = detail)
    }
  }

  return(list(
    .progress = progress,
    inc = inc,
    close = close,
    reset = reset
  ))
}

