#' Color console for RAVE
#' @param ... things to print
#' @param level logger level: DEBUG, INFO, WARNING, ERROR, FATAL
#' @param quiet suppress logger info
#' @export
logger <- function(..., level = 'DEBUG', quiet = FALSE){
  if(quiet){
    return(invisible())
  }
  tryCatch({
    cat2(..., level = level)
  }, error = function(e){
    stop(shiny::safeError('FATAL error found. Process terminated.'))
  })

}
