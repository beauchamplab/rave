
#' @title Start 'RAVE' main application
#' @name start_rave
#' @param host host IP address; default is \code{"127.0.0.1"}
#' @param port integer port number; default is random
#' @param launch.browser whether to launch browser; default is true
#' @param jupyter whether to launch the 'Jupyter' server; default is false
#' @param as_job whether to launch in the background as an 'RStudio' job;
#' available only in 'RStudio'
#' @param ... passed to \code{\link[ravedash]{start_session}}
#' @returns A 'shiny' application object (invisibly when launched as a job).
#' @seealso \code{\link[ravedash]{start_session}}
#' 
#' @examples
#' 
#' if (interactive()) {
#'  
#' start_rave()
#' 
#' }
#' 
#' 
#' @export
start_rave2 <- function(host = "127.0.0.1", port = NULL, launch.browser = TRUE,
                        jupyter = FALSE, as_job = FALSE, ...) {
  ravedash::start_session(..., host = host, port = port, jupyter = jupyter,
                          as_job = as_job, launch_browser = launch.browser)
}

#' @rdname start_rave
#' @export
start_rave <- start_rave2

#' @name start_yael
#' @title Start 'YAEL' electrode localization
#' @param host host IP address
#' @param port integer port number; default is random
#' @param launch.browser whether to launch browsers
#' @param as_job whether to launch in background; available only in 'RStudio'
#' @param ... passed to \code{\link[ravedash]{start_session}}
#' @returns A 'shiny' application object (invisibly when launched as a job).
#' 
#' @examples
#' 
#' if (interactive()) {
#'  
#' start_yael()
#' 
#' }
#' 
#' @export
start_yael <- function(host = "127.0.0.1", port = NULL, launch.browser = TRUE,
                       as_job = FALSE, ...) {
  modules <- c(
    "yael_preprocess",
    "electrode_localization",
    "custom_3d_viewer",
    "configure_rave"
  )
  page_title <- c("YAEL", sprintf("YAEL (%s)", 
                                  utils::packageVersion("threeBrain")))
  ravedash::start_session(..., host = host, port = port, jupyter = FALSE,
                          as_job = as_job, launch_browser = launch.browser,
                          modules = modules, page_title = page_title)
}
