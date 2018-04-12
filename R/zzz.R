# life cycle
#' @export
rave_life_cycle <- function(topic = 'general'){

}



#' @name rave_life_cycle_general
#' @title RAVE development cycle - General
#' @author Zhengjia Wang
#'
#' There are
#' \link[rave]{rave_life_cycle_util}
NULL

#' @name rave_life_cycle_python
#' @title RAVE development cycle - Python Support
#' @author Zhengjia Wang
#'
#' @section Part 1 - Install Python:
#' \enumerate{
#'   \item \code{\link{py_console}}
#' }
NULL

#' @name rave_life_cycle_signal
#' @title RAVE development cycle - Signal Processing
#' @author Zhengjia Wang
#'
#' @section Part 1 - Transformations:
#' \enumerate{
#'   \item \code{\link{notch_filter}} Matlab-compatible notch filter
#'   \item \code{\link{wavelet}} Wavelet function compatible with Matlab Fieldtrip package
#' }
#'
#' @section Part 2 - Visualizations:
#' \enumerate{
#'   \item \code{\link{diagnose_signal}} Inspect signal(s) with four plots: Time series, 2 x Welch periodogram, Histogram
#'   \item \code{\link{pwelch}} Welch periodogram for signals
#'   \item \code{\link{plot_signals}} Plot and compare signals line by line
#' }
NULL



#' @name rave_life_cycle_util
#' @title RAVE development cycle - Utils
#' @author Zhengjia Wang
#'
#' @section Part 1 - Operators:
#' \enumerate{
#'   \item \code{\link{\%&\%}} Function to concatenate two strings
#'   \item \code{\link{\%within\%}} Also see \code{\link{is_within}}, check if x is within a range
#'   \item \code{\link[rave:grapes-help-set-grapes]{\%?<-\%}} Assign value to variable if that variable is \code{NULL} or doesn't exist
#' }
#'
#' @section Part 2 - Loops, make life easier:
#' \enumerate{
#'   \item \code{\link{lapply_async}} Apply each functions using future package (async version, parallel)
#'   \item \code{\link{lapply_expr}} Apply and evaluate each expressions within each applied elements
#' }
#'
#' @section Part 3 - Non-standard evaluation, and Evnironment operation:
#' \enumerate{
#'   \item \code{\link{clear_env}} Remove all variables within environment
#'   \item \code{\link{eval_dirty}} Naive evaluate expressions with side effects, also see \code{\link[rlang]{eval_tidy}}
#'   \item \code{\link{eval_within}} Evaluate function as if it's run within specified environment
#' }
#'
#' @section Part 4 - File IO, cache, etc:
#' \enumerate{
#'   \item \code{\link{load_h5}} Load HDF5 file via rhdf5 package with lazy mode
#'   \item \code{\link{save_h5}} Save data to HDF5 file with no validity checking
#'   \item \code{\link{cache}} Cache any R objects in case to save calculation time
#'   \item \code{\link{clear_cache}} Clear cache environment
#' }
#'
#' @section Part 5 - Validity checks, or others:
#' \enumerate{
#'   \item \code{\link{get_val}} Similar to \%?<-\%, get key from list, returns default values if the key is invalid
#'   \item \code{\link{is_invalid}} Check validity of data
#'   \item \code{\link{is.blank}} Check if a string is "" - blank
#'   \item \code{\link{zero_length}} Check if argument(s) has zero-length
#'   \item \code{\link{safe_str_c}} Try to make any object to string
#'   \item \code{\link{try_normalizePath}} Convert relative path to absolute path
#' }
#'
NULL
