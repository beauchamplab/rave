% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-parallel.R
\name{lapply_async}
\alias{lapply_async}
\alias{lapply_async3}
\title{\code{lapply} using future package in asynchronous way}
\usage{
lapply_async(
  x,
  fun,
  ...,
  .ncores = 0,
  .call_back = NULL,
  .packages = NULL,
  .envir = environment(),
  .globals = TRUE,
  .gc = TRUE,
  .as_datatable = FALSE,
  .nrows = 0
)

lapply_async3(
  x,
  fun,
  ...,
  .globals = TRUE,
  .gc = TRUE,
  .callback = NULL,
  .ncores = 0
)
}
\arguments{
\item{x, fun, ...}{(See \code{\link{lapply}})}

\item{.ncores}{Number of cores to use. If the value is 0, the number of cores
will be determined by rave_options('max_worker').}

\item{.call_back}{A function takes current iteration number as argument, can be NULL.}

\item{.packages}{NULL be default, then the function will detect attached packages
automatically. Otherwise you have to specify the packages that you want to load.}

\item{.envir}{internally used}

\item{.globals}{Automatically detect variables. See ?future::future}

\item{.gc}{Clean up environment after each iterations? Recommended for large datasets.}

\item{.as_datatable}{logical, return result as \code{data.frame}. Experimental.}

\item{.nrows}{integer, if \code{.as_datatable=TRUE}, number of rows expected.}

\item{.callback}{function or \code{NULL}, callback function to monitor updates.}
}
\description{
\code{lapply} using future package in asynchronous way
}
\examples{
\dontrun{
lapply_async(1:10, function(x){
  Sys.sleep(2) # Run for 1 secs
  Sys.getpid()
}, .ncores = 3, .call_back = function(i){
  cat('Running iteration -', i, '\n')
})
}
}
