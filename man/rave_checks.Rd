% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{rave_checks}
\alias{rave_checks}
\title{Check if data is loaded for current module}
\usage{
rave_checks(
  ...,
  data = NULL,
  .raise_error = TRUE,
  rave_data = getDefaultDataRepository()
)
}
\arguments{
\item{...}{see details}

\item{data}{same as \code{...}, but can be a vector}

\item{.raise_error}{whether to raise error if data is missing}

\item{rave_data}{internally used}
}
\description{
Check if data is loaded for current module
}
\details{
This function checks whether "ECoG" data is loaded. The format is: 
\code{"DATA+(blankspace)+TYPE"}. \code{"DATA"} can be "power" (wavelet 
transform amplitude), "phase" (complex angle), or "volt"/"voltage" (Before 
wavelet). \code{"TYPE"} can be "raw" (no reference), "referenced" 
(referenced by common average reference, white matter reference, or 
bipolar reference). For voltage data, there is one more special type 
"full" which loads voltage data for all electrodes.
}
