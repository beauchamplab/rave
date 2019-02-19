#' @include utils_syscheck.R
NULL

#' @import utils
#' @import stats
#' @import R6
#' @import htmltools
#' @import shiny
#' @import yaml
#' @import stringr
#' @import graphics
#' @import grDevices
#'
#' @importFrom grid grid.newpage
#'
#' @importFrom crayon make_style
#'
#' @importFrom Matrix %&%
#'
#' @importFrom DT formatRound
#' @importFrom DT datatable
#'
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang quo_squash
#' @importFrom rlang eval_tidy
#' @importFrom rlang :=
#' @importFrom rlang is_quosure
#' @importFrom rlang as_quosure
#' @importFrom rlang fn_body
#' @importFrom digest digest
#'
#' @importFrom future plan
#' @importFrom future future
#' @importFrom future value
#' @importFrom future values
#' @importFrom future multisession
#' @importFrom future multiprocess
#' @importFrom future futureAssign
#' @importFrom future availableCores
#' @importFrom future cluster
#' @importFrom future resolved
#'
#' @importFrom hdf5r is_hdf5
#' @importFrom hdf5r H5File
#'
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabBox
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#'
#' @importFrom methods is
#' @importFrom methods setMethod
#' @importFrom methods signature
#'
#' @importFrom assertthat assert_that
NULL


#' Internally use to auto update RAVE package version
#' DO NOT use it, it's copied from websites and I'll probably remove this function in the future
#' @param packageLocation To be documented
updatePackageVersion <- function(packageLocation ="."){
  ## Read DESCRIPTION file
  desc <- readLines(file.path(packageLocation, "DESCRIPTION"))

  ## Find the line where the version is defined
  vLine <- grep("^Version\\:", desc)

  ## Extract version number
  vNumber <- gsub("^Version\\:\\s*", "", desc[vLine])
  ## Split the version number into two; a piece to keep, a piece to increment
  versionNumber <- strsplit(vNumber, "\\.")[[1]]
  versionParts <- length(versionNumber)
  vNumberKeep <- paste(versionNumber[1:(versionParts-1)], sep= "", collapse= ".")
  vNumberUpdate <- versionNumber[versionParts]

  ## Replace old version number with new one (increment by 1)
  oldVersion <- as.numeric(vNumberUpdate)
  newVersion <- oldVersion + 1

  ## Build final version number
  vFinal <- paste(vNumberKeep, stringr::str_pad(newVersion, 4, 'left', '0'), sep = ".")

  ## Update DESCRIPTION file (in R)
  desc[vLine] <- paste0("Version: ", vFinal )


  ## Update Data
  vLine <- grep("^Date\\:", desc)
  desc[vLine] = sprintf("Date: %s", Sys.Date())


  ## Update the actual DESCRIPTION file
  writeLines(desc, file.path(packageLocation, "DESCRIPTION"))

  ## Return the updated version number to screen
  return(vFinal)
}



### Stores internal settings (session-based)
.conf_env <- new.env(parent = emptyenv())

get_conf <- function(key, default = NULL){
  if(exists(key, envir = .conf_env)){
    return(.conf_env[[key]])
  }else{
    default
  }
}

set_conf <- function(key, val, remove_if_null = TRUE){
  if(remove_if_null && (missing(val) || is.null(val))){
    rm(list = key, envir = .conf_env, inherits = FALSE)
  }else{
    .conf_env[[key]] = val
  }
}
