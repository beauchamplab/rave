## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE, collapse=FALSE-----------------------------------------
#  # Load the package
#  require(firstPkg)
#  
#  # Load RAVE dev toolbox
#  # It's called "dev_*", where * is your package name
#  dev_firstPkg(expose_functions = TRUE)
#  
#  # Load a demo subject
#  mount_demo_subject()
#  
#  # Preview layout
#  # view_layout(Your_module_id)
#  view_layout('firstPkg_first_example')

## ---- eval=FALSE, collapse=FALSE-----------------------------------------
#  #' @title Handles results and print which electrodes selected
#  #' @export
#  text_result <- function(result){
#    text_electrode = result$get_value('text_electrode')
#    cat("Electrode Selected: ", text_electrode, sep = '')
#  }

## ---- eval=FALSE, collapse=FALSE-----------------------------------------
#  view_layout('firstPkg_first_example')

