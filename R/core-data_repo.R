#' Function to check if data repository has data
#' @param rave_data internally used
#' @export
any_subject_loaded <- function(rave_data = getDefaultDataRepository()){
  # Right now this function only has simple checks
  
  has_data = FALSE
  
  if(all(c("data_check", "module_tools",
           "preload_info", "subject") %in% names(rave_data))){
    has_data = TRUE
  }
  
  return(has_data)
}
