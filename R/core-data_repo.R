#' Function to check if data repository has data
#' @export
any_subject_loaded <- function(){
  # Right now this function only has simple checks
  
  has_data = FALSE
  
  rave_data = getDefaultDataRepository()
  if(all(c("data_check", "module_tools",
           "preload_info", "subject") %in% names(rave_data))){
    has_data = TRUE
  }
  
  rm(rave_data)
  return(has_data)
}
