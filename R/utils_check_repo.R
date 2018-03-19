check_data_repo <- function(var = c('subject', 'electrodes'), any = F){
  data_repo = getDefaultDataRepository()
  sel = var %in% names(as.list(data_repo, all.names = T))
  if(any && sum(sel)){
    return(TRUE)
  }
  if(!any && sum(!sel) == 0){
    return(TRUE)
  }

  return(FALSE)
}
