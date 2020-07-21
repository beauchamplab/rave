
guess_raw_trace <- function(dat, electrodes = NULL, matrix = FALSE){
  nms = names(dat)
  for(nm in nms){
    x <- dat[[nm]]
    if(!is.numeric(x) || mode(x) != "numeric"){ next }
    
    if(matrix){
      if(!is.matrix(x)){ next }
      dm <- dim(x)
      d1 <- min(dm)
      
      # d2 is the time points, d1 should be electrodes
      if(d1 < length(electrodes)){ next }
      return(nm)
    } else {
      # should be vector
      dm <- dim(x)
      if(is.null(dm)){
        return(nm)
      } else if (length(dm) %in% c(1,2)){
        if(min(dm) == 1){
          return(nm)
        }
      }
    }
    
  }
}
