try_normalizePath <- function(path){
  if(file.exists(path)){
    return(normalizePath(path))
  }else{
    return(path)
  }
}
