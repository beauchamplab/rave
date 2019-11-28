
verify_rstudio_version <- function(version_needed = '1.2'){
  if(!is.null(shiny::getDefaultReactiveDomain())){
    return(FALSE)
  }
  if(!requireNamespace('rstudioapi')){
    return(FALSE)
  }
  tryCatch({
    rstudioapi::verifyAvailable(version_needed = version_needed)
    TRUE
  }, error = function(e){
    FALSE
  })
}

rstudio_viewer <- function(..., default = TRUE){
  if(verify_rstudio_version()){
    rstudioapi::viewer(...)
  }else{
    return(default)
  }
}


select_path <- function(is_directory = TRUE){
  if(verify_rstudio_version()){
    if(is_directory){
      path = rstudioapi::selectDirectory()
    }else{
      path = rstudioapi::selectFile()
    }
    warning("Please fix the path in your script!!!\n\t", path)
    return(path)
  }else{
    stop("Cannot find file path. Please contact package owner to fix it.")
  }
}

ask_question <- function(title, message, ok = 'Yes', cancel = 'No',
                         use_console = FALSE, level = 'WARNING'){
  if(!verify_rstudio_version()){
    use_console = TRUE
  }
  if(use_console){
    res = dipsaus::ask_yesno(title, '\n  - ', message)
    if(is.null(res) || is.na(res)){
      stop('Well... Maybe next time enter "yes" or "no" :)')
    }
    return( res )
  }else{
    rstudioapi::showQuestion(
      title = title,
      message = message,
      ok = ok,
      cancel = cancel
    )
  }
  
}



save_all <- function(){
  if(verify_rstudio_version()){
    if (rstudioapi::hasFun("documentSaveAll")) {
      rstudioapi::documentSaveAll()
    }
  }
}


