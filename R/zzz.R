
#' Get RAVE version
#' @export
rave_version <- function(){
  as.character(utils::packageVersion('rave'))
}


.onLoad <- function(libname, pkgname){
  
  try({
    # get rave_version
    old_ver = rave_options('rave_ver')
    old_ver %?<-% rave_hist()$get_or_save('..rave_ver..', '0.0.0.0000')
    new_ver = rave_version()
    is_newer = tryCatch({
      is_newer = utils::compareVersion(old_ver, new_ver) < 0
      if(!length(is_newer) || !is.logical(is_newer)){
        is_newer = TRUE
      }
      is_newer
    }, error = function(e){
      return(TRUE)
    })
    if(is_newer){
      rave_hist()$save(
        '..rave_startup_msg..' = 
          sprintf('RAVE %s ==> %s. Module information has been updated.',
                  old_ver, new_ver)
      )
      # New RAVE installed! update
      
      # 3. Data files
      has_data = arrange_data_dir(TRUE)
      
      
      rave_hist()$save('..rave_ver..' = new_ver)
      
      # 1. additional settings
      rave_options(
        delay_input = 20,
        max_worker = future::availableCores() - 1,
        # crayon_enabled = TRUE,
        rave_ver = new_ver
      )
      
    }else{
      has_data = arrange_data_dir(F)
    }
    
    save_options()
    
  })
}

#' @title Check and Install RAVE Dependencies
#' @export
check_dependencies <- function(){
  cat2('Check package rutabaga - Plot Helpers', level = 'DEFAULT')
  remotes::install_github('dipterix/rutabaga@develop', upgrade = FALSE, force = FALSE, quiet = TRUE)
  cat2('Check package threeBrain - 3D Viewer', level = 'DEFAULT')
  remotes::install_github('dipterix/threeBrain', upgrade = FALSE, force = FALSE, quiet = TRUE)
  cat2('Check package ravebuiltins - Default RAVE modules', level = 'DEFAULT')
  remotes::install_github('beauchamplab/ravebuiltins@migrate2', upgrade = FALSE, force = FALSE, quiet = TRUE)
  cat2('Check package dipsaus - System Utils', level = 'DEFAULT')
  remotes::install_github('dipterix/dipsaus', upgrade = FALSE, force = FALSE, quiet = TRUE)
  cat2('Check update RAVE', level = 'DEFAULT')
  remotes::install_github('beauchamplab/rave', upgrade = FALSE, force = FALSE, quiet = TRUE)
  cat2('Finished. Please restart session by \n\tRStudio > Session > Restart R',
       level = 'INFO')
  rm(list = ls(envir = globalenv()), envir = globalenv())
  f = get0('.rs.restartR', envir = globalenv(), ifnotfound = NULL)
  if(is.function(f)){
    do.call(f, list('library("rave")'))
  }
  invisible()
}



.onAttach <- function(libname, pkgname){
  try({
    if( arrange_data_dir(FALSE) ){
      packageStartupMessage(sprintf(paste(
        "RAVE is loaded! - %s",
        "Data Repository:     \t%s",
        "Raw-data Repository: \t%s",
        "\nTo set option, type %s.",
        sep = '\n'
      ),
      rave_version(), rave_options('data_dir'), rave_options('raw_data_dir'),
      sQuote('rave_options(launch_gui=TRUE)')
      ))
    }else{
      packageStartupMessage('[WARNING]: Cannot find RAVE repository! Please run the following command set them.\n\trave::rave_options()')
    }
    
  }, silent = TRUE)
  
  try({
    startup_msg = rave_hist()$get_or_save('..rave_startup_msg..')
    if(interactive() && !is.null(startup_msg)){
      rave_hist()$save('..rave_startup_msg..' = NULL)
      packageStartupMessage(startup_msg)
      packageStartupMessage('Please run ', sQuote("rave::check_dependencies()"), " to check dependencies.")
    }
  }, silent = TRUE)
  
  
}

