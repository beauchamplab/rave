
#' Get RAVE version
#' @export
rave_version <- function(){
  as.character(utils::packageVersion('rave'))
}


.onLoad <- function(libname, pkgname){
  
  try({
    # get rave_version
    old_ver = rave_options('rave_ver')
    old_ver %?<-% rave_hist$get_or_save('..rave_ver..', '0.0.0.0000')
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
      packageStartupMessage(sprintf('RAVE Updated from %s ==> %s. Updating module information...', old_ver, new_ver))
      # New RAVE installed! update
      
      # 3. Data files
      has_data = arrange_data_dir(TRUE)
      
      
      rave_hist$save('..rave_ver..' = new_ver)
      
      # 1. additional settings
      rave_options(
        delay_input = 20,
        max_worker = future::availableCores() - 1,
        # crayon_enabled = TRUE,
        rave_ver = new_ver
      )
      
      try({
        if(!isTRUE(rave_options('disable_startup_speed_check'))){
          remotes::install_github('dipterix/dipsaus', upgrade = FALSE, force = FALSE)
          remotes::install_github('dipterix/rutabaga@develop', upgrade = FALSE, force = FALSE)
          remotes::install_github('dipterix/threeBrain', upgrade = FALSE, force = FALSE)
          remotes::install_github('beauchamplab/ravebuiltins@migrate2', upgrade = FALSE, force = FALSE)
        }
      }, silent = TRUE)
      
    }else{
      has_data = arrange_data_dir(F)
    }
    
    save_options()
    
  })
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
  
}

