
#' Get RAVE version
#' @export
rave_version <- function(){
  as.character(utils::packageVersion('rave'))
}


.onAttach <- function(libname, pkgname){

  try({
    register_compoundInput()
  }, silent = T)

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
      logger('RAVE Updated from ', old_ver, ' ==> ', new_ver)
      logger('Updating module information', level = 'INFO')
      # New RAVE installed! update

      # 3. Data files
      has_data = arrange_data_dir(T)


      rave_hist$save('..rave_ver..' = new_ver)

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

    if(has_data){
      logger("RAVE - (Code: Fir) is loaded!", level = 'INFO')
      logger('Data Repository:    \t', rave_options('data_dir'), level = 'INFO')
      logger('Raw-data Repository:    \t', rave_options('raw_data_dir'), level = 'INFO')
      logger("Type 'rave_options(launch_gui = T)' or '?rave_options' for details", level = 'INFO')
    }else{
      logger('Cannot find RAVE repository! Please run the following command set them.', level = 'ERROR')
      logger('\trave::rave_options()', level = 'ERROR')
    }

  })
}













