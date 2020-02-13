
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
  
  template_subcode = rave_options('threeBrain_template_subject')
  template_rootdir = rave_options('threeBrain_template_dir')
  if( length(template_subcode) && template_subcode != '' && dir.exists(template_rootdir) ){
    options(
      `threeBrain.template_subject` = template_subcode,
      `threeBrain.template_dir` = template_rootdir
    )
  }

}

#' @title Check and Install RAVE Dependencies
#' @param update_rave logical, whether to update RAVE
#' @param restart logical, whether to restart `RStudio` after installation
#' @param skip numeric, to skip some updates
#' @export
check_dependencies <- function(update_rave = TRUE, restart = TRUE, skip = 0){
  threshold = 2
  
  cat2 <- dipsaus::cat2
  
  if( skip <= 0 ){
    cat2('Checking package rutabaga - Plot Helpers', level = 'INFO', end = '\n')
    if('rutabaga' %in% loadedNamespaces()){ devtools::unload('rutabaga', quiet = TRUE) }
    t1 = Sys.time()
    remotes::install_github('dipterix/rutabaga@develop', upgrade = FALSE, force = FALSE, quiet = TRUE)
    if(as.numeric(Sys.time() - t1, units = 'secs') < threshold){
      cat2('  - Already up to date', level = 'DEFAULT', end = '\n')
    }else{
      cat2('  - Updated', level = 'DEFAULT', end = '\n')
    }
  }
  
  if( skip <= 1 ){
    cat2('Checking package threeBrain - 3D Viewer', level = 'INFO', end = '\n')
    if('threeBrain' %in% loadedNamespaces()){ devtools::unload('threeBrain', quiet = TRUE) }
    t1 = Sys.time()
    remotes::install_github('dipterix/threeBrain', upgrade = FALSE, force = FALSE, quiet = TRUE)
    if(as.numeric(Sys.time() - t1, units = 'secs') < threshold){
      cat2('  - Already up to date', level = 'DEFAULT', end = '\n')
    }else{
      cat2('  - Updated', level = 'DEFAULT', end = '\n')
    }
  }
  
  if( skip <= 2 ){
    cat2('Checking package ravebuiltins - Default RAVE modules', level = 'INFO', end = '\n')
    if('ravebuiltins' %in% loadedNamespaces()){ devtools::unload('ravebuiltins', quiet = TRUE) }
    t1 = Sys.time()
    remotes::install_github('beauchamplab/ravebuiltins@migrate2', upgrade = FALSE, force = FALSE, quiet = TRUE)
    if(as.numeric(Sys.time() - t1, units = 'secs') < threshold){
      cat2('  - Already up to date', level = 'DEFAULT', end = '\n')
    }else{
      cat2('  - Updated', level = 'DEFAULT', end = '\n')
    }
  }
  
  if( skip <= 3 ){
    cat2('Checking package dipsaus - System Utils', level = 'INFO', end = '\n')
    if('dipsaus' %in% loadedNamespaces()){ devtools::unload('dipsaus', quiet = TRUE) }
    t1 = Sys.time()
    remotes::install_github('dipterix/dipsaus', upgrade = FALSE, force = FALSE, quiet = TRUE)
    if(as.numeric(Sys.time() - t1, units = 'secs') < threshold){
      cat2('  - Already up to date', level = 'DEFAULT', end = '\n')
    }else{
      cat2('  - Updated', level = 'DEFAULT', end = '\n')
    }
  }
  
  
  if( update_rave && skip <= 4 ){
    cat2('Update RAVE', level = 'DEFAULT', end = '\n')
    remotes::install_github('beauchamplab/rave', upgrade = FALSE, force = FALSE, quiet = TRUE)
    cat2('Finished. If R does not restart correctly, please manually restart the session \n\tGo to task bar -> Session > Restart R',
         level = 'INFO', end = '\n')
  }
  
  
  if(restart){
    rm(list = ls(envir = globalenv()), envir = globalenv())
    f = get0('.rs.restartR', envir = globalenv(), ifnotfound = NULL)
    if(is.function(f)){
      do.call(f, list('library("rave"); rave:::check_dependencies2()'))
      invisible()
    }
  }
  
  return(invisible())
  
}

check_dependencies2 <- function(){
  # 
  dipsaus::cat2('Checking N27 brain', level = 'INFO', end = '\n')
  threeBrain::merge_brain()
  
  dipsaus::cat2('Arranging all existing RAVE modules', level = 'INFO', end = '\n')
  arrange_modules(refresh = TRUE, reset = FALSE, quiet = TRUE)
  
  # check if any demo data exists
  dipsaus::cat2('Checking RAVE data repository', level = 'INFO', end = '\n')
  p = get_projects()
  if('demo' %in% p){
    subs = get_subjects('demo')
    if(length(subs)){
      return(invisible())
    }
  }
  
  
  # install demo subjects
  download_sample_data('_group_data', replace_if_exists = TRUE)
  download_sample_data('KC')
  download_sample_data('YAB')
  return(invisible())
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

