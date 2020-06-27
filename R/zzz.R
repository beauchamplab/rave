
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

.onUnload <- function(libpath){
  clear_env(data_repository)
}


restart_r <- function(){
  f <- get0(".rs.restartR")
  if (is.function(f)) {
    message("Restarting RStudio rsession. Might take a while. Please wait...")
    f()
    return(invisible())
  }
  message("Using startup::restart()")
  startup::restart()
  return(invisible())
}

#' @title Check and Install RAVE Dependencies
#' @param update_rave logical, whether to update RAVE
#' @param restart logical, whether to restart `RStudio` after installation
#' @param nightly logical, whether to install develop version
#' @param demo_data logical, whether to check demo data
#' @param ... for compatibility purpose, ignored
#' @export
check_dependencies <- function(update_rave = TRUE, restart = TRUE, 
                               nightly = FALSE, demo_data = FALSE, ...){
  
  # Check N27 brain
  dipsaus::cat2('Checking N27 brain', level = 'DEFAULT', end = '\n')
  threeBrain::merge_brain()
  dipsaus::cat2('   - Done', level = 'INFO', end = '\n')
  
  
  # Check demo subjects
  if( demo_data ){
    dipsaus::cat2('Checking RAVE data repository', level = 'DEFAULT', end = '\n')
    p = get_projects()
    if(!length(p)){
      has_demo = FALSE
      if('demo' %in% p){
        subs = get_subjects('demo')
        if(length(subs)){
          has_demo = TRUE
        }
      }
      if(!has_demo){
        if(interactive()){
          ans <- dipsaus::ask_yesno("There is no project found in data repository. Install demo subjects?")
        } else {
          ans <- TRUE
        }
        if(isTRUE(ans)){
          # install demo subjects
          download_sample_data('_group_data', replace_if_exists = TRUE)
          download_sample_data('KC')
          download_sample_data('YAB')
        }
      }
      dipsaus::cat2('   - Done', level = 'INFO', end = '\n')
    }
  }
  
  
  
  lazy_install <- NULL
  lazy_github <- NULL
  
  # check if Rcpp version is 1.0.4 and os is macosx
  if(stringr::str_detect(R.version$os, "^darwin")){
    rcpp_version <- utils::packageVersion('Rcpp')
    if(utils::compareVersion(as.character(rcpp_version), '1.0.4') == 0){
      # you need to install
      lazy_install <- c(lazy_install, 'Rcpp')
    }
  }
  
  if(nightly){
    lazy_install <- c(lazy_install, 'beauchamplab/ravebuiltins@migrate2', 'dipterix/rutabaga@develop')
    if(update_rave){
      lazy_install <- c(lazy_install, 'beauchamplab/rave@dev-1.1')
    }
    lazy_install <- c(lazy_install, c('dipterix/threeBrain', 'dipterix/dipsaus'))
  } else {
    lazy_install <- c(lazy_install, 'ravebuiltins', 'rutabaga')
    if(update_rave){
      lazy_install <- c(lazy_install, 'rave')
    }
    lazy_install <- c(lazy_install, c('threeBrain', 'dipterix/dipsaus'))
  }
  
  dipsaus::prepare_install2(unique(lazy_install), restart = FALSE)
  
  arrange_modules(refresh = TRUE)
  
  return(invisible())
  
}

check_dependencies2 <- function(){
  # 
  
  
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

