
#' Get RAVE version
#' @export
rave_version <- function(){
  as.character(utils::packageVersion('rave'))
}

latest_version <- function() {
  tryCatch({
    suppressWarnings({
      versions <- raveio::load_json("https://beauchamplab.r-universe.dev/packages/rave")
      return(list(
        version = versions$Version[[1]],
        built = versions$Built$Date[[1]]
      ))
    })
  }, error = function(e){
    NULL
  })
}


.onLoad <- function(libname, pkgname){
  
  try({
    # get rave_version
    old_ver <- rave_options('rave_ver')
    old_ver %?<-% rave_hist()$get_or_save('..rave_ver..', '0.0.0.0000')
    new_ver <- rave_version()
    is_newer <- tryCatch({
      is_newer <- utils::compareVersion(old_ver, new_ver) < 0
      if(!length(is_newer) || !is.logical(is_newer)){
        is_newer <- TRUE
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
      has_data <- arrange_data_dir(TRUE)
      
      
      rave_hist()$save('..rave_ver..' = new_ver)
      
      # 1. additional settings
      rave_options(
        delay_input = 20,
        max_worker = future::availableCores() - 1,
        # crayon_enabled = TRUE,
        rave_ver = new_ver
      )
      
    }else{
      # has_data = arrange_data_dir(FALSE)
    }
    
    save_options()
    
  })
  

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
  
  if(!dipsaus::package_installed("ravemanager")) {
    utils::install.packages("ravemanager", repos = c(
      beauchamplab = "https://beauchamplab.r-universe.dev", 
      dipterix = "https://dipterix.r-universe.dev", 
      CRAN = "https://cloud.r-project.org"))
  }
  
  ravemanager <- asNamespace("ravemanager")
  ravemanager$install(upgrade_manager = FALSE)
  
  rave <- asNamespace("rave")
  rave$arrange_modules(refresh = TRUE)
  
  if( restart ){
    f <- get0(".rs.restartR", ifnotfound = NULL)
    if(is.function(f)) {
      f()
    }
  }
  
  return(invisible())
  
}

check_dependencies2 <- function(){

  catgl('Arranging all existing RAVE modules', level = 'INFO', end = '\n')
  arrange_modules(refresh = TRUE, reset = FALSE, quiet = TRUE)
  
  # check if any demo data exists
  catgl('Checking RAVE data repository', level = 'INFO', end = '\n')
  p <- get_projects()
  if('demo' %in% p){
    subs <- get_subjects('demo')
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


finalize_installation_internal_demo <- function(upgrade = c('ask', 'always', 'never')){
  upgrade <- match.arg(upgrade)
  # check demo data
  has_demo <- FALSE
  installed_subs <- NULL
  if('demo' %in% get_projects()){
    has_demo <- TRUE
    if( upgrade %in% c('never') ){ return() }
    
    installed_subs <- get_subjects('demo')
  }
  
  opt <- c('None', 'Only missing demos', 'All', 'First two subjects')
  
  missing_subs <- c('KC', 'YAB', 'YAD', 'YAF', 'YAH', 'YAI', 'YAJ', 'YAK')
  
  if( upgrade == 'always' ){
    missing_subs <- c('KC', 'YAB')
  }
  
  missing_subs <- missing_subs[!missing_subs %in% installed_subs]
  
  if( upgrade == 'ask' ){
    default_opt <- 1
    default_opt <- dipsaus::ask_or_default(
      'Choose an option to install demo subjects:\n',
      paste0(seq_along(opt), '. ', opt, collapse = '\n'),
      '\n\nEnter the option number',
      default = default_opt
    )
    default_opt <- dipsaus::parse_svec(default_opt)
    
    if(1 %in% default_opt || !any(seq_len(4) %in% default_opt)){
      # no data installed
      catgl('No demo data will be installed', level = 'DEFAULT')
      return()
    } else if(2 %in% default_opt){
    } else if(3 %in% default_opt){
      missing_subs <- c('KC', 'YAB', 'YAD', 'YAF', 'YAH', 'YAI', 'YAJ', 'YAK')
    } else {
      missing_subs <- c('KC', 'YAB')
    }
    catgl(paste(missing_subs, collapse = ', '), ' will be installed', level = 'DEFAULT')
    
  }
  
  if(length(missing_subs)){
    quo <- rlang::quo({
      dipsaus::rs_focus_console()
      for(sub in !!missing_subs){
        raveio::catgl('Launching download process - ', sub)
        rave::download_sample_data(sub)
      }
    })
    
    dipsaus::rs_exec(rlang::quo_squash(quo), quoted = TRUE, name = 'Download rave-base demo subjects')
  }
  
  return()
  
}

#' Finalize installation
#' @description download demo data
#' @param packages package name to finalize. 'rave' to only update base demo 
#' data, or \code{c('threeBrain', 'ravebuiltins')} to upgrade built-in data, 
#' or leave it blank to upgrade all.
#' @param upgrade whether to ask. Default is \code{'always'} to receive default 
#' settings. Other choices are \code{'ask'}, \code{'never'}, 
#' \code{'config-only'}, and \code{'data-only'}
#' @param async whether to run scripts in parallel; default is true.
#' 
#' @export
finalize_installation <- function(
  packages, upgrade = c('ask', 'config-only', 'always', 'never', 'data-only'), 
  async = FALSE){
  
  suppressWarnings({
    upgrade <- upgrade[upgrade %in% c('ask', 'config-only', 'always', 'never', 'data-only')]
    if(!length(upgrade)) {
      upgrade <- "ask"
    }
    
    if(missing(packages)){
      packages <- NULL
    }
    # 
    # if(!length(packages) || 'threeBrain' %in% packages){
    #   # Check N27 brain
    #   # To be backward compatible, we need to check threeBrain files
    #   tmp <- system.file('rave.yaml', package = 'threeBrain')
    #   if(tmp == ''){
    #     threeBrain::download_N27()
    #     threeBrain::merge_brain()
    #   }
    # }
    
    # Get all packages with rave.yaml
    lib_path <- .libPaths()
    
    allpackages <- unlist(sapply(lib_path, function(lp){
      list.dirs(lp, recursive = FALSE, full.names = FALSE)
    }, simplify = FALSE))
    allpackages <- unique(allpackages)
    
    yaml_path <- sapply(allpackages, function(p){
      system.file('rave.yaml', package = p)
    })
    sel <- yaml_path != ''
    
    if(length(packages)){
      sel <- sel & (allpackages %in% packages)
    }
    packages <- allpackages[sel]
    
    for(pkg in packages){
      tryCatch({
        
        # load yaml
        conf <- raveio::load_yaml(system.file('rave.yaml', package = pkg))
        
        fname <- conf$finalize_installation
        
        if(is.character(fname) && length(fname) == 1){
          ns <- asNamespace(pkg)
          fun <- ns[[fname]]
          if(is.function(fun)){
            fml <- formals(fun)
            upgrade1 <- tryCatch({
              dipsaus::`%OF%`(upgrade, eval(fml$upgrade))
            }, error = function(e) {
              upgrade
            })
            # print(upgrade1)
            if('async' %in% names(fml)){
              fun(upgrade = upgrade1, async)
            } else {
              fun(upgrade = upgrade1)
            }
          }
        }
        
      }, error = function(e){
        catgl('Error found while finalize installation of [', pkg, ']. Reason:\n',
              e$message, '\nSkipping...\n', level = 'WARNING')
      })
      
    }
    
    if( async ) {
      catgl('Scheduled. There might be some job running in the background. Please wait for them to finish.')
    } else {
      catgl("Finalize installation done!", level = "INFO")
      message("Finalize installation done! Please close all your R/RStudio sessions and restart.")
    }
  })
  
  invisible()
}


.onAttach <- function(libname, pkgname){
  try({
    if( arrange_data_dir(FALSE) ){
      
      current_version <- rave_version()
      latest_version <- latest_version()
      if(is.null(latest_version)) {
        latest_version <- "(Unable to obtain the update information)"
      } else {
        latest_version <- sprintf("%s (built: %s)", latest_version$version, latest_version$built)
      }
      
      packageStartupMessage(sprintf(paste(
        "RAVE is loaded!",
        "  Current version          - %s",
        "  Latest available version - %s",
        "Data Repository:     \t%s",
        "Raw-data Repository: \t%s",
        "\nTo set option, type %s",
        "To check for update, type %s.",
        sep = '\n'
      ),
      current_version, latest_version,
      rave_options('data_dir'), rave_options('raw_data_dir'),
      sQuote('rave::rave_options(launch_gui=TRUE)'),
      sQuote('ravemanager::version_info()')
      ))
    }else{
      packageStartupMessage('[WARNING]: Cannot find RAVE repository! Please run the following command set them.\n\trave::rave_options()')
    }
    
  }, silent = TRUE)
  
  try({
    startup_msg <- rave_hist()$get_or_save('..rave_startup_msg..')
    if(interactive() && !is.null(startup_msg)){
      rave_hist()$save('..rave_startup_msg..' = NULL)
      packageStartupMessage(startup_msg)
      packageStartupMessage('Please run ', sQuote("rave::check_dependencies()"), " to check dependencies.")
    }
  }, silent = TRUE)
  
  
}

