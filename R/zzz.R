
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
      # has_data = arrange_data_dir(FALSE)
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
  catgl('Checking N27 brain', level = 'DEFAULT', end = '\n')
  threeBrain::merge_brain()
  catgl('   - Done', level = 'INFO', end = '\n')
  
  
  # Check demo subjects
  if( demo_data ){
    catgl('Checking RAVE data repository', level = 'DEFAULT', end = '\n')
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
      catgl('   - Done', level = 'INFO', end = '\n')
    }
  }
  
  lazy_install <- NULL
  lazy_github <- NULL
  
  if(nightly){
    # get repos from Github
    git_repos <- tryCatch({
      readLines("https://raw.githubusercontent.com/beauchamplab/rave/master/DEVREPO")[1:3]
    }, error = function(e){
      c("beauchamplab/rave", "beauchamplab/ravebuiltins@migrate2",  "dipterix/rutabaga@develop", 'dipterix/threeBrain', 'dipterix/dipsaus')
    })
    # lazy_install <- c(lazy_install, 'beauchamplab/ravebuiltins@migrate2', 'dipterix/rutabaga@develop')
    lazy_install <- c(lazy_install, git_repos[-1])
    if(update_rave){
      # get directly from DEVREPO
      lazy_install <- c(lazy_install, git_repos[1])
    }
  } else {
    lazy_install <- c(lazy_install, 'ravebuiltins', 'rutabaga')
    if(update_rave){
      lazy_install <- c(lazy_install, 'rave')
    }
    lazy_install <- c(lazy_install, c('threeBrain', 'dipterix/dipsaus'))
  }
  
  dipsaus::prepare_install2(unique(lazy_install), restart = FALSE)
  
  arrange_modules(refresh = TRUE)
  
  if( restart ){
    dipsaus::restart_session()
  }
  
  return(invisible())
  
}

check_dependencies2 <- function(){
  # 
  
  
  catgl('Arranging all existing RAVE modules', level = 'INFO', end = '\n')
  arrange_modules(refresh = TRUE, reset = FALSE, quiet = TRUE)
  
  # check if any demo data exists
  catgl('Checking RAVE data repository', level = 'INFO', end = '\n')
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
#' @param upgrade whether to ask. Default is 'always' to receive default 
#' settings Other choices are 'ask' or 'never'.
#' @param async whether to run scripts in parallel; default is true.
#' 
#' @export
finalize_installation <- function(packages, upgrade = c('always', 'ask', 'never'), async = TRUE){
  upgrade <- match.arg(upgrade)
  
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
  lib_path = .libPaths()
  
  allpackages = unlist(sapply(lib_path, function(lp){
    list.dirs(lp, recursive = FALSE, full.names = FALSE)
  }, simplify = FALSE))
  allpackages = unique(allpackages)
  
  yaml_path = sapply(allpackages, function(p){
    system.file('rave.yaml', package = p)
  })
  sel = yaml_path != ''
  
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
          if('async' %in% names(formals(fun))){
            fun(upgrade, async)
          } else {
            fun(upgrade)
          }
        }
      }
      
    }, error = function(e){
      catgl('Error found while finalize installation of [', pkg, ']. Reason:\n',
                    e$message, '\nSkipping...\n', level = 'WARNING')
    })
    
  }
  
  catgl('Scheduled. There might be some job running in the background. Please wait for them to finish.')
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

