#' Check dependencies and update them at start up (Highly recommended)
#' @param file file to check package update
#' @export
check_updates <- function(file){
  if(missing(file)){
    file = '~/rave_modules/packages.txt'
  }
  if(!file.exists(file)){
    file = system.file('packages.txt', package = 'rave')
  }
  s = unique(c(readLines(file), readLines(system.file('packages.txt', package = 'rave'))))
  s = stringr::str_trim(s)
  s = s[!stringr::str_detect(s, '^#') & s!='']
  lapply(s, function(ss){
    info = stringr::str_trim(as.vector(stringr::str_split(ss, ',', simplify = T)))
    if(length(info) == 4){
      pkg = info[1]
      ver = info[2]
      src = info[3]
      details = info[4]

      needs_install = !package_version_check(pkg, version = ver)

      if(needs_install){
        switch (src,
          'cran' = {
            install.packages(pkg)
          },
          'github' = {
            devtools::install_github(details)
          },
          'bioc' = {
            source("https://bioconductor.org/biocLite.R")
            do.call('biocLite', list(pkg, suppressUpdates = T))
          }
        )
      }
    }
  })
}

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
        crayon_enabled = TRUE,
        rave_ver = new_ver
      )

    }else{
      has_data = arrange_data_dir(F)
    }

    # try({
    #   check_updates_onstartup = rave_options('check_updates_onstartup')
    #   check_updates_onstartup %?<-% T
    #   if(check_updates_onstartup){
    #     suppressMessages({
    #       check_updates()
    #     })
    #   }
    # }, silent = T)

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















