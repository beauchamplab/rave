#' Check dependencies and update them at start up (Highly recommended)
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

      tbl = installed.packages()
      which = tbl[,1] == pkg
      needs_install = F
      if(sum(which)){
        row = as.list(tbl[which, ])
        if(pkg != 'rave' && utils::compareVersion(row$Version, ver) < 0){
          needs_install = T
        }
      }else{
        needs_install = T
      }

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
            biocLite(pkg, suppressUpdates = T)
          }
        )
      }
    }
  })
}

#' @export
rave_version <- function(){
  tbl = installed.packages()
  row = as.list(tbl[tbl[,1] == 'rave',])
  return(row$Version)
}




.onAttach <- function(libname, pkgname){

  try({
    shiny::registerInputHandler("rave.compoundInput", function(data, shinysession, name) {
      if (is.null(data)){
        return(NULL)
      }

      # restoreInput(id = , NULL)
      meta = as.list(data$meta)
      timeStamp = as.character(data$timeStamp)
      maxcomp = as.integer(data$maxcomp)
      inputId = as.character(data$inputId)
      value =  data$val

      ids = names(meta)
      ncomp = as.integer(data$ncomp)
      if(length(ids) == 0 || is.null(ncomp) || ncomp <= 0){
        return(NULL)
      }

      # nvalid = length(dropInvalid(value, deep = T))
      # nvalid = max(1, nvalid)
      # nvalid = min(nvalid, ncomp)

      re = lapply(value, function(val){
        sapply(val, function(v){
          tryCatch({
            jsonlite::fromJSON(v)
          }, error = function(e){
            NULL
          })
        }, simplify = F, USE.NAMES = T)
      })

      attr(re, 'ncomp') <- ncomp
      attr(re, 'meta') <- meta
      attr(re, 'timeStamp') <- timeStamp
      attr(re, 'maxcomp') <- maxcomp
      return(re)

    }, force = TRUE)
  }, silent = T)

  try({
    # get rave_version
    old_ver = rave_options('rave_ver')
    old_ver %?<-% rave::rave_hist$get_or_save('..rave_ver..', '0.0.0.0000')
    new_ver = rave:::rave_version()
    if(utils::compareVersion(old_ver, new_ver) < 0){
      logger('RAVE Updated from ', old_ver, ' ==> ', new_ver)
      logger('Updating module information', level = 'INFO')
      # New RAVE installed! update

      # 2. Module Files
      rave::arrange_modules(T)

      # 3. Data files
      has_data = rave::arrange_data_dir(T)


      rave::rave_hist$save('..rave_ver..' = new_ver)

      # 1. additional settings
      rave::rave_options(
        delay_input = 20,
        max_worker = future::availableCores() - 1,
        crayon_enabled = TRUE,
        rave_ver = new_ver
      )

    }else{
      rave::arrange_modules(F)
      has_data = rave::arrange_data_dir(F)
    }

    try({
      check_updates_onstartup = rave_options('check_updates_onstartup')
      check_updates_onstartup %?<-% T
      if(check_updates_onstartup){
        suppressMessages({
          check_updates()
        })
      }
    }, silent = T)

    rave::save_options()

    rave::rave_setup()

    if(has_data){
      rave::logger("RAVE - (Code: Ent) is loaded!", level = 'INFO')

      nms = list(
        'Module File:        \t' =  rave::rave_options('module_lookup_file'),
        'Data Repository:    \t' = rave::rave_options('data_dir'),
        'Raw-data Repository:\t' = rave::rave_options('raw_data_dir')
      )

      for(nm in names(nms)){
        rave::logger(nm, nms[[nm]], level = 'INFO')
      }

      rave::logger("Type 'rave_options(launch_gui = T)' or '?rave_options' for details", level = 'INFO')
    }else{
      logger('An error or more occur during loading process', level = 'ERROR')
    }

  })
}















