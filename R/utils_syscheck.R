check_updates <- function(file){
  if(missing(file)){
    file = '~/rave_modules/packages.txt'
  }
  if(!file.exists(file)){
    return(invisible())
  }
  s = readLines(file)
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
    check_updates_onstartup = rave_options('check_updates_onstartup')
    check_updates_onstartup %?<-% T
    if(check_updates_onstartup){
      check_updates()
    }
  }, silent = T)


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
    old_ver = options('rave_ver')[[1]]
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
      options('rave_ver' = new_ver)

      # 1. additional settings
      rave::rave_options(
        delay_input = 20,
        max_worker = future::availableCores() - 1,
        crayon_enabled = TRUE
      )

    }else{
      rave::arrange_modules(F)
      has_data = rave::arrange_data_dir(F)
    }

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

      rave::logger("Type '?rave_options' for details", level = 'INFO')
    }else{
      logger('An error or more occur during loading process', level = 'ERROR')
    }

  })
}








.onAttach_old <- function(libname, pkgname){

  try({

    # Check if rutabaga and threejsr is installed
    inst_pkg  = installed.packages()[,1]
    if('rave' %in% loadedNamespaces()){
      logger = function(...){ rave::logger(..., level = 'INFO') }
    }else{
      logger = message
    }

    if(!'threejsr' %in% inst_pkg){
      logger('Installing RAVE THREE JS widget - github - dipterix/threejsr')
      devtools::install_github('dipterix/threejsr')
    }
    if(!'rutabaga' %in% inst_pkg){
      logger('Installing RAVE Toolbox widget - github - dipterix/threejsr')
      devtools::install_github('dipterix/rutabaga')
    }



  }, silent = T)


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


    # ui_register_function('shiny::textInput', shiny::updateTextInput)
    # ui_register_function('shiny::selectInput', shiny::updateSelectInput, value_field = 'selected')
    # ui_register_function('shiny::numericInput', shiny::updateNumericInput)
    # ui_register_function('shiny::sliderInput', shiny::updateSliderInput)
    # ui_register_function('shiny::checkboxInput', shiny::updateCheckboxInput)
    # ui_register_function('shiny::textAreaInput', shiny::updateTextAreaInput)
    # ui_register_function('shiny::dateInput', shiny::updateDateInput)
    # ui_register_function('shiny::dateRangeInput', shiny::updateDateRangeInput)
    # ui_register_function('shiny::checkboxGroupInput', shiny::updateDateInput, value_field = 'selected')
    # ui_register_function('rave::compoundInput', rave:::updateCompoundInput, update_value = T)
    #
    # ui_register_function('shiny::plotOutput', shiny::renderPlot, default_args = list(res = 72))
    # ui_register_function('shiny::tableOutput', shiny::renderTable)
    # ui_register_function('shiny::verbatimTextOutput', shiny::renderPrint)
    # ui_register_function('shiny::dataTableOutput', shiny::renderDataTable)
    # ui_register_function('shiny::uiOutput', shiny::renderUI)
    # ui_register_function('shiny::imageOutput', shiny::renderImage, default_args = list(deleteFile = FALSE))
    # ui_register_function('shiny::htmlOutput', shiny::renderText)
    # ui_register_function('shiny::textOutput', shiny::renderText)





    last_ver = rave::rave_hist$get_or_save('..last_ver..')

    ver = as.character(utils::packageVersion('rave'))

    is_dev = exists('..rave__dev..', envir = globalenv(), inherits = F)
    is_new = is.null(last_ver) || utils::compareVersion(last_ver, as.character(ver)) < 0

    if(is_dev){
      # For dev:
      dev_log = c(
        '0. [Finished] Added version check',
        '1. [Finished] Remembers and restore the global inputs',
        '      a. [Finished] Cache inputs and overrides when re-init modules',
        '      b. [Finished] Fix updateCompountInput',
        '      c. [Finished] When switch to other modules, update globals',
        '            I.   [Finished] Re-write cache function',
        '            II.  [Finished] Expose fake cache_input',
        '            III. [Aborted] Inplement interactive inputs',
        '2. [Finished] On session ended, clean up memory.',
        '3a. [Finished] (buggy) Export as reports',
        '3b. SUMA export',
        '4. [Finished] Import modules as RMD files.',
        '5. Support data types: power? phase?',
        '6. [Partial] Do per elecctrodes'
      )
      fins = stringr::str_detect(dev_log, '\\[Finished\\]')
      parts = stringr::str_detect(dev_log, '\\[Partial\\]')
      abrts = stringr::str_detect(dev_log, '\\[Aborted\\]')
      dev_log[fins] = crayon::green(dev_log[fins])
      dev_log[parts] = crayon::bgYellow(dev_log[parts])
      dev_log[abrts] = crayon::silver(dev_log[abrts])
      dev_log[!(fins | parts | abrts)] = crayon::bgRed(dev_log[!(fins | parts | abrts)])
      cat('Dev-cycle - ', dev_log, '\nGithub - @rave-Dipterix', sep = '\n')

    }


    first_time = is_dev || is_new

    rave::arrange_modules(
      look_up_file = rave::rave_options('module_lookup_file'),
      target_dir = NULL,
      is_new = first_time
    ) ->
      is_changed_module

    rave::arrange_data_dir(is_dev = is_dev) ->
      is_changed_data


    # additional settings
    if(is_new){
      rave::rave_options(
        delay_input = 20,
        max_worker = parallel::detectCores() - 1,
        crayon_enabled = TRUE
      )
      rave::save_options()
    }

    capture.output(rave::rave_hist$save(`..last_ver..` = ver))





    rave::rave_setup()

    rave::logger("RAVE - (Code: Ent) is loaded!", level = 'INFO')

    if(is_changed_module || is_changed_data){
      message('--------------------')
      rave::logger("Your settings are changed, please check ... ", level = 'INFO')
    }

    nms = list(
      'Module File:        \t' =  rave::rave_options('module_lookup_file'),
      'Data Repository:    \t' = rave::rave_options('data_dir'),
      'Raw-data Repository:\t' = rave::rave_options('raw_data_dir')
    )

    for(nm in names(nms)){
      rave::logger(nm, nms[[nm]], level = 'INFO')
    }

    rave::logger("Type '?rave_options' for details", level = 'INFO')

  }, silent = T)

}






