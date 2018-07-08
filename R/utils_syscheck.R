

.onAttach <- function(libname, pkgname){

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

      nvalid = length(dropInvalid(value, deep = T))
      nvalid = max(1, nvalid)
      nvalid = min(nvalid, ncomp)

      re = value[1:nvalid]

      attr(re, 'ncomp') <- ncomp
      attr(re, 'meta') <- meta
      attr(re, 'timeStamp') <- timeStamp
      attr(re, 'maxcomp') <- maxcomp
      return(re)

    }, force = TRUE)


    ui_register_function('shiny::textInput', shiny::updateTextInput)
    ui_register_function('shiny::selectInput', shiny::updateSelectInput, value_field = 'selected')
    ui_register_function('shiny::numericInput', shiny::updateNumericInput)
    ui_register_function('shiny::sliderInput', shiny::updateSliderInput)
    ui_register_function('shiny::checkboxInput', shiny::updateCheckboxInput)
    ui_register_function('shiny::textAreaInput', shiny::updateTextAreaInput)
    ui_register_function('shiny::dateInput', shiny::updateDateInput)
    ui_register_function('shiny::dateRangeInput', shiny::updateDateRangeInput)
    ui_register_function('shiny::checkboxGroupInput', shiny::updateDateInput, value_field = 'selected')
    ui_register_function('rave::compoundInput', rave:::updateCompoundInput, update_value = T)

    ui_register_function('shiny::plotOutput', shiny::renderPlot, default_args = list(res = 72))
    ui_register_function('shiny::tableOutput', shiny::renderTable)
    ui_register_function('shiny::verbatimTextOutput', shiny::renderPrint)
    ui_register_function('shiny::dataTableOutput', shiny::renderDataTable)
    ui_register_function('shiny::uiOutput', shiny::renderUI)
    ui_register_function('shiny::imageOutput', shiny::renderImage, default_args = list(deleteFile = FALSE))
    ui_register_function('shiny::htmlOutput', shiny::renderText)
    ui_register_function('shiny::textOutput', shiny::renderText)





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






