local({
  opt_names = list(
    # Systems
    'max_mem' = 'Max RAM for this machine in GB',
    'max_worker' = 'Maximum number of CPU cores to be used',
    'drive_speed' = 'Hard drive speed',

    # Core Option
    'raw_data_dir' = 'Raw subject data path',
    'data_dir' = 'RAVE subject data path',
    # 'crayon_enabled' = 'Color console',

    # SUMA
    'suma_path' = 'SUMA path (absolute path)',
    'suma_lib' = 'SUMA library paths (use new lines to separate)',
    'suma_nodes_per_electrodes' = 'Number of vertices per electrodes',
    'suma_spec_file' = 'Subject spec file name'
  )

  ######## UI and observers
  comps = list()
  ##### raw_data_dir
  {
    comps[[length(comps) + 1]] = list(
      type = 'Core Settings',
      opt_name = 'raw_data_dir',
      observer = rlang::quo({
        opt_id = 'raw_data_dir'
        output$raw_data_dir_input <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })
        output$raw_data_dir_ui <- renderUI({
          val = input[[opt_id]]
          val %?<-% rave_options(opt_id)
          msg = ''
          col = 'red'
          btn = TRUE
          label = "Set Directory"
          if(dir.exists(val)){
            finfo = file.info(val)
            if(!finfo$isdir){
              msg = 'This is not a valid "directory" path'
              btn = F
            }else if(val == rave_options(opt_id)){
              btn = F
            }
          }else{
            msg = 'Path not exists, click "Create & Set Directory" to create'
            col = 'black'
            label = "Create & Set Directory"
          }

          return(tagList(
            p(tags$small(span(style = fprintf('color:${{col}};'), msg))),
            div(
              class = ifelse(btn, '', 'hidden'),
              actionLink('raw_data_dir_reset', label)
            )
          ))
        })
        observeEvent(input$raw_data_dir_reset, {
          dir = input[[opt_id]]
          if(!dir.exists(dir)){
            dir.create(dir, recursive = T, showWarnings = F)
          }
          # Make sure it exists, otherwise error notification
          if(dir.exists(dir)){
            dir = try_normalizePath(path = dir)
            arg = list(dir)
            names(arg) = opt_id
            do.call(set_opt, arg)
            showNotification('Raw data directory is set.', type = 'message', id = paste0(opt_id, '_noty'))
          }else{
            showNotification('Failed while setting raw data directory: Cannot create directory', type = 'error', id = paste0(opt_id, '_noty'))
          }
        })
      })
    )
  }

  ##### data_dir
  {

    comps[[length(comps) + 1]] = list(
      type = 'Core Settings',
      opt_name = 'data_dir',
      observer = rlang::quo({
        opt_id = 'data_dir'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        output[[output_uiid]] <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })
        output[[resp_uiid]] <- renderUI({
          val = input[[opt_id]]
          val %?<-% rave_options(opt_id)
          msg = ''
          btn = TRUE
          label = "Set Directory"
          if(dir.exists(val)){
            finfo = file.info(val)
            if(!finfo$isdir){
              msg = 'This is not a valid "directory" path'
              btn = F
            }else if(val == rave_options(opt_id)){
              btn = F
            }
          }else{
            msg = 'Path not exists, click "Create & Set Directory" to create and set this option.'
            label = "Create & Set Directory"
          }

          return(tagList(
            p(tags$small(span(style = 'color:red', msg))),
            div(
              class = ifelse(btn, '', 'hidden'),
              actionLink(set_btnid, label)
            )
          ))
        })
        observeEvent(input[[set_btnid]], {
          dir = input[[opt_id]]
          if(!dir.exists(dir)){
            dir.create(dir, recursive = T, showWarnings = F)
          }
          # Make sure it exists, otherwise error notification
          if(dir.exists(dir)){
            dir = try_normalizePath(path = dir)
            arg = list(dir)
            names(arg) = opt_id
            do.call(set_opt, arg)

            # test speed
            speed = test_hdspeed()
            set_opt(drive_speed = speed)
            showNotification('RAVE data directory is set.', type = 'message', id = paste0(opt_id, '_noty'))

          }else{
            showNotification('Failed while setting RAVE data directory: Cannot create directory', type = 'error', id = paste0(opt_id, '_noty'))
          }
        })
      })
    )
  }


  ##### Crayon
  # {
  # 
  #   comps[[length(comps) + 1]] = list(
  #     type = 'Core Settings',
  #     opt_name = 'crayon_enabled',
  #     observer = rlang::quo({
  #       opt_id = 'crayon_enabled'
  #       output_uiid = paste0(opt_id, '_input')
  #       resp_uiid = paste0(opt_id, '_ui')
  #       output[[output_uiid]] <- renderUI({
  #         tagList(
  #           span(strong(opt_names[[opt_id]]), ' - Current status: ',
  #                actionLink(opt_id, ifelse(isTRUE(local_data[[opt_id]]), 'Enabled', 'Disabled')))
  #         )
  #       })
  #       observeEvent(input[[opt_id]], {
  #         val = !isTRUE(rave_options(opt_id))
  #         set_opt(crayon_enabled = val)
  #         txt = ifelse(val, 'Enabled', 'Disabled')
  #         showNotification(fprintf('Color console is set to - ${{txt}}'), type = 'message', id = paste0(opt_id, '_noty'))
  #       })
  #     })
  #   )
  # }


  ##### suma_path
  {
    comps[[length(comps) + 1]] = list(
      type = 'SUMA',
      opt_name = 'suma_path',
      observer = rlang::quo({
        opt_id = 'suma_path'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })

        output[[resp_uiid]] <- renderUI({
          local_data$refresh
          val = input[[opt_id]]
          val %?<-% rave_options(opt_id)
          logger(opt_id, ' - ', val)
          if(val == rave_options(opt_id) && dir.exists(val)){
            return()
          }
          if(!dir.exists(val)){
            return(tags$small(span(style = 'color:red', 'Cannot find SUMA path. Make sure it exists and contains executable file "suma".')))
          }
          if(!file.info(val)$isdir){
            return(tags$small(span(style = 'color:red', 'Please make sure this is a directory.')))
          }
          suma_file = file.path(val, 'suma')
          if(!file.exists(suma_file) || file.info(suma_file)$isdir){
            return(tags$small(span(style = 'color:red', 'Cannot find file "suma" within this directory.')))
          }
          return(tagList(
            actionLink(set_btnid, 'Set AFNI-SUMA path')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val) && dir.exists(val) && file.exists(file.path(val, 'suma'))){
            val = try_normalizePath(val)
            set_opt(suma_path = val)
            showNotification('SUMA path is found and set.', type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed while setting SUMA path. Make sure this is a directory and it contains file "suma"', type = 'error', id = notification_id)
        })
      })
    )
  }


  ##### suma_lib
  {
    comps[[length(comps) + 1]] = list(
      type = 'SUMA',
      opt_name = 'suma_lib',
      observer = rlang::quo({
        opt_id = 'suma_lib'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          val = local_data[[opt_id]]
          val = paste(val, collapse = '\n')
          shiny::textAreaInput(opt_id, opt_names[[opt_id]], value = val, resize = 'vertical', rows = 4L)
        })

        output[[resp_uiid]] <- renderUI({
          local_data$refresh
          val = input[[opt_id]]
          if(!length(val)){
            return()
          }
          logger(opt_id, ' - ', val)
          if(val == paste(rave_options(opt_id), collapse = '\n')){
            return()
          }
          return(tagList(
            actionLink(set_btnid, 'Set SUMA library paths')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val)){
            val = stringr::str_trim(unlist(stringr::str_split(val, '\\n')))
            val = val[val != '']
            if(!length(val)){
              val = ''
            }
            set_opt(suma_lib = val)
            showNotification('SUMA library paths are set.', type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed while setting SUMA library paths. Try it again?', type = 'error', id = notification_id)
        })
      })
    )
  }

  ##### suma_nodes_per_electrodes
  {
    comps[[length(comps) + 1]] = list(
      type = 'SUMA',
      opt_name = 'suma_nodes_per_electrodes',
      observer = rlang::quo({
        opt_id = 'suma_nodes_per_electrodes'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          shiny::numericInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })

        output[[resp_uiid]] <- renderUI({
          val = input[[opt_id]]
          local_data$refresh
          logger(opt_id, ' - ', val)
          if(length(val) != 1 || is.na(val)){
            val = rave_options(opt_id)
          }
          if(val == rave_options(opt_id)){
            return()
          }
          if(val <= 2){
            return(tags$small(span(style = 'color:red',
                                   'One face needs at least three vertices. This number is at least 3.')))
          }
          return(tagList(
            actionLink(set_btnid, 'Set Number of Vertices')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val) && is.numeric(val) && val >= 3){
            set_opt(suma_nodes_per_electrodes = val)
            showNotification(fprintf('Electrode mesh has ${{val}} vertices'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed while setting # of vertices per electrodes', type = 'error', id = notification_id)
        })
      })
    )
  }

  ##### suma_spec_file
  {
    comps[[length(comps) + 1]] = list(
      type = 'SUMA',
      opt_name = 'suma_spec_file',
      observer = rlang::quo({
        opt_id = 'suma_spec_file'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })

        output[[resp_uiid]] <- renderUI({
          val = input[[opt_id]]
          local_data$refresh
          logger(opt_id, ' - ', val)
          if(length(val) != 1){
            val = rave_options(opt_id)
          }
          val = stringr::str_trim(val)
          if(val == rave_options(opt_id)){
            return()
          }
          if(val == ''){
            return(tags$small(span(style = 'color:red',
                                   'Cannot be blank')))
          }
          msg = NULL
          if(!stringr::str_detect(stringr::str_to_lower(val), '\\.spec$')){
            msg = p(
              tags$small(span(style = 'color:red',
                              'WARNING: need to be a spec file in order to run (suma -spec [SPEC_FILE])'))
            )
          }
          return(tagList(
            msg,
            actionLink(set_btnid, 'Set spec file')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val) == 1){
            val = stringr::str_trim(val)
            if(val != ''){
              set_opt(suma_spec_file = val)
            }
            showNotification(fprintf('Default SUMA spec file is set - ${{val}}'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed: is it a valid spec file name?', type = 'error', id = notification_id)
        })
      })
    )
  }

  ##### max_mem
  {
    comps[[length(comps) + 1]] = list(
      type = 'System',
      opt_name = 'max_mem',
      observer = rlang::quo({
        opt_id = 'max_mem'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          shiny::numericInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })

        output[[resp_uiid]] <- renderUI({
          val = input[[opt_id]]
          local_data$refresh
          logger(opt_id, ' - ', val)
          if(length(val) != 1 || is.na(val)){
            val = rave_options(opt_id)
          }
          if(val == rave_options(opt_id)){
            return()
          }
          if(val < 2){
            return(tags$small(span(style = 'color:red', 'I can barely do anything with memory<2GB')))
          }
          msg = NULL
          if(val < rave_options('max_worker') * 8){
            msg = p(
              tags$small(span("It is highly recommended that Max RAM size >= 8GB x CPU allowed. If you don't have enough RAM, it's OK to ignore this message :)"))
            )
          }
          return(tagList(
            msg,
            actionLink(set_btnid, 'Set Max RAM')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val) == 1 && val >=2){
            set_opt(max_mem = val)
            showNotification(fprintf('Max RAM is set - ${{val}}'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed: invalid RAM size or blank entry.', type = 'error', id = notification_id)
        })
      })
    )
  }

  ##### max_worker
  {
    comps[[length(comps) + 1]] = list(
      type = 'System',
      opt_name = 'max_worker',
      observer = rlang::quo({
        opt_id = 'max_worker'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        set_btnid = paste0(opt_id, '_set')
        notification_id = paste0(opt_id, '_noty')

        output[[output_uiid]] <- renderUI({
          shiny::numericInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })

        output[[resp_uiid]] <- renderUI({
          val = input[[opt_id]]
          local_data$refresh
          logger(opt_id, ' - ', val)
          if(length(val) != 1 || is.na(val)){
            val = rave_options(opt_id)
          }
          if(val == rave_options(opt_id)){
            return()
          }
          if(val < 1){
            return(tags$small(span(style = 'color:red', 'At least one CPU is needed')))
          }
          msg = NULL
          ncores = future::availableCores()
          if(val >= ncores){
            msg = p(
              tags$small(span(HTML(fprintf("It is recommended that the number of CPU utilized is (the number of CPU cores) - 1 = ${{ncores-1}}. This ensures good utilizations of your CPU while still leaving one core for the other tasks (such as watching videos?).", br(), "However, if you want highest speed, set this value to be ${{ncores}}"))))
            )
          }
          if(stringr::str_detect(stringr::str_to_lower(Sys.info()['sysname']), '^win')){
            msg = p(
              tags$small(span(style = 'color:red;', "WARNING: you are using Windows system. Windows doesn't allow forked clusters, therefore this value is not used."))
            )
          }
          return(tagList(
            msg,
            actionLink(set_btnid, 'Set Max Number of Workers')
          ))
        })

        observeEvent(input[[set_btnid]], {
          val = input[[opt_id]]
          if(length(val) == 1 && val >= 1){
            val = round(val)
            set_opt(max_worker = val)
            showNotification(fprintf('Max workers is set - ${{val}}'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed: invalid max workers or blank entry.', type = 'error', id = notification_id)
        })
      })
    )
  }



  ##### Drive-Speed
  {

    comps[[length(comps) + 1]] = list(
      type = 'System',
      opt_name = 'drive_speed',
      observer = rlang::quo({
        opt_id = 'drive_speed'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        output[[output_uiid]] <- renderUI({
          speed = local_data[[opt_id]]

          # if speed is blank, test speed
          if(!is.numeric(speed) || length(speed) != 2){
            speed = test_hdspeed()
            if(!any(is.na(speed))){
              set_opt(drive_speed = speed)
            }
          }

          speed = 1 / speed * 1000000

          # Upload speed
          upload_speed = to_ram_size(speed[1])
          download_speed = to_ram_size(speed[2])

          tagList(
            span(strong(opt_names[[opt_id]]), sprintf(': Write - %s/sec, Read - %s/sec ', upload_speed, download_speed),
                 actionLink(opt_id, 're-test speed'))
          )
        })
        observeEvent(input[[opt_id]], {
          speed = test_hdspeed()
          set_opt(drive_speed = speed)
        })
      })
    )
  }

  ######## Module file
  load_module_table = function(){
    modules = arrange_modules(FALSE, FALSE, FALSE)
    modules = modules[, c("ID", "Name", "Group", "Package", "Active", "Notes")]
    modules
  }


  function(..., launch.browser = TRUE){
    shiny::shinyApp(
      ui = shinydashboard::dashboardPage(
        header = shinydashboard::dashboardHeader(
          title = 'RAVE Settings'
        ),
        sidebar = shinydashboard::dashboardSidebar(
          disable = T
        ),
        body = shinydashboard::dashboardBody(

          fluidRow(

            # Modules
            box(
              title = 'Modules',
              width = 12L,
              actionLink('module_update', 'Check for new modules'),
              div(DT::DTOutput('modules'), style = 'overflow-x:scroll;'),
              collapsible = TRUE
            ),

            column(
              width = 6,
              fluidRow(
                box(
                  title = 'Core Settings',
                  width = 12L,
                  fluidRow(
                    ##### Core Settings
                    column(
                      width = 12L,
                      tagList(
                        dropNulls(
                          lapply(comps, function(comp){
                            if(comp$type == 'Core Settings'){
                              id = comp$opt_name
                              tagList(
                                uiOutput(paste0(id, '_input')),
                                uiOutput(paste0(id, '_ui')),
                                div(style = 'margin-bottom: 15px')
                              )
                            }else{
                              NULL
                            }
                          })
                        )
                      )
                    )
                  )
                ),

                box(
                  title = 'System',
                  width = 12L,
                  fluidRow(
                    ##### System Settings
                    column(
                      width = 12L,
                      tagList(
                        dropNulls(
                          lapply(comps, function(comp){
                            if(comp$type == 'System'){
                              id = comp$opt_name
                              tagList(
                                uiOutput(paste0(id, '_input')),
                                uiOutput(paste0(id, '_ui')),
                                div(style = 'margin-bottom: 15px')
                              )
                            }else{
                              NULL
                            }
                          })
                        )
                      )
                    )
                  )

                )
              )
            ),
            column(
              width = 6,
              fluidRow(
                box(
                  title = 'SUMA',
                  width = 12L,
                  fluidRow(
                    ##### SUMA
                    column(
                      width = 12L,
                      tagList(
                        dropNulls(
                          lapply(comps, function(comp){
                            if(comp$type == 'SUMA'){
                              id = comp$opt_name
                              tagList(
                                uiOutput(paste0(id, '_input')),
                                uiOutput(paste0(id, '_ui')),
                                div(style = 'margin-bottom: 15px')
                              )
                            }else{
                              NULL
                            }
                          })
                        )
                      ),
                      hr(),
                      div(style = 'float:right', actionButton('SUMA_test', 'Test SUMA'))
                    )
                  )
                )
              )
            )



          )
        ),
        title = 'RAVE Settings',
        skin = 'purple'
      ),
      server = function(input, output, session){
        modules = load_module_table()
        has_copy = FALSE

        local_data = reactiveValues(refresh = NULL, modules = modules)
        envir = environment()

        for(opt_id in names(opt_names)){
          local_data[[opt_id]] = rave_options(opt_id)
        }

        set_opt = function(...){
          rave_options(..., launch_gui = F, .save = T)
          args = list(...)
          for(nm in names(args)){
            local_data[[nm]] = args[[nm]]
          }
          local_data$refresh = Sys.time()
        }

        lapply(comps, function(comp){
          eval_dirty(comp$observer)
        })

        observeEvent(input$SUMA_test, {
          suma_path = rave_options('suma_path')
          suma_lib = rave_options('suma_lib')
          system2('suma', env = c(
            fprintf('PATH=$PATH:"${{suma_path}}"'),
            suma_lib
          ), wait = F)
          showModal(shiny::modalDialog(
            title = 'Test SUMA',
            easyClose = F,
            footer = shiny::modalButton('Confirm & Dismiss'),
            p('If SUMA window pops out, then you have the right settings. Dismiss this dialogue or use the following debug information:'),
            hr(),
            p(
              strong('bash'), ' terminal:',
              tags$pre(fprintf(
                'export PATH=$PATH:"${{suma_path}}"',
                paste('export', suma_lib, collapse = '\n'),
                'suma'
              ))
            ),
            hr(),
            p(
              strong('tcsh'), ' terminal:', br(), tags$small('we highly recommend you switching back to bash terminals because RAVE does not use tcsh. However, you can still use tcsh terminals to debug.'),
              tags$pre(fprintf(
                'set path = ( $path "${{suma_path}}" )',
                paste('setenv', stringr::str_replace_all(suma_lib, '\\=', ' '), collapse = '\n'),
                'suma'
              ))
            )
          ))
        })


        observeEvent(input$module_update, {
          envir$modules = arrange_modules(refresh = TRUE, reset = FALSE, quiet = FALSE)
          local_data$table_updated = Sys.time()
        })

        output$modules <- DT::renderDT({
          local_data$table_updated
          DT::datatable(
            class = 'compact nowrap',
            envir$modules,
            editable = T,
            rownames = F,
            selection = list(mode = 'single', target = 'cell'),
            options = list(ordering = FALSE, pageLength = 50, nowrap = T),
            escape = F
          )
        })


        proxy = DT::dataTableProxy('modules')

        observeEvent(input$modules_cell_edit, {
          info = input$modules_cell_edit
          row = info$row
          col = info$col + 1
          tbl = envir$modules
          var = names(tbl)[[col]]
          val = info$value
          switch (
            var,
            'ID' = {
              showNotification(
                p('Cannot change module ID, otherwise RAVE cannot find modules. The change will be ignored'),
                type = 'error', id = 'module_table')
              NULL
            },
            'Package' = {
              showNotification(
                p('Cannot change Package name, otherwise RAVE cannot find modules. The change will be ignored'),
                type = 'error', id = 'module_table')
              NULL
            },
            'Active' = {
              tryCatch({
                val = stringr::str_to_lower(stringr::str_c(val))
                val_nb = suppressWarnings(as.numeric(val))
                if(stringr::str_detect(val, '(t)|(tr(u)|(ue))') || (!is.na(val_nb) && val_nb > 0)){
                  val = TRUE
                }else{
                  val = FALSE
                }
                val
              }, error = function(e){
                FALSE
              })
            },
            {
              val
            }
          ) ->
            val



          if(!is.null(val)){
            logger(tbl[row, col], ' >> ', val)
            envir$modules[row, col] <- val
            DT::replaceData(proxy, envir$modules, resetPaging = FALSE, rownames = F)

            if(has_copy){
              write.csv(envir$modules, rave_options('module_lookup_file'), row.names = F)
            }else{
              safe_write_csv(envir$modules, rave_options('module_lookup_file'), row.names = F)
              envir$has_copy = TRUE
            }

            local_data$table_updated = Sys.time()
          }

          local_data$refresh = Sys.time()

        })

      },
      options = list(..., launch.browser = launch.browser)
    )
  }

}) ->
  rave_options_gui

# rave_options_gui()

#' Function to test local disk speed
#' @param file_size default 10MB, i.e. 1e7
#' @param quiet show messages?
test_hdspeed <- function(file_size = 1e7, quiet = F){
  data_dir = rave_options('data_dir')

  if(!dir.exists(data_dir)){
    logger(fprintf('RAVE data_dir is missing, please make sure the following directory exists: ${{data_dir}}'), level = 'ERROR')
    return(c(NA, NA))
  }

  # create tempdir for testing
  test_dir = file.path(data_dir, '.rave_hd_test', paste(sample(LETTERS, 8), collapse = ''))
  dir.create(test_dir, recursive = T, showWarnings = T)

  progress = progress(title = 'Testing read/write speed', max = 2, quiet = quiet)
  on.exit({
    unlink(test_dir, recursive = T)
    progress$close()
  })

  progress$inc(message = 'Write to disk...')

  # generate 10M file, tested
  file = tempfile(tmpdir = test_dir)
  dat = paste0(sample(LETTERS, file_size - 1, replace = T), collapse = '')
  upload = system.time(writeLines(dat, file, useBytes = T))

  progress$inc(message = 'Read from disk...')
  download = system.time({dat_c = readLines(file)})

  if(exists('dat_c') && dat_c != dat){
    logger('Uploaded data is broken...', level = 'WARNING')
  }

  ratio = file.info(file)$size / 1000000

  speed = c(upload[3], download[3]) / ratio
  names(speed) = NULL
  return(speed)
}
