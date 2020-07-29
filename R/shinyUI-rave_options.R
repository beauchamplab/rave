rave_options_gui <- local({
  opt_names = list(
    # Systems
    'max_mem' = 'Max RAM for this machine in GB',
    'max_worker' = 'Maximum number of CPU cores to be used',
    'drive_speed' = 'Hard drive speed',
    
    # Core Option
    'raw_data_dir' = 'Native RAVE raw folder',
    'bids_data_dir' = 'BIDS data root (optional)',
    'data_dir' = 'RAVE subject data path',
    'tensor_temp_path' = 'Path to cache large data files',
    
    # 'crayon_enabled' = 'Color console',
    
    # SUMA
    'suma_path' = 'SUMA path (absolute path)',
    'suma_lib' = 'SUMA library paths (use new lines to separate)',
    'suma_nodes_per_electrodes' = 'Number of vertices per electrodes',
    'suma_spec_file' = 'Subject spec file name'
  )
  
  ######## UI and observers
  comps = list()
  # ------------------------ raw_data_dir ------------------------
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
            p(tags$small(span(style = gl('color:{col};'), msg))),
            div(
              class = ifelse(btn, '', 'hidden'),
              actionLink('raw_data_dir_reset', label)
            )
          ))
        })
        observeEvent(input$raw_data_dir_reset, {
          dir = input[[opt_id]]
          if(!dir.exists(dir)){
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  
  # ------------------------ bids_data_dir ------------------------
  {
    comps[[length(comps) + 1]] = list(
      type = 'Core Settings',
      opt_name = 'bids_data_dir',
      observer = rlang::quo({
        opt_id = 'bids_data_dir'
        output$bids_data_dir_input <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })
        output$bids_data_dir_ui <- renderUI({
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
              btn = FALSE
            }else if(val == rave_options(opt_id)){
              btn = FALSE
            }
          }else{
            msg = 'Path not exists, click "Create & Set Directory" to create'
            col = 'black'
            label = "Create & Set Directory"
          }
          
          return(tagList(
            p(tags$small(span(style = gl('color:{col};'), msg))),
            div(
              class = ifelse(btn, '', 'hidden'),
              actionLink('bids_data_dir_reset', label)
            )
          ))
        })
        observeEvent(input$bids_data_dir_reset, {
          dir = input[[opt_id]]
          if(!dir.exists(dir)){
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  
  
  
  # ------------------------ data_dir ----------------------------
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
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  
  # ------------------------ tensor_temp_path ------------------------
  {
    comps[[length(comps) + 1]] = list(
      type = 'Core Settings',
      opt_name = 'tensor_temp_path',
      observer = rlang::quo({
        opt_id = 'tensor_temp_path'
        output$tensor_temp_path_input <- renderUI({
          shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
        })
        output$tensor_temp_path_ui <- renderUI({
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
              btn = FALSE
            }else if(val == rave_options(opt_id)){
              btn = FALSE
            }
          }else{
            msg = 'Path not exists, click "Create & Set Directory" to create'
            col = 'black'
            label = "Create & Set Directory"
          }
          
          return(tagList(
            p(tags$small(span(style = gl('color:{col};'), msg))),
            div(
              class = ifelse(btn, '', 'hidden'),
              actionLink('tensor_temp_path_reset', label)
            )
          ))
        })
        observeEvent(input$tensor_temp_path_reset, {
          dir = input[[opt_id]]
          if(!dir.exists(dir)){
            dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
  
  # ------------------------ Depricated ------------------------
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
  #         showNotification(catgl('Color console is set to - {txt}'), type = 'message', id = paste0(opt_id, '_noty'))
  #       })
  #     })
  #   )
  # }
  
  
  ##### suma_path
  # {
  #   comps[[length(comps) + 1]] =
  #     list(
  #     type = 'SUMA',
  #     opt_name = 'suma_path',
  #     observer = rlang::quo({
  #       opt_id = 'suma_path'
  #       output_uiid = paste0(opt_id, '_input')
  #       resp_uiid = paste0(opt_id, '_ui')
  #       set_btnid = paste0(opt_id, '_set')
  #       notification_id = paste0(opt_id, '_noty')
  #       
  #       output[[output_uiid]] <- renderUI({
  #         shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
  #       })
  #       
  #       output[[resp_uiid]] <- renderUI({
  #         local_data$refresh
  #         val = input[[opt_id]]
  #         val %?<-% rave_options(opt_id)
  #         cat2(opt_id, ' - ', val)
  #         if(val == rave_options(opt_id) && dir.exists(val)){
  #           return()
  #         }
  #         if(!dir.exists(val)){
  #           return(tags$small(span(style = 'color:red', 'Cannot find SUMA path. Make sure it exists and contains executable file "suma".')))
  #         }
  #         if(!file.info(val)$isdir){
  #           return(tags$small(span(style = 'color:red', 'Please make sure this is a directory.')))
  #         }
  #         suma_file = file.path(val, 'suma')
  #         if(!file.exists(suma_file) || file.info(suma_file)$isdir){
  #           return(tags$small(span(style = 'color:red', 'Cannot find file "suma" within this directory.')))
  #         }
  #         return(tagList(
  #           actionLink(set_btnid, 'Set AFNI-SUMA path')
  #         ))
  #       })
  #       
  #       observeEvent(input[[set_btnid]], {
  #         val = input[[opt_id]]
  #         if(length(val) && dir.exists(val) && file.exists(file.path(val, 'suma'))){
  #           val = try_normalizePath(val)
  #           set_opt(suma_path = val)
  #           showNotification('SUMA path is found and set.', type = 'message', id = notification_id)
  #           return()
  #         }
  #         showNotification('Failed while setting SUMA path. Make sure this is a directory and it contains file "suma"', type = 'error', id = notification_id)
  #       })
  #     })
  #   )
  # }
  
  
  ##### suma_lib
  # {
  #   comps[[length(comps) + 1]] =
  #     list(
  #     type = 'SUMA',
  #     opt_name = 'suma_lib',
  #     observer = rlang::quo({
  #       opt_id = 'suma_lib'
  #       output_uiid = paste0(opt_id, '_input')
  #       resp_uiid = paste0(opt_id, '_ui')
  #       set_btnid = paste0(opt_id, '_set')
  #       notification_id = paste0(opt_id, '_noty')
  #       
  #       output[[output_uiid]] <- renderUI({
  #         val = local_data[[opt_id]]
  #         val = paste(val, collapse = '\n')
  #         shiny::textAreaInput(opt_id, opt_names[[opt_id]], value = val, resize = 'vertical', rows = 4L)
  #       })
  #       
  #       output[[resp_uiid]] <- renderUI({
  #         local_data$refresh
  #         val = input[[opt_id]]
  #         if(!length(val)){
  #           return()
  #         }
  #         cat2(opt_id, ' - ', val)
  #         if(val == paste(rave_options(opt_id), collapse = '\n')){
  #           return()
  #         }
  #         return(tagList(
  #           actionLink(set_btnid, 'Set SUMA library paths')
  #         ))
  #       })
  #       
  #       observeEvent(input[[set_btnid]], {
  #         val = input[[opt_id]]
  #         if(length(val)){
  #           val = stringr::str_trim(unlist(stringr::str_split(val, '\\n')))
  #           val = val[val != '']
  #           if(!length(val)){
  #             val = ''
  #           }
  #           set_opt(suma_lib = val)
  #           showNotification('SUMA library paths are set.', type = 'message', id = notification_id)
  #           return()
  #         }
  #         showNotification('Failed while setting SUMA library paths. Try it again?', type = 'error', id = notification_id)
  #       })
  #     })
  #   )
  # }
  
  ##### suma_nodes_per_electrodes
  # {
  #   comps[[length(comps) + 1]] =
  #     list(
  #     type = 'SUMA',
  #     opt_name = 'suma_nodes_per_electrodes',
  #     observer = rlang::quo({
  #       opt_id = 'suma_nodes_per_electrodes'
  #       output_uiid = paste0(opt_id, '_input')
  #       resp_uiid = paste0(opt_id, '_ui')
  #       set_btnid = paste0(opt_id, '_set')
  #       notification_id = paste0(opt_id, '_noty')
  #       
  #       output[[output_uiid]] <- renderUI({
  #         shiny::numericInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
  #       })
  #       
  #       output[[resp_uiid]] <- renderUI({
  #         val = input[[opt_id]]
  #         local_data$refresh
  #         cat2(opt_id, ' - ', val)
  #         if(length(val) != 1 || is.na(val)){
  #           val = rave_options(opt_id)
  #         }
  #         if(val == rave_options(opt_id)){
  #           return()
  #         }
  #         if(val <= 2){
  #           return(tags$small(span(style = 'color:red',
  #                                  'One face needs at least three vertices. This number is at least 3.')))
  #         }
  #         return(tagList(
  #           actionLink(set_btnid, 'Set Number of Vertices')
  #         ))
  #       })
  #       
  #       observeEvent(input[[set_btnid]], {
  #         val = input[[opt_id]]
  #         if(length(val) && is.numeric(val) && val >= 3){
  #           set_opt(suma_nodes_per_electrodes = val)
  #           showNotification(catgl('Electrode mesh has {val} vertices'), type = 'message', id = notification_id)
  #           return()
  #         }
  #         showNotification('Failed while setting # of vertices per electrodes', type = 'error', id = notification_id)
  #       })
  #     })
  #   )
  # }
  
  ##### suma_spec_file
  # {
  #   comps[[length(comps) + 1]] =
  #     list(
  #     type = 'SUMA',
  #     opt_name = 'suma_spec_file',
  #     observer = rlang::quo({
  #       opt_id = 'suma_spec_file'
  #       output_uiid = paste0(opt_id, '_input')
  #       resp_uiid = paste0(opt_id, '_ui')
  #       set_btnid = paste0(opt_id, '_set')
  #       notification_id = paste0(opt_id, '_noty')
  #       
  #       output[[output_uiid]] <- renderUI({
  #         shiny::textInput(opt_id, opt_names[[opt_id]], value = local_data[[opt_id]])
  #       })
  #       
  #       output[[resp_uiid]] <- renderUI({
  #         val = input[[opt_id]]
  #         local_data$refresh
  #         cat2(opt_id, ' - ', val)
  #         if(length(val) != 1){
  #           val = rave_options(opt_id)
  #         }
  #         val = stringr::str_trim(val)
  #         if(val == rave_options(opt_id)){
  #           return()
  #         }
  #         if(val == ''){
  #           return(tags$small(span(style = 'color:red',
  #                                  'Cannot be blank')))
  #         }
  #         msg = NULL
  #         if(!stringr::str_detect(stringr::str_to_lower(val), '\\.spec$')){
  #           msg = p(
  #             tags$small(span(style = 'color:red',
  #                             'WARNING: need to be a spec file in order to run (suma -spec [SPEC_FILE])'))
  #           )
  #         }
  #         return(tagList(
  #           msg,
  #           actionLink(set_btnid, 'Set spec file')
  #         ))
  #       })
  #       
  #       observeEvent(input[[set_btnid]], {
  #         val = input[[opt_id]]
  #         if(length(val) == 1){
  #           val = stringr::str_trim(val)
  #           if(val != ''){
  #             set_opt(suma_spec_file = val)
  #           }
  #           showNotification(catgl('Default SUMA spec file is set - {val}'), type = 'message', id = notification_id)
  #           return()
  #         }
  #         showNotification('Failed: is it a valid spec file name?', type = 'error', id = notification_id)
  #       })
  #     })
  #   )
  # }
  
  # ------------------------ max_mem ------------------------
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
            showNotification(gl('Max RAM is set - {val}'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed: invalid RAM size or blank entry.', type = 'error', id = notification_id)
        })
      })
    )
  }
  
  # ------------------------ max_worker ------------------------
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
              tags$small(span(HTML(gl("It is recommended that the number of CPU utilized is (the number of CPU cores) - 1 = {ncores-1}. This ensures good utilizations of your CPU while still leaving one core for the other tasks."))))
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
            showNotification(gl('Max workers is set - {val}'), type = 'message', id = notification_id)
            return()
          }
          showNotification('Failed: invalid max workers or blank entry.', type = 'error', id = notification_id)
        })
      })
    )
  }
  
  
  
  # ------------------------ Drive-Speed ------------------------
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
          
          upload_speed = speed[1]
          download_speed = speed[2]
          
          tagList(
            span(strong(opt_names[[opt_id]]), sprintf(': Write - %.1f MB/sec, Read - %.1f MB/sec ', upload_speed, download_speed),
                 actionLink(opt_id, 're-test speed'))
          )
        })
        observeEvent(input[[opt_id]], {
          speed = test_hdspeed()
          set_opt(drive_speed = speed)
          set_opt(check_updates_onstartup = FALSE)
        })
      })
    )
  }
  
  
  # ------------------------ Template Brain --------------------
  {
    
    comps[[length(comps) + 1]] = list(
      type = '3D Viewer',
      opt_name = 'template_brain',
      observer = rlang::quo({
        opt_id = 'template_brain'
        output_uiid = paste0(opt_id, '_input')
        resp_uiid = paste0(opt_id, '_ui')
        output[[output_uiid]] <- renderUI({
          local_data$refresh_3dviewer
          # get current template 
          old_sub = rave_options(opt_id)
          tsub = isolate(local_data$current_sub)
          tsub %?<-% old_sub
          tsub %?<-% getOption('threeBrain.template_subject', 'N27')
          
          # TODO: get all possible subjects
          template_subs = list.dirs('~/rave_data/others/three_brain/', full.names = FALSE, recursive = FALSE)
          template_subs = unique(c(tsub, template_subs, '[import new]'))
          template_subs = template_subs[!stringr::str_detect(template_subs, '^_')]
          
          tagList(
            p(
              'Template brain will be used when displaying electrodes across ',
              'multiple subjects. '
            ),
            selectInput(session$ns('template_subname'), 'Select a template brain', 
                        choices = template_subs, selected = tsub),
            shiny::conditionalPanel(
              condition = 'input.template_subname === "[import new]"',
              textInput(session$ns('template_newname'), 'New template name',
                        placeholder = 'name cannot be blank'),
              shinyFiles::shinyDirButton(
                id = session$ns('template_subdir'), 
                label = 'Select template FreeSurfer directory',
                title = 'Target FreeSurfer Directory', buttonType = FALSE,
                class = 'btn-link'
              ),
              uiOutput(session$ns('template_target')),
              hr(),
              dipsaus::actionButtonStyled(session$ns('template_import'), 'Import', type = 'primary'),
              ns = session$ns
            ),
            
            shiny::conditionalPanel(
              condition = 'input.template_subname !== "[import new]"',
              ns = session$ns,
              uiOutput(session$ns('template_snapshot'))
            )
            
          )
        })
        
        output$template_snapshot <- renderUI({
          tsub = input$template_subname
          rootdir = '~/rave_data/others/three_brain/'
          dir_create(rootdir, recursive = TRUE)
          rootdir = normalizePath(rootdir)
          tdir = file.path(rootdir, tsub)
          if(length(tdir) && dir.exists(tdir)){
            set_opt(threeBrain_template_subject = tsub)
            set_opt(threeBrain_template_dir = rootdir)
            options(
              `threeBrain.template_subject` = tsub,
              `threeBrain.template_dir` = rootdir
            )
            
            tagList(
              strong('Current template: '), br(), tsub, br(),
              strong('Parent Directory: '), br(), rootdir
              
            )
          } else{ NULL }
        })
        
        roots = c('Current Path' = getwd(), 'Home' = '~', 'Root' = '/')
        
        shinyFiles::shinyDirChoose( input, session$ns('template_subdir'), 
                                    roots = roots, defaultRoot = 'Home', filetypes = '')
        
        output$template_target <- renderUI({
          path = get_fspath(quiet = TRUE)
          passed = TRUE
          if(is.null(path)){
            path = span(style = 'color:red', 'not set')
            passed = FALSE
          } else if( isFALSE(path) ){
            path = span(style = 'color:red', ' (not valid FreeSurfer directory)')
            passed = FALSE
          }
          subname = input$template_newname
          if(subname == ''){
            subname = span(style = 'color:red', 'not set')
            passed = FALSE
          }else{
            new_name2 = stringr::str_remove_all(subname, '[^0-9a-zA-Z_]')
            new_name2 = stringr::str_remove_all(new_name2, '^[_]*')
            if(new_name2 != subname){
              subname = span(style = 'color:red', 'not valid. Please only enter letters, digits, and "_". ',
                             'Avoid "_" in the first character.')
              passed = FALSE
            }
          }
          if(passed){
            msg = tags$small(
              'Please press ', span(style = 'color:red', 'import button'), ' to set & preview'
            )
          }else{
            msg = NULL
          }
          p(
            'Template subject code: ', subname, br(),
            'Template path: ', path, br(),
            msg
          )
        })
        
        get_fspath = function( quiet = FALSE ){
          dir = as.list(input$template_subdir)
          
          if(!length(dir$root) || !(dir$root %in% names(roots))){ return() }
          root = roots[[dir$root]]
          paths = c(list(root), as.list(dir$path))
          names(paths) = NULL
          
          path = normalizePath(do.call(file.path, paths), mustWork = FALSE)
          if(!dir.exists(path)){
            if( quiet ){ return(FALSE) }
            stop('Cannot find path to template brain')
          }
          # check file name
          paths = stringr::str_split(path, '/|\\\\', simplify = TRUE)
          depth = length(paths)
          if(paths[depth] %in% c('mri', 'label', 'surf', 'RAVE', 'SUMA')){
            # this is not the root dir for subject
            path = dirname(path)
            paths = stringr::str_split(path, '/|\\\\', simplify = TRUE)
            depth = length(paths)
          }
          
          # check path
          if(!threeBrain::check_freesurfer_path(path)){
            if( quiet ){ return(FALSE) }
            stop('This is not a valid FreeSurfer folder.')
          }
          
          path
        }
        
        observeEvent(input$template_subdir, {
          get_fspath()
        })
        
        
        
        observeEvent(input$template_import, {
          new_name = input$template_newname
          if( !length(new_name) || new_name == '' ){
            stop('Please enter name for new template brain')
          }
          new_name2 = stringr::str_remove_all(new_name, '[^0-9a-zA-Z_]')
          new_name2 = stringr::str_remove_all(new_name2, '^[_]*')
          if(new_name2 != new_name){
            stop('Template name can only contains letters, digits and "_". "_" cannot be the first character.')
          }
          source_path = get_fspath()
          # check if the path exists in three_brain, just ignore and override
          target_dir = file.path('~/rave_data/others/three_brain/', new_name2)
          dir_create(target_dir)
          target_dir = normalizePath(target_dir)
          
          if(target_dir != source_path){
            # override
            fs = list.files(source_path, full.names = TRUE, all.files = FALSE, include.dirs = TRUE, recursive = FALSE)
            prog = dipsaus::progress2('Copying files to home - rave_data - others - three_brain', max = length(fs)+1, shiny_auto_close = TRUE)
            
            lapply(fs, function(f){
              prog$inc(detail = f)
              file.copy(f, target_dir, overwrite = TRUE, recursive = TRUE)
            })
          }else{
            prog = dipsaus::progress2('Re-cache brain', max = 1, shiny_auto_close = TRUE)
          }
          
          # unlink cache
          unlink(file.path(target_dir, 'RAVE'), recursive = TRUE)
          
          # cache the brain
          prog$inc(detail = 'Creating cache')
          threeBrain::import_from_freesurfer(
            fs_path = file.path('~/rave_data/others/three_brain/', new_name2),
            subject_name = new_name2
          )
          
          # now update?
          local_data$current_sub = new_name2
          local_data$refresh_3dviewer = Sys.time()
        })
        
        .env = environment()
        
        observeEvent(input$template_downloadn27, {
          # print(input$template_downloadn27)
          threeBrain::download_N27()
          local_data$brain = threeBrain::merge_brain()
          local_data$refresh_3dviewer2 = Sys.time()
        })
        
        output[[resp_uiid]] <- renderUI({
          local_data$refresh_3dviewer2
          tsub = input$template_subname
          if( length(tsub) && (tsub != '[import new]') ){
            # check brain
            tryCatch({
              local_data$brain = threeBrain::freesurfer_brain2(
                fs_subject_folder = file.path('~/rave_data/others/three_brain/', tsub),
                subject_name = tsub,
                surface_types = c('pial')
              )
              return(
                threeBrain::threejsBrainOutput(session$ns('template_viewer'))
              )
            }, error = function(e){
              local_data$brain = NULL
              # return method to download
              
              if(tsub == 'N27'){
                return(
                  div(
                    style = 'text-align:center; min-height: 500px',
                    p(style = 'padding-top: 10px', 
                      'N27 brain needs download from internet. Click ',
                      actionLink(session$ns('template_downloadn27'), 'here to download'), '.'))
                )
                
              }else{
                return(
                  div(style = 'text-align:center; color: #d1d1d1; min-height: 500px; padding-top: 10px', 
                      'This seems to be an invalid template folder. Please select others or import from your own repository.')
                )
              }
              
            })
          }else{
            div(style = 'text-align:center; color: #d1d1d1; min-height: 500px; padding-top: 10px', 
                'Please finish import process')
          }
          
        })
        
        output$template_viewer <- threeBrain::renderBrain({
          # Check if the template is valid
          if(length(local_data$brain)){
            local_data$brain$plot(side_width = 150, side_display = FALSE)
          }
          
        })
        
      })
    )
  }
  
  # ------------------------ Module file ------------------------
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
              width = 3, 
              fluidRow(
                box(
                  title = 'Core Settings',
                  width = 12L,
                  fluidRow(
                    ##### Core Settings
                    column(
                      width = 12L,
                      tagList(
                        dipsaus::drop_nulls(
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
                        dipsaus::drop_nulls(
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
              width = 9L,
              fluidRow(
                box(
                  title = '3D Viewer',
                  width = 12L,
                  fluidRow(
                    ##### Viewer Settings
                    tagList(
                      dipsaus::drop_nulls(
                        lapply(comps, function(comp){
                          if(comp$type == '3D Viewer'){
                            id = comp$opt_name
                            tagList(
                              column(
                                width = 3L,
                                uiOutput(paste0(id, '_input'))
                              ),
                              column(
                                width = 9L,
                                uiOutput(paste0(id, '_ui'))
                              )
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
            
            # column(
            #   width = 6,
            #   fluidRow(
            #     box(
            #       title = 'SUMA',
            #       width = 12L,
            #       fluidRow(
            #         ##### SUMA
            #         column(
            #           width = 12L,
            #           tagList(
            #             dipsaus::drop_nulls(
            #               lapply(comps, function(comp){
            #                 if(comp$type == 'SUMA'){
            #                   id = comp$opt_name
            #                   tagList(
            #                     uiOutput(paste0(id, '_input')),
            #                     uiOutput(paste0(id, '_ui')),
            #                     div(style = 'margin-bottom: 15px')
            #                   )
            #                 }else{
            #                   NULL
            #                 }
            #               })
            #             )
            #           ),
            #           hr(),
            #           div(style = 'float:right', actionButton('SUMA_test', 'Test SUMA'))
            #         )
            #       )
            #     )
            #   )
            # )
            
            
            
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
          rave_options(..., launch_gui = FALSE, .save = TRUE)
          args = list(...)
          for(nm in names(args)){
            local_data[[nm]] = args[[nm]]
            catgl('Setting option: {nm} => {args[[nm]]}')
          }
          local_data$refresh = Sys.time()
        }
        
        lapply(comps, function(comp){
          dipsaus::eval_dirty(comp$observer)
        })
        
        observeEvent(input$SUMA_test, {
          suma_path = rave_options('suma_path')
          suma_lib = rave_options('suma_lib')
          system2('suma', env = c(
            gl('PATH=$PATH:"{suma_path}"'),
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
              tags$pre(gl(
                'export PATH=$PATH:"{suma_path}"',
                paste('export', suma_lib, collapse = '\n'),
                'suma'
              ))
            ),
            hr(),
            p(
              strong('tcsh'), ' terminal:', br(), tags$small('we highly recommend you switching back to bash terminals because RAVE does not use tcsh. However, you can still use tcsh terminals to debug.'),
              tags$pre(gl(
                'set path = ( $path "{suma_path}" )',
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
            editable = TRUE,
            rownames = FALSE,
            selection = list(mode = 'single', target = 'cell'),
            options = list(ordering = FALSE, pageLength = 50, nowrap = TRUE),
            escape = FALSE
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
            catgl(tbl[row, col], ' >> ', val)
            envir$modules[row, col] <- val
            DT::replaceData(proxy, envir$modules, resetPaging = FALSE, rownames = FALSE)
            
            if(has_copy){
              utils::write.csv(envir$modules, rave_options('module_lookup_file'), row.names = FALSE)
            }else{
              safe_write_csv(envir$modules, rave_options('module_lookup_file'), row.names = FALSE)
              envir$has_copy = TRUE
            }
            
            local_data$table_updated = Sys.time()
          }
          
          local_data$refresh = Sys.time()
          
        })
        
        session$onSessionEnded(function() {
          shiny::stopApp()
        })
        
      },
      options = list(..., launch.browser = launch.browser)
    )
  }
  
})
  
