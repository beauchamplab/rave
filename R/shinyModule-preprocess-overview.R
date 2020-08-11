rave_pre_overview3 <- function(module_id = 'OVERVIEW_M', sidebar_width = 2, doc_prefix = 'ravepreprocessoverview'){
  ns = shiny::NS(module_id)
  
  HAS_CACHE = 1L
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  file_formats <- names(raveio::IMPORT_FORMATS)

  body = fluidRow(
    box(
      # id = ns('sidebar'),
      width = sidebar_width,
      # shiny::tabPanel(
      title = 'Overview',
      box_link = sprintf(url_format, 'input', 'overview'),
      fluidRow(
        column(
          width = 12,
          div(
            class = 'rave-grouped-inputs rave-grid-inputs margin-top-20',
            div(class='rave-grid-inputs-legend', 'Step 1'),
            div(
              style = 'flex-basis: 100%;',
              uiOutput(ns('overview_inputs1')),
              tags$small(uiOutput(ns('step_1_msg')))
            )
          ),
          div(
            class = 'rave-grouped-inputs rave-grid-inputs margin-top-20',
            div(class='rave-grid-inputs-legend', 'Step 2'),
            div(
              style = 'flex-basis: 100%;',
              uiOutput(ns('overview_inputs2')),
              uiOutput(ns('overview_inputs3')),
              tags$small(uiOutput(ns('step_2_msg'))),
              uiOutput(ns('overview_inputs4'))
            )
          ),
          div(
            class = 'rave-grouped-inputs rave-grid-inputs margin-top-20',
            div(class='rave-grid-inputs-legend', 'Step 3'),
            div(
              style = 'flex-basis: 100%;',
              uiOutput(ns('overview_inputs5'))
            )
          )
        )
        # )
      )
    ),
    column(
      width = 12 - sidebar_width,
      fluidRow(
        box(
          width = 12,
          title = 'Information',
          box_link = sprintf(url_format, 'output', 'information'),
          fluidRow(
            column(
              width = 6,
              h2('Basic Information'),
              tableOutput(ns('overview_table'))
            ),
            column(
              width = 6,
              h2('Electrode Settings'),
              tableOutput(ns('channel_table'))
            ),
            column(
              width = 12,
              h2('Log'),
              tableOutput(ns('subject_log_table'))
            )
          )
        ),
        box(
          width = 12L,
          title = 'Import widgets',
          box_link = sprintf(url_format, 'output', 'importwidgets'),
          uiOutput(ns('lfp_import'))
        )
      )
    )
  )



  server = function(input, output, session, user_data, utils, project_name = NULL, subject_code = NULL, ...){
    local_data = reactiveValues(
      all_projects = '',
      project_name = '',
      subject_code = ''
    )

    # eval once
    # To be changed
    local_data$all_projects = potential_projects = get_projects()

    last_inputs = utils$last_inputs()
    if(!is.null(project_name)){
      local_data$project_name = project_name
    }else{
      local_data$project_name = last_inputs$last_project_name
    }
    
    if(!is.null(subject_code)){
      local_data$subject_code = subject_code
    }else{
      local_data$subject_code = last_inputs$last_subject_code
    }
    
    observeEvent(input$project_name, {
      print(input$project_name)
    })
    

    # Reactives
    observe({
      local_data$reset = Sys.time()
      
      # project_name_sel = get_val(input, 'project_name_sel', default = 'New Project...')
      # if( project_name_sel == 'New Project...' ){
      #   utils$clear_subject()
      #   return()
      # }
      project_name = get_val(local_data, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      
      catgl(sprintf(
        'Check whether subject has been imported: %s/%s',
        project_name, subject_code
      ))
      
      if(!is.blank(subject_code) && !is.blank(project_name)){
        # check if subject dir exists
        dirs = get_dir(subject_code = subject_code, project_name = project_name)
        catgl(dirs$preprocess_dir)
        if(dir.exists(dirs$preprocess_dir)){
          # subject exists, load subject data
          utils$load_subject(subject_code = subject_code, project_name = project_name)
          # local_data$project_name = project_name
          local_data$subject_code = subject_code
          showNotification(p('Subject ', subject_code, '(', project_name, ')', ' loaded!'), type = 'message')
          return()
        }
      }
      utils$clear_subject()
      # Update
      updateSelectInput(session, 'blocks', label = 'Folders (sessions or blocks)', selected = NULL, choices = '')
      updateTextInput(session, 'channels', label = 'Electrodes', value = '')
      updateNumericInput(session, 'srate', label = 'Sample Rate', value = 0)
    })

    observeEvent({
      input$save
      input$save1
    }, {
      project_name = get_val(local_data, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      if(!is.blank(subject_code) && !is.blank(project_name)){
        dirs = get_dir(subject_code = subject_code, project_name = project_name)
        
        if(isTRUE(local_data$is_bids)){
          target_bids <- normalizePath(raveio::raveio_getopt('bids_data_dir'), mustWork = FALSE)
          target_bids <- file.path(target_bids, project_name, sprintf('sub-%s', subject_code))
          dirs$bids_subject_dir <- target_bids
          if(dir.exists(dirs$bids_subject_dir)){
            if(!dir.exists(dirs$pre_subject_dir)){
              linked <- file.link(dirs$bids_subject_dir, dirs$pre_subject_dir)
              if(!linked){
                # osx or linux, create symlink
                linked <- file.symlink(dirs$bids_subject_dir, dirs$pre_subject_dir)
              }
              if(!linked){
                showNotification(p('Cannot create sym/hard link from BIDS folder to rave raw folder. Please manually copy subject folder from BIDS path to rave raw path'), type = 'error')
                return()
              }
            }
          }else{
            showNotification(p('Subject ', subject_code, ' NOT found!'), type = 'error')
            return()
          }
        }
        
        if(dir.exists(dirs$pre_subject_dir)){
          
          if(utils$has_subject() &&
             utils$get_from_subject('project_name', '') == project_name &&
             utils$get_from_subject('subject_code', '') == subject_code
          ){
            # Subject already loaded, save!
            n_changes = 3
            if(!utils$set_blocks(input$blocks)){
              blocks = utils$get_from_subject('blocks', NULL)
              updateSelectInput(session, inputId = 'blocks', selected = blocks)
              n_changes = n_changes - 1
            }
            
            if(!utils$set_electrodes(input$channels, name = 'channels')){
              channels = dipsaus::deparse_svec(utils$get_from_subject('channels', NULL))
              updateTextInput(session, inputId = 'channels', value = channels)
              n_changes = n_changes - 1
            }
            
            if(!utils$set_srate(input$srate)){
              srate = utils$get_from_subject('srate', 0)
              updateNumericInput(session, 'srate', value = srate)
              n_changes = n_changes - 1
            }
            
            # Check if subject has been imported or not
            if(!utils$has_raw_cache() && length(utils$get_blocks()) && length(utils$get_electrodes())){
              # Subject is created but not imported
              # Check if channels are matched with subject raw files
              electrodes = utils$get_electrodes()
              blocks = utils$get_blocks()
              pre_dir = utils$get_from_subject('dirs')$pre_subject_dir
              # # check if block maches
              # wrong_blocks = blocks[!blocks %in% list.dirs(pre_dir, recursive = F, full.names = F)]
              # if(length(wrong_blocks)){
              #   showNotification(p('Subject ', subject_code, '(', project_name, ')', ' has invalid block(s) - ', paste(wrong_blocks, collapse = ', '), '!'), type = 'error')
              #   return()
              # }
              # for(b in blocks){
              #   f = list.files(file.path(pre_dir, b), pattern = '_ch[0-9]+.[mM][aA][tT]$')
              #   f = stringr::str_match(f, '_ch([0-9]+).[mM][aA][tT]$')[,2]
              #   f = as.integer(f)
              #   w_e = electrodes[!electrodes %in% f]
              #   if(length(w_e)){
              #     showNotification(p('Subject ', subject_code, '(', project_name, ')', ' missing file for electrode(s) - ',
              #                        dipsaus::deparse_svec(w_e), '! Please check.'), type = 'error')
              #     return()
              #   }
              # }
              # 
              # 
              # utils$collect_raw_voltage()
            }
            
            if(n_changes){
              utils$reset()
              showNotification(p('Subject ', subject_code, '(', project_name, ')', ' saved! ', br(),
                                 'Total ', n_changes, ' changes'), type = 'message')
            }
          }else{
            # No subject or subject changed, discard changes and load
            local_data$all_projects = unique(c(isolate(local_data$all_projects), project_name))
            utils$save_subject(subject_code = subject_code, project_name = project_name)
            showNotification(p('Subject ', subject_code, '(', project_name, ')', ' created/switched!'), type = 'message')
          }
          
          local_data$project_name = project_name
          local_data$subject_code = subject_code
        }else{
          showNotification(p('Subject ', subject_code, ' NOT found!'), type = 'error')
        }        
      }
    })

    output$step_1_msg <- renderUI({
      scode = input$subject_code
      project_name <- local_data$project_name
      raw_dir = normalizePath(rave_options('raw_data_dir'), mustWork = FALSE)
      target_dir = file.path(raw_dir, scode)
      has_scode = length(scode) && scode != ''
      if( !has_scode ){
        return('')
      }
      valid_scode = stringr::str_detect(scode, '^[a-zA-Z0-9][a-zA-Z0-9_]{0,50}$')
      if( !valid_scode ){
        return(p(style='color:red;word-break: break-word;',
                    'Invalid subject code. Only letters (A-Z), numbers (0-9) and "_" are allowed. (Do not start with "_")'))
      }
      has_dir = dir.exists(target_dir)
      if( has_dir ){
        local_data$has_dir = TRUE
        local_data$is_bids = FALSE
        return(p(style='color:green;word-break: break-word;',sprintf('%s (found!)', target_dir)))
      } else {
        
        # check BIDS
        target_bids <- normalizePath(raveio::raveio_getopt('bids_data_dir'), mustWork = FALSE)
        target_bids <- file.path(target_bids, project_name, sprintf('sub-%s', scode))
        if(dir.exists(target_bids)){
          local_data$has_dir = TRUE
          local_data$is_bids = TRUE
          return(p(style='color:green;word-break: break-word;',sprintf('%s (found!)', target_bids)))
        }
        local_data$is_bids = NULL
        local_data$has_dir = FALSE
        return(p(style='color:red;word-break: break-word;',
                 sprintf('%s (NOT found)', target_dir),
                 sprintf('%s (NOT found)', target_bids)))
      }
      
    })

    output$overview_inputs1 <- renderUI({
      textInput(ns('subject_code'), 'Subject Code (subject folder in raw directory)', value = isolate(local_data$subject_code))
    })
    
    output$overview_inputs2 <- renderUI({
      tagList(
        selectInput(ns('project_name_sel'), 'Project', choices = c(
          'New Project...', local_data$all_projects
        ), selected = isolate(local_data$project_name)),
        conditionalPanel(
          sprintf('input[["%s"]] === "New Project..."', ns('project_name_sel')),
          textInput(ns('project_name'), 'Project Name')
        )
      )
    })
    output$overview_inputs3 <- renderUI({
      # # reset = user_data$reset
      # if(is.null(input$project_name_sel) || input$project_name_sel == 'New Project...'){
      #   re = textInput(ns('project_name'), 'Project Name', value = '')
      # }else{
      #   re = div(
      #     class = 'hidden',
      #     textInput(ns('project_name'), 'Project Name', value = input$project_name_sel)
      #   )
      # }
      # re
    })
    
    observeEvent(input$project_name, {
      if(length(input$project_name) && input$project_name != ''){
        local_data$project_name <- input$project_name
      }
    })
    
    observeEvent(input$project_name_sel, {
      if(input$project_name_sel != 'New Project...'){
        local_data$project_name <- input$project_name_sel
      }
    })
    
    
    output$step_2_msg <- renderUI({
      project_name = get_val(local_data, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      
      catgl(sprintf(
        'Check subject path: %s/%s',
        project_name, subject_code
      ))
      
      data_dir = normalizePath(rave_options('data_dir'), mustWork = FALSE)
      if( subject_code == '' ){
        if( project_name != '' ){
          return(p(style='color:red;word-break: break-word;', 'Please enter subject code first!'))
        }
        return()
      }
      
      if( !isTRUE(local_data$has_dir) || project_name == '' ){
        return()
      }
      
      valid_proj = stringr::str_detect(project_name, '^[a-zA-Z0-9][a-zA-Z0-9_]*')
      if( !valid_proj ){
        return(p(style='color:red;word-break: break-word;', 'Project name is not valid. Use letters, digits, and "_" (Do not start with "_")'))
      }
      
      fpath = file.path(data_dir, project_name, subject_code)
      if( dir.exists(fpath) && isTRUE(input$project_name_sel == 'New Project...') ){
        return(p(style = 'color:blue;word-break: break-word;', sprintf(
          'Subject [%s/%s] already exists and loaded',
          project_name, subject_code
        )))
      }
      return(p(style = 'color:green;word-break: break-word;', sprintf(
        'Subject [%s/%s] will be stored at: %s', 
        project_name, subject_code, fpath
      )))
      
    })
    
    output$overview_inputs4 <- renderUI({
      reset = user_data$reset
      local_data$reset
      if( isTRUE(utils$has_subject()) ){
        return()
      }else{
        dipsaus::actionButtonStyled(ns('save1'), 'Create Subject', type = 'primary', width = '100%')
      }
    })

    output$overview_inputs5 <- renderUI({
      reset = user_data$reset
      local_data$reset
      if( !isTRUE(utils$has_subject()) ){
        return(p(style='color:grey;word-break: break-word;',
                    'Please specify subject code, project name and make sure press the "Create Subject" button'))
      }
      last_inputs = utils$last_inputs()
      srate = utils$get_from_subject('srate', default = 0)
      block_choices = utils$get_from_subject('available_blocks', '')
      block_selected = utils$get_from_subject('blocks', NULL)
      channels = dipsaus::deparse_svec(utils$get_from_subject('channels', NULL))
      # exclchan = dipsaus::deparse_svec(utils$get_from_subject('exclchan', NULL))
      # badchan = dipsaus::deparse_svec(utils$get_from_subject('badchan', NULL))
      # epichan = dipsaus::deparse_svec(utils$get_from_subject('epichan', NULL))
      # catgl('gen UI')


      block_label = 'Folders (sessions or blocks)'
      elec_label = 'Electrodes'
      srate_label = 'Sample Rate'
      unit_label = 'Physical unit'
      save_label = 'Update Changes'
      file_format_ui = NULL

      save_type = 'primary';
      btn_cls = ''
      save_id = 'save2'
      if(!utils$has_subject()){
        save_label = 'Create Subject'
        save_id = 'save'
      }else{
        if(utils$has_raw_cache()){
          block_label = 'Folders (sessions or blocks, read-only)'
          elec_label = 'Electrodes (read-only)'
          save_type = 'warning'
        }else{
          save_label = 'Check Subject'
          file_format_ui = tagList(
            selectInput(ns('file_format'), 'File format', choices = file_formats),
            uiOutput(ns('format_demo'))
          )
        }
        if(utils$notch_filtered()){
          srate_label = 'Sample Rate (read-only)'
          save_type = 'warning'
          btn_cls = 'hidden'
        }
      }



      re = shiny::tagList(
        shiny::selectInput(ns('blocks'), block_label, selected = block_selected, choices = block_choices, multiple = TRUE),
        shiny::textInput(ns('channels'), elec_label, placeholder = 'E.g. 1-84', value = channels),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns('srate'), srate_label, value = srate, step = 1L, min = 1L)),
          shiny::column(6, shiny::selectInput(ns('edf_lfp_unit'), unit_label, choices = c('as-is (no change)', 'uV', 'mV', 'V')))
        ),
        file_format_ui,
        dipsaus::actionButtonStyled(ns(save_id), save_label, type = save_type, 
                                    width = '100%', class = btn_cls)
      )


    })
    
    output$format_demo <- renderUI({
      fmt <- input$file_format
      
      if(fmt == file_formats[[1]]){
        div(
          p("In each block folder, one Matlab/HDF5 file stands for one electrode. ", 
            "File name should match with format XXX1.h5 or xxx2.mat. ", 
            "Each file only contains a one-dimensional vector. ",
            "The vector lengths stand for total time points and they must be the same across all electrode files. ",
            "For example:"),
          tags$pre(
            dipsaus::print_directory_tree(
              c('block1', 'block2'),
              root = '<subject folder>',
              child = c(
                'datafile_e1.mat <vector of time>',
                'datafile_e2.mat <same length>',
                'datafile_e3.mat',
                '...'
              ),
              collapse = '\n'
            )
          )
        )
      } else if(fmt == file_formats[[2]]){
        div(p("A single Matlab/HDF5 file containing all electrode information. ",
              "Data must be a matrix. One of the dimension must be electrodes, ",
              "the other dimension must be time points. ",
              "ALL blocks must share the same file & data name; for example:"),
            tags$pre(
              dipsaus::print_directory_tree(
                c('block1', 'block2'),
                root = '<subject folder>',
                child = c(
                  'datafile.mat <one big matrix>'
                ),
                collapse = '\n'
              )
            ))
      }else{
        div(p("In each block folder, one EDF(+) file containing all electrode data; for example:"),
            tags$pre(
              dipsaus::print_directory_tree(
                c('block1', 'block2'),
                root = '<subject folder>',
                child = c(
                  'datafile.edf <ONLY one EDF file per block>'
                ),
                collapse = '\n'
              )
            ))
      }
    })

    output$overview_table <- renderTable({
      reset = user_data$reset
      list(
        `Project Name` = utils$get_from_subject('project_name', NA),
        `Subject Code` = utils$get_from_subject('subject_code', NA),
        `Blocks` = paste(utils$get_from_subject('blocks', ''), collapse = ', '),
        `Channels` = dipsaus::deparse_svec(utils$get_from_subject('channels', NULL))
      ) ->
        tbl
      data.frame(
        Entry = names(tbl),
        Value = unlist(tbl)
      )
    })

    output$subject_log_table <- renderTable({
      reset = user_data$reset
      log = as.data.frame(utils$get_from_subject('log', data.frame(), customized = T))
      log

    })
    
    
    # Check and import subject
    observeEvent(input$save2, {
      local_data$import_valid = TRUE
      local_data$import_checks = NULL
      
      electrodes <- dipsaus::parse_svec(input$channels)
      srate <- input$srate
      blocks <- input$blocks
      file_format <- input$file_format
      project_name = get_val(local_data, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      
      if(!length(blocks)){
        showNotification(p('No block is chosen. At least one block is needed'), type = 'error')
        local_data$import_valid = FALSE
      }
      if(!length(electrodes)){
        showNotification(p('Please enter electrodes to be imported'), type = 'error')
        local_data$import_valid = FALSE
      }
      if(srate <= 0){
        showNotification(p('Sample rate is invalid: must be positive'), type = 'error')
        local_data$import_valid = FALSE
      }
      if(isFALSE(local_data$import_valid)){
        return()
      }
      
      # project_name = 'test'; subject_code = 'YAB'
      utils$check_load_subject(subject_code = subject_code, project_name = project_name)
      
      if(utils$has_raw_cache()){
        showNotification(p('Subject has been imported, please proceed to other modules'), type = 'error')
        local_data$import_valid = FALSE
        return()
      }
      
      utils$set_blocks(blocks, notification = FALSE)
      utils$set_electrodes(electrodes, notification = FALSE)
      utils$set_sample_rate(srate, notification = FALSE)
      
      subject <- Subject$new(subject_code = subject_code, project_name = project_name)
      # subject$.__enclos_env__$private$subjectinfo$available_blocks
      all_blocks <- subject$preprocess_info('available_blocks')
      
      # list2env(as.list(environment()), envir = .GlobalEnv)
      lfp_valid <- raveio::validate_raw_file(
        subject_code = subject_code,
        blocks = blocks,
        electrodes = electrodes,
        format = file_format,
        check_content = TRUE,
        project_name = project_name, 
        data_type = 'continuous'
      )
      
      import_checks <- dipsaus::list_to_fastmap2(list(
        blocks = blocks,
        fmt = file_format,
        lfp_file_format = which(file_formats == file_format),
        lfp_electrodes = electrodes,
        lfp_sample_rate = srate,
        lfp_valid = lfp_valid,
        freeze_lfp = utils$has_raw_cache()
      ))
      
      local_data$import_checks = import_checks
      local_data$input_changed_import <- TRUE
      local_data$input_changed_import2 <- TRUE
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent({
      list(
        local_data$project_name,
        input$subject_code,
        input$blocks,
        input$channels,
        input$srate,
        input$edf_lfp_unit,
        input$file_format
      )
    }, {
      local_data$input_changed_import <- Sys.time()
    })
    
    
    # Import widgets
    output$lfp_import <- renderUI({
      main_results <- local_data$import_checks
      last_time <- local_data$input_changed_import
      last_time2 <- local_data$input_changed_import2
      # assign('main_results', main_results, envir = globalenv())
      if(!inherits(main_results, 'fastmap2')){
        return(shiny::div(
          'Please validate data first. If you have imported subject, please proceed to Notch filter',
          class = 'shiny-bound-output shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation'
        ))
      }
      if(!isTRUE(local_data$import_valid)){
        return(shiny::div(
          'Please enter valid blocks, electrodes, and sample rate to check & validate first.',
          class = 'shiny-bound-output shiny-output-error shiny-output-error-shiny.silent.error shiny-output-error-validation'
        ))
      }
      
      project_name = local_data$project_name
      project_name %?<-% ''
      subject_code = input$subject_code
      subject_code %?<-% ''
      utils$check_load_subject(subject_code = subject_code, project_name = project_name)
      
      if(isTRUE(main_results$freeze_lfp)){
        return('Subject has been imported.')
      }
      
      lfp_valid <- main_results$lfp_valid
      if( !lfp_valid && inherits(lfp_valid, 'validate_failure') ){
        reason <- attr(lfp_valid, 'reason')
        reason_name <- names(reason)
        error_ui <- lapply(seq_along(reason_name), function(ii){
          shiny::tagList(
            shiny::hr(),
            reason_name[[ii]],
            shiny::tags$ul(
              lapply(reason[[ii]], function(item){
                shiny::tags$li(item)
              })
            )
          )
        })
        return(shiny::p(
          shiny::h5('Issue(s) found while checking raw data:'),
          shiny::tags$small(
            'Please make sure ', 
            shiny::strong('File format'), 
            'option is proper and resolve the following potential issues.'
          ),
          error_ui
        ))
      }
      
      # Show import widgets
      main_results$lfp_sample_rate
      main_results$lfp_electrodes
      
      blocks <- utils$get_blocks()
      snapshot <- attr(lfp_valid, 'snapshot')
      if(!is.null(snapshot)){
        snapshot <- shiny::HTML(snapshot)
      }
      
      first_block <- blocks[[1]]
      
      run_ui <- NULL
      if(!main_results$lfp_file_format %in% 1:4) {
        # BIDS format
        valid_runs <- attr(lfp_valid, 'valid_run_names')
        selected <- isolate(input$bids_runs)
        selected <- selected[selected %in% valid_runs]
        if(!length(selected)){
          selected <- valid_runs
        }
        run_ui <- shiny::tagList(
          hr(),
          HTML('Choose BIDS <strong>session+task+runs</strong> to include'),
          shiny::selectInput(ns('bids_runs'), 'Please select to import',
                             choices = valid_runs, multiple = TRUE, selected = selected),
        )
      }
      
      
      if(main_results$lfp_file_format %in% c(3:6)){
        color <- 'color: red;'
      } else {
        color = ''
      }
      
      if(isTRUE(last_time == last_time2)){
        btn <- dipsaus::actionButtonStyled(ns('btn_import_lfp_btn'), 'Start import')
      } else{
        btn <- span(style = 'color:grey', 'Input changed, please redo validation.')
      }
      
      shiny::p(
        shiny::span(style = sprintf("font-weight: 900; %s", color), 
                    'Please check the following information and make sure your configuration is consistent.'),
        shiny::hr(),
        'Meta information obtained from the first data file:',
        br(),
        snapshot,
        run_ui,
        shiny::hr(),
        sprintf('Data to be imported: %d block(s) x %d electrode(s) at sample rate %.1f Hz.',
                length(blocks), length(main_results$lfp_electrodes), main_results$lfp_sample_rate),
        br(),
        btn
      )
      
    })
    
    observeEvent(input$btn_import_lfp_btn, {
      shiny::showModal(shiny::modalDialog(
        title = 'Confirm importing data',
        shiny::p('Please comfirm importing data. This procedure cannot be undone.'),
        size = 's', footer = tagList(
          shiny::modalButton("I'll double check"),
          dipsaus::actionButtonStyled(ns('btn_import_lfp'), "Let's go!")
        )
      ))
    })
    
    observeEvent(input$btn_import_lfp, {
      main_results <- local_data$import_checks
      if(!inherits(main_results, 'fastmap2') || !isTRUE(local_data$import_valid)){
        showNotification(p('Some information changed. Please perform validation again.'), type = 'error')
      }
      lfp_valid <- main_results$lfp_valid
      if(!lfp_valid){
        showNotification(p('Failed validation. Please check your raw data'), type = 'error')
      }
      
      project_name = get_val(local_data, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      utils$check_load_subject(subject_code = subject_code, project_name = project_name)
      
      if(utils$has_raw_cache()){
        showNotification(p('Data was imported. Cannot import again'), type = 'error')
        return()
      }
      
      electrodes <- dipsaus::parse_svec(input$channels)
      srate <- input$srate
      blocks <- input$blocks
      fmt <- input$file_format
      
      utils$set_blocks(blocks)
      utils$set_electrodes(electrodes)
      utils$set_sample_rate(srate)
      dirs = utils$get_from_subject('dirs', list(), customized = FALSE)
      file = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
      
      if(file.exists(file)){
        unlink(file)
      }
      
      edf_lfp_unit <- input$edf_lfp_unit
      if(length(edf_lfp_unit) == 1){
        lfp_unit <- list('mV' = 'mV', 'V' = 'V', 'uV' = 'uV', 
                         'as-is (no change)' = 'NA')[[edf_lfp_unit]]
        if(!isTRUE(lfp_unit %in% c('NA', 'V', 'mV', 'uV'))){
          lfp_unit <- 'NA'
        }
      } else {
        lfp_unit <- 'NA'
      }
      
      
      
      bids_runs <- input$bids_runs
      
      # BIDS?
      if(fmt %in% file_formats[5:6]){
        if(!length(bids_runs)){
          showNotification(p('Must specify runs to import from BIDS format'), type = 'error')
          return()
        }
        # force set blocksbecause the structure is BIDS not native rave
        utils$set_blocks(bids_runs, force = TRUE)
      } else {
        bids_runs <- NULL
      }
      
      raveio::rave_import(project_name = project_name, subject_code = subject_code,
                          blocks, electrodes, format = fmt, 
                          sample_rate = srate, conversion = lfp_unit, task_runs = bids_runs)
      utils$save_to_subject(checklevel = HAS_CACHE)
      utils$reset()
      msg = 'Raw voltage signals are cached.'
      type = 'message'
      utils$showNotification(msg, type = type)
      shiny::removeModal()
      
    })
    
  }

  return(list(
    body = body,
    server = server
  ))
}
