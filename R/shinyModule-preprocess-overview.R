rave_pre_overview3 <- function(module_id = 'OVERVIEW_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    box(
      # id = ns('sidebar'),
      width = sidebar_width,
      # shiny::tabPanel(
      title = 'Overview',
      fluidRow(
        column(
          width = 12,
          a(href = 'https://openwetware.org/wiki/Beauchamp:RAVE:Data_Formats', target = '_blank',
            'Please check this to import data to RAVE', shiny::icon('external-link')),
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
    box(
      width = 12 - sidebar_width,
      title = 'Overview',
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
    
    

    # Reactives
    observe({
      local_data$reset = Sys.time()
      # project_name_sel = get_val(input, 'project_name_sel', default = 'New Project...')
      # if( project_name_sel == 'New Project...' ){
      #   utils$clear_subject()
      #   return()
      # }
      project_name = get_val(input, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      
      if(!is.blank(subject_code) && !is.blank(project_name)){
        # check if subject dir exists
        dirs = get_dir(subject_code = subject_code, project_name = project_name)
        cat2(dirs$preprocess_dir)
        if(dir.exists(dirs$preprocess_dir)){
          # subject exists, load subject data
          utils$load_subject(subject_code = subject_code, project_name = project_name)
          local_data$project_name = project_name
          local_data$subject_code = subject_code
          showNotification(p('Subject ', subject_code, '(', project_name, ')', ' loaded!'), type = 'message')
          return()
        }
      }
      utils$clear_subject()
      # Update
      updateSelectInput(session, 'blocks', label = 'Block', selected = NULL, choices = '')
      updateTextInput(session, 'channels', label = 'Electrodes', value = '')
      updateNumericInput(session, 'srate', label = 'Sample Rate', value = 0)
    })

    observeEvent({
      input$save
      input$save1
    }, {
      project_name = get_val(input, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      if(!is.blank(subject_code) && !is.blank(project_name)){
        dirs = get_dir(subject_code = subject_code, project_name = project_name)
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
              # check if block maches
              wrong_blocks = blocks[!blocks %in% list.dirs(pre_dir, recursive = F, full.names = F)]
              if(length(wrong_blocks)){
                showNotification(p('Subject ', subject_code, '(', project_name, ')', ' has invalid block(s) - ', paste(wrong_blocks, collapse = ', '), '!'), type = 'error')
                return()
              }
              for(b in blocks){
                f = list.files(file.path(pre_dir, b), pattern = '_ch[0-9]+.[mM][aA][tT]$')
                f = stringr::str_match(f, '_ch([0-9]+).[mM][aA][tT]$')[,2]
                f = as.integer(f)
                w_e = electrodes[!electrodes %in% f]
                if(length(w_e)){
                  showNotification(p('Subject ', subject_code, '(', project_name, ')', ' missing file for electrode(s) - ',
                                     dipsaus::deparse_svec(w_e), '! Please check.'), type = 'error')
                  return()
                }
              }


              utils$collect_raw_voltage()
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
      if( !has_dir ){
        local_data$has_dir = FALSE
        return(p(style='color:red;word-break: break-word;',sprintf('%s (NOT found)', target_dir)))
      }
      local_data$has_dir = TRUE
      return(p(style='color:green;word-break: break-word;',sprintf('%s (found!)', target_dir)))
    })

    output$overview_inputs1 <- renderUI({
      textInput(ns('subject_code'), 'Subject Code (subject folder in raw directory)', value = isolate(local_data$subject_code))
    })
    
    output$overview_inputs2 <- renderUI({
      selectInput(ns('project_name_sel'), 'Project', choices = c(
        'New Project...', local_data$all_projects
      ), selected = isolate(local_data$project_name))
    })
    output$overview_inputs3 <- renderUI({
      # reset = user_data$reset
      if(is.null(input$project_name_sel) || input$project_name_sel == 'New Project...'){
        re = textInput(ns('project_name'), 'Project Name', value = '')
      }else{
        re = div(
          class = 'hidden',
          textInput(ns('project_name'), 'Project Name', value = input$project_name_sel)
        )
      }
      re
    })
    
    output$step_2_msg <- renderUI({
      project_name = get_val(input, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
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
      # cat2('gen UI')


      block_label = 'Blocks'
      elec_label = 'Electrodes'
      srate_label = 'Sample Rate'
      save_label = 'Update Changes'

      save_type = 'primary';
      btn_cls = ''
      if(!utils$has_subject()){
        save_label = 'Create Subject'
      }else{
        if(utils$has_raw_cache()){
          block_label = 'Blocks (read-only)'
          elec_label = 'Electrodes (read-only)'
          save_type = 'warning'
        }else{
          save_label = 'Import Subject'
        }
        if(utils$notch_filtered()){
          srate_label = 'Sample Rate (read-only)'
          save_type = 'warning'
          btn_cls = 'hidden'
        }
      }



      re = shiny::tagList(
        shiny::selectInput(ns('blocks'), block_label, selected = block_selected, choices = block_choices, multiple = T),
        shiny::textInput(ns('channels'), elec_label, placeholder = 'E.g. 1-84', value = channels),
        shiny::numericInput(ns('srate'), srate_label, value = srate, step = 1L, min = 1L),
        dipsaus::actionButtonStyled(ns('save'), save_label, type = save_type, 
                                    width = '100%', class = btn_cls)
      )


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
  }

  return(list(
    body = body,
    server = server
  ))
}
