#' Preprocess Module - Overview
#' @param module_id internally used
#' @param sidebar_width sidebar width from 1 to 12
#' @export
rave_pre_overview3 <- function(module_id = 'OVERVIEW_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    shinydashboard::tabBox(
      id = ns('sidebar'),
      width = sidebar_width,
      shiny::tabPanel(
        title = 'Overview',
        fluidRow(
          column(
            width = 12,
            uiOutput(ns('overview_inputs1')),
            uiOutput(ns('overview_inputs2'))
          )
        )
      )
    ),
    shinydashboard::box(
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



  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      all_projects = '',
      project_name = '',
      subject_code = ''
    )

    # eval once
    # To be changed
    local_data$all_projects = potential_projects = get_projects()

    last_inputs = utils$last_inputs()
    local_data$project_name = last_inputs$last_project_name
    local_data$subject_code = last_inputs$last_subject_code

    # Reactives
    observe({
      project_name = get_val(input, 'project_name', default = '')
      subject_code = get_val(input, 'subject_code', default = '')
      if(!is.blank(subject_code) && !is.blank(project_name)){
        # check if subject dir exists
        dirs = get_dir(subject_code = subject_code, project_name = project_name)
        logger(dirs$preprocess_dir)
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
      updateActionButton(session, 'save', label = 'Create Subject')
    })

    observeEvent(input$save, {
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
              channels = deparse_selections(utils$get_from_subject('channels', NULL))
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
                                     deparse_selections(w_e), '! Please check.'), type = 'error')
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


    output$overview_inputs1 <- renderUI({

      tagList(
        textInput(ns('subject_code'), 'Subject Code', value = isolate(local_data$subject_code)),
        selectInput(ns('project_name_sel'), 'Project', choices = c(
          'New Project...', local_data$all_projects
        ), selected = isolate(local_data$project_name))
      )
    })

    output$overview_inputs2 <- renderUI({
      reset = user_data$reset
      last_inputs = utils$last_inputs()
      srate = utils$get_from_subject('srate', default = 0)
      block_choices = utils$get_from_subject('available_blocks', '')
      block_selected = utils$get_from_subject('blocks', NULL)
      channels = deparse_selections(utils$get_from_subject('channels', NULL))
      # exclchan = deparse_selections(utils$get_from_subject('exclchan', NULL))
      # badchan = deparse_selections(utils$get_from_subject('badchan', NULL))
      # epichan = deparse_selections(utils$get_from_subject('epichan', NULL))
      # logger('gen UI')



      if(is.null(input$project_name_sel) || input$project_name_sel == 'New Project...'){
        re = tagList(
          textInput(ns('project_name'), 'Project Name', value = '')
        )
      }else{
        re = tagList(
          div(
            class = 'hidden',
            textInput(ns('project_name'), 'Project Name', value = input$project_name_sel)
          )
        )
      }

      block_label = 'Blocks'
      elec_label = 'Electrodes'
      srate_label = 'Sample Rate'
      save_label = 'Update Changes'

      if(!utils$has_subject()){
        save_label = 'Create Subject'
      }else{
        if(utils$has_raw_cache()){
          block_label = 'Blocks (read-only)'
          elec_label = 'Electrodes (read-only)'
        }else{
          save_label = 'Import Subject'
        }
        if(utils$notch_filtered()){
          srate_label = 'Sample Rate (read-only)'
        }
      }



      re = tagList(
        re,
        selectInput(ns('blocks'), block_label, selected = block_selected, choices = block_choices, multiple = T),
        textInput(ns('channels'), elec_label, placeholder = 'E.g. 1-84', value = channels),
        numericInput(ns('srate'), srate_label, value = srate, step = 1L, min = 1L),
        actionButton(ns('save'), save_label)
      )


    })

    output$overview_table <- renderTable({
      reset = user_data$reset
      list(
        `Project Name` = utils$get_from_subject('project_name', NA),
        `Subject Code` = utils$get_from_subject('subject_code', NA),
        `Blocks` = paste(utils$get_from_subject('blocks', ''), collapse = ', '),
        `Channels` = deparse_selections(utils$get_from_subject('channels', NULL))
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
