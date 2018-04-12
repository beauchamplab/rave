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
            uiOutput(ns('overview_inputs2')),

            actionButton(ns('save'), 'Update Changes')
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
          h2('Channel Settings'),
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
    potential_projects = str_split_fixed(list.dirs(rave_options('data_dir'), full.names = F, recursive = F), '_', 2)[,2]
    local_data$all_projects = potential_projects

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
        }
      }
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

            if(!utils$set_channels(input$channels, name = 'channels')){
              channels = rave:::deparse_selections(utils$get_from_subject('channels', NULL))
              updateTextInput(session, inputId = 'channels', value = channels)
              n_changes = n_changes - 1
            }

            if(!utils$set_srate(input$srate)){
              srate = utils$get_from_subject('srate', 0)
              updateNumericInput(session, 'srate', value = srate)
              n_changes = n_changes - 1
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
      channels = rave:::deparse_selections(utils$get_from_subject('channels', NULL))
      exclchan = rave:::deparse_selections(utils$get_from_subject('exclchan', NULL))
      badchan = rave:::deparse_selections(utils$get_from_subject('badchan', NULL))
      epichan = rave:::deparse_selections(utils$get_from_subject('epichan', NULL))
      logger('gen UI')


      if(is.null(input$project_name_sel) || input$project_name_sel == 'New Project...'){
        tagList(
          textInput(ns('project_name'), 'Project Name', value = ''),
          selectInput(ns('blocks'), 'Blocks', selected = block_selected, choices = block_choices, multiple = T),
          textInput(ns('channels'), 'Channels', placeholder = 'E.g. 1-84', value = channels),
          numericInput(ns('srate'), 'Sample Rate', value = srate, step = 1L, min = 1L),
          textInput(ns('exclchan'), 'Excluded Channels', placeholder = 'E.g. 51,44-45', value = exclchan),
          textInput(ns('badchan'), 'Bad Channels', placeholder = 'E.g. 3,4,5,11-20', value = badchan),
          textInput(ns('epichan'), 'Epilepsy Channels', placeholder = 'E.g. 1-10', value = epichan)
        )
      }else{
        tagList(
          div(
            class = 'hidden',
            textInput(ns('project_name'), 'Project Name', value = input$project_name_sel)
          ),
          selectInput(ns('blocks'), 'Blocks', selected = block_selected, choices = block_choices, multiple = T),
          textInput(ns('channels'), 'Channels', placeholder = 'E.g. 1-84', value = channels),
          numericInput(ns('srate'), 'Sample Rate', value = srate, step = 1L, min = 1L),
          textInput(ns('exclchan'), 'Excluded Channels', placeholder = 'E.g. 51,44-45', value = exclchan),
          textInput(ns('badchan'), 'Bad Channels', placeholder = 'E.g. 3,4,5,11-20', value = badchan),
          textInput(ns('epichan'), 'Epilepsy Channels', placeholder = 'E.g. 1-10', value = epichan)
        )
      }


    })

    output$overview_table <- renderTable({
      reset = user_data$reset
      list(
        `Project Name` = utils$get_from_subject('project_name', NA),
        `Subject Code` = utils$get_from_subject('subject_code', NA),
        `Blocks` = paste(utils$get_from_subject('blocks', ''), collapse = ', '),
        `Channels` = rave:::deparse_selections(utils$get_from_subject('channels', NULL))
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
