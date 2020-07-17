rave_pre_import_widget3 <- function(module_id = 'IMPORTWIDGETS_M', sidebar_width = 2, 
                                    doc_prefix = 'ravepreprocessimportwidgets'){
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  file_formats <- c(
    '.mat/.h5 file per electrode',
    'Single .mat/.h5 file per block',
    'Plain text'
  )
  
  body = shiny::fluidRow(
    box(
      width = sidebar_width,
      title = 'General',
      box_link = sprintf(url_format, 'input', 'general'),
      
      div(
        class = 'rave-grouped-inputs rave-grid-inputs margin-top-20',
        div(class='rave-grid-inputs-legend', 'Step 1'),
        div(
          style = 'flex-basis: 100%;',
          textInput(ns('subject_code'), 'Subject code (subject folder in raw directory)'),
          tags$small(uiOutput(ns('step_1_msg'))),
          
          selectInput(ns('project_name'), 'Project name', choices = character(0)),
          conditionalPanel(
            condition = sprintf('input["%s"] === "[New project]"', ns('project_name')),
            textInput(ns('new_project_name'), 'Enter a new name')
          )
        )
      ),
      hr(),
      selectInput(ns('blocks'), 'Sessions/Blocks', choices = character(0)),
      selectInput(ns('lfp_file_format'), 'File format', choices = file_formats),
      
    )
  )
  
  server = function(input, output, session, user_data, utils, project_name = NULL, subject_code = NULL, ...){
    local_data = reactiveValues()
    
    raw_dir <- normalizePath(rave_options('raw_data_dir'), mustWork = FALSE)
    
    output$step_1_msg <- renderUI({
      scode = input$subject_code
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
    
    
    
    
    # Initialize
    all_projects <- c(get_projects(), '[New project]')
    updateSelectInput(session, 'project_name', choices = all_projects, selected = project_name)
    updateTextInput(session, 'subject_code', value = subject_code)
    
    
  }
  
  return(list(
    body = body,
    server = server
  ))
}
