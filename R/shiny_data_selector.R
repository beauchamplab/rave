#' @export
check_subjects <- function(
  project_name, subject_code, check = T,
  folders = c('Subject Folder', 'RAVE Folder', 'Preprocessing Folder', 'Meta Folder', 'Channel Folder'),
  preprocess = c('Started Proprocess', 'Notch Filter', 'Wavelet'),
  Meta = c("Electrode File", "Time point File", "Frequency File", "Epoch File")
){
  utils = rave_preprocess_tools()
  miss_subject_code = missing(subject_code)
  miss_project_name = missing(project_name)
  if(miss_project_name){
    projects = get_projects()
  }else{
    projects = project_name
  }
  sapply(projects, function(project_name){
    if(miss_subject_code){
      sc = list.dirs(file.path(rave_options('data_dir'), project_name), full.names = F, recursive = F)
    }else{
      sc = subject_code
    }
    sapply(sc, function(subject_code){
      if(!check){
        return(project_name %&% '/' %&% subject_code)
      }

      re = utils$check_load_subject(subject_code = subject_code, project_name = project_name)
      errs = list()
      l = unlist(re$Folders[folders]); l = l[!l]
      if(length(l)){
        errs[['Subject hierarchy is wrong. Please check existence of following folders: ']] = names(l)
      }

      l = unlist(re$Preprocess[preprocess]); l = l[!l]
      if(length(l)){
        errs[['Preprocess is needed: ']] = names(l)
      }

      l = unlist(re$Meta[Meta]); l = l[!l]
      if(length(l)){
        errs[['Subject meta file missing: ']] = names(l)
      }

      errs
    }, simplify = F, USE.NAMES = T)
  }, simplify = F, USE.NAMES = T) ->
    re
  if(!miss_project_name && !miss_subject_code){
    if(length(re[[1]][[1]])){
      error = re[[1]][[1]]
    }else{
      error = FALSE
    }
    re = list(
      adapter = utils,
      error = error
    )
  }
  re
}

#' @export
last_entry <- function(key, default, save = F, group = 'customized'){
  assertthat::assert_that(is.character(key), msg = 'Key must be a string')
  dict = rave_hist$get_or_save(key = group, val = list(), save = F)
  val = dict[[key]]
  val %?<-% default
  if(save){
    dict[[key]] = default
    arg = list(dict); names(arg) = group
    do.call(rave_hist$save, arg)
    return(invisible(val))
  }else{
    return(val)
  }
}

# data selector
shiny_data_selector <- function(moduleId){
  ns = shiny::NS(moduleId)

  header = function(){
    tags$li(
      actionLink(ns('data_select'), 'Select Data', icon = shiny::icon('tasks'),
                 role = 'button', class = 'nav-item nav-link')
    )
  }
  control = function(){
    NULL
  }
  server = function(input, output, session, global_reactives){
    group = 'main_app'
    local_data = reactiveValues(
      error = TRUE,
      error_info = list(),
      last_subject = FALSE
    )

    output$modal_error <- renderUI({
      if(local_data$error){
        error_msg = local_data$error_info
        tagList(
          tagList(
            lapply(names(error_msg), function(nm){
              p(
                nm,
                tags$ul(
                  tagList(
                    lapply(error_msg[[nm]], tags$li)
                  )
                )
              )
            })
          )
        )
      }else{
        NULL
      }
    })
    output$modal_electrodes <- renderUI({
      validate(need(!local_data$error, message = ''))
      is_last = local_data$last_subject
      if(is_last){
        e = last_entry('electrodes', NULL, group = group)
      }
      e %?<-% local_data$electrodes

      if(length(e) > 1){
        e = rave:::deparse_selections(e)
      }

      rs = local_data$references

      tagList(
        hr(),
        textInput(ns('electrode_text'), 'Electrodes:', value = e, placeholder = 'E.g. 1-5,8,11-20'),
        selectInput(ns('reference'), 'Reference Table:', selected = 'default', choices = rs)
      )
    })
    output$modal_epochs <- renderUI({
      validate(need(!local_data$error, message = ''))
      is_last = local_data$last_subject


      es = local_data$epochs
      e = last_entry('epoch', es[1], group = group)
      if(!e %in% es){
        e = es[1]
      }
      e_range = last_entry('epoch_range', c(1,2), group = group)

      tagList(
        hr(),
        selectInput(ns('epoch'), 'Epoch Selection', choices = es, selected = e),
        div(
          class = 'rave-grid-inputs',
          div(
            style = 'flex-basis:50%; max-width:50%',
            numericInput(ns('pre'), 'Before onset', value = e_range[1], min = 0, step = 0.01)
          ),
          div(
            style = 'flex-basis:50%; max-width:50%',
            numericInput(ns('post'), 'After onset', value = e_range[2], min = 0, step = 0.01)
          )
        )
      )
    })
    output$plot3dui <- renderUI({
      if(!local_data$error){
        plotly::plotlyOutput(ns('plot3d'))
      }
    })
    output$plot3d <- plotly::renderPlotly({
      if(!local_data$error){
        project_name = input$project_name
        subject_code = input$subject_code
        loaded_electrodes = rave:::parse_selections(input$electrode_text)
        tbl = load_meta('electrodes', project_name = project_name, subject_code = subject_code)
        try({
          rave:::render_3d_electrodes(tbl = tbl, loaded_electrodes = loaded_electrodes)
        }, silent = T)
      }
    })

    # Update subject_code according to project name
    observe({
      pn = input$project_name
      if(length(pn) == 1){
        subjects = get_subjects(project_name = pn)
        if(!length(subjects)){
          subjects = ''
        }
        sc = last_entry('subject_code', '', group = group)
        if(subjects == '' && !is.null(sc)){
          subject = sc
        }
        if(!sc %in% subjects){
          sc = subjects[1]
          local_data$last_subject = FALSE
        }else{
          local_data$last_subject = TRUE
        }
        updateSelectInput(session, 'subject_code', choices = subjects, selected = sc)
      }
    }, priority = 10000L)

    # Ckeck subject validity
    observe({
      project_name = input$project_name
      subject_code = input$subject_code
      if(length(subject_code) != 1 || subject_code == '') return()
      # check subject
      check_subjects(
        project_name = project_name,
        subject_code = subject_code) ->
        subject_info
      if(!is.logical(subject_info$error) ||subject_info$error != FALSE){
        local_data$error = TRUE
        local_data$error_info = subject_info$error
      }else{
        local_data$error = FALSE
        local_data$electrodes = subject_info$adapter$get_electrodes()
        local_data$epochs = subject_info$adapter$epochs()

        dirs = subject_info$adapter$get_from_subject('dirs')
        local_data$data_types = c('power', 'phase', 'voltage', list.dirs(dirs$module_data_dir, full.names = F, recursive = F))
        local_data$references = subject_info$adapter$references()
      }
    })

    data_modal = function(){
      subject_list = check_subjects(check = F)
      subject_ids = unlist(subject_list, use.names = F)

      subjects = get_subjects(project_name = last_entry('project_name', NULL, group = group))
      if(length(subjects) == 0){ subjects = '' }

      shiny::modalDialog(
        title = 'Data Pre-loading',
        easyClose = T,
        size = 'l',
        fluidRow(
          column(
            width = 3,
            selectInput(
              ns('project_name'),
              'Project Name:',
              choices = get_projects(),
              multiple = F,
              selected = last_entry('project_name', NULL, group = group)
            ),
            selectInput(
              ns('subject_code'),
              'Subject Code:',
              choices = subjects,
              selected = last_entry('subject_code', NULL, group = group)
            ),
            uiOutput(ns('modal_electrodes')),
            uiOutput(ns('modal_epochs'))
          ),
          column(
            width = 7,
            uiOutput(ns('modal_error')),
            uiOutput(ns('plot3dui'))
          ),
          column(
            width = 2,
            div(
              style = 'max-height: 60vh; overflow-y: scroll;',
              uiOutput(ns('modal_data'))
            )
          )
        ),
        footer = tagList(
          shiny::modalButton('Cancel'),
          actionButton(ns('data_import'), label = 'Import')
        )
      )
    }

    output$modal_data <- renderUI({
      if(!local_data$error){
        l = local_data$data_types
        last_data_types = last_entry('data_types', NULL, group = group)
        last_data_types = last_data_types[last_data_types %in% l]

        checkboxGroupInput(ns('data_types'), "Preloaded Data Types:", choices = l, selected = last_data_types)
      }else{
        NULL
      }
    })


    observeEvent(input$data_select, {
      shiny::showModal(data_modal())
    })

    observeEvent(input$data_import, {
      error = local_data$error
      electrodes = rave:::parse_selections(input$electrode_text)
      if(error){
        showNotification('This is an invalid subject. Please check data integrity.', type = 'error')
        return(NULL)
      }

      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error')
        return(NULL)
      }
      data_types = input$data_types
      subject_code = input$subject_code
      project_name = input$project_name
      epoch = input$epoch
      epoch_range = c(input$pre, input$post)
      reference = input$reference
      subject_id = project_name %&% '/' %&% subject_code

      tmp_subject = Subject$new(project_name = project_name, subject_code = subject_code, reference = reference)
      electrodes = tmp_subject$filter_valid_electrodes(electrodes)
      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error')
        return(NULL)
      }

      rave_prepare(
        subject = subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        reference = reference,
        attach = F,
        data_types = data_types
      )
      # register

      last_entry('project_name', project_name, save = T, group = group)
      last_entry('subject_code', subject_code, save = T, group = group)
      last_entry('electrodes', electrodes, save = T, group = group)
      last_entry('epoch', epoch, save = T, group = group)
      last_entry('epoch_range', epoch_range, save = T, group = group)
      last_entry('data_types', data_types, save = T, group = group)

      global_reactives$force_refresh_all = Sys.time()
      global_reactives$has_data = Sys.time()
      removeModal()
    })

    # onload, check if data has been loaded into datarepo
    data_env = getDefaultDataRepository()
    data_loaded = rlang::env_has(
      data_env,
      nms = c(
        ".private",
        "data_check",
        "module_tools",
        "preload_info",
        "subject"
      ),
      inherit = F
    )
    if(!(FALSE %in% data_loaded)){
      global_reactives$force_refresh_all = Sys.time()
      global_reactives$has_data = Sys.time()
    }


  }



  return(list(
    header = header,
    control = control,
    server = server
  ))
}


# Test
if(FALSE){
  re = shiny_data_selector('A')
  shinyApp(
    ui = shiny::basicPage(re$header()),
    server = function(input, output, session){
      global_reactives = reactiveValues()
      callModule(re$server, id = 'A', session = session, global_reactives = global_reactives)
    }
  )
}
