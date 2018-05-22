check_subjects <- function(
  project_name, subject_code, check = T,
  folders = c('Subject Folder', 'RAVE Folder', 'Preprocessing Folder', 'Meta Folder', 'Channel Folder'),
  preprocess = c('Started Proprocess', 'Notch Filter', 'Wavelet'),
  Meta = c("Trial File", "Electrode File", "Time point File", "Frequency File", "Epoch File")
){
  utils = rave_preprocess_tools()
  miss_subject_code = missing(subject_code)
  miss_project_name = missing(project_name)
  if(miss_project_name){
    projects = list.dirs(rave_options('data_dir'), full.names = F, recursive = F)
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



# data selector
shiny_data_selector <- function(moduleId){
  ns = shiny::NS(moduleId)


  # subject_info = check_subjects('Congruency1', 'YAB')

  # get projects - subject list


  #
  #
  # subject_choices = subject_choices[subject_exists]
  #
  # get_epochs = function(subject_id){
  #   dirs = get_dir(subject_id = subject_id)
  #   epochs = list.files(dirs$meta_dir, pattern = 'epoch_[a-zA-Z0-9]+.csv')
  #   if(length(epochs)){
  #     epochs = str_match(epochs, 'epoch_([a-zA-Z0-9]+).csv')[,2]
  #     return(epochs)
  #   }else{
  #     return('')
  #   }
  # }

  header = function(){
    tags$li(
      actionLink(ns('data_select'), 'Select Data', icon = shiny::icon('tasks'),
                 role = 'button', class = 'nav-item nav-link')
    )
  }
  control = function(){
    # tags$li(
    #   actionLink(ns('auto_calculate'), 'Auto-Calculation ON',
    #              icon = shiny::icon('calculator'), role = 'button'),
    #   class = 'control-sidebar-items'
    # )
    NULL
  }
  server = function(input, output, session, global_reactives){
    last_entry = function(key, default = NULL, check = FALSE){
      re = rave_hist$get_or_save(key = 'main_app', val = list())
      if(!is.list(re)){
        rave_hist$save(main_app = list())
        re = list()
      }
      if(missing(key)){
        return(re)
      }else{
        re = re[[key]]
        if(is.null(re) || (check != FALSE && !re %in% check)) {
          re = default
        }
        return(re)
      }
    }

    observe({
      subject_id = input$subject_id
      last_subject_id = last_entry('current_subject_id', default = '')
      if(length(subject_id) == 1){
        sinfo = unlist(str_split(subject_id, '/'))
        sinfo = check_subjects(sinfo[1], sinfo[2])
        local_data$error = sinfo$error
        # init
        local_data$channels = NULL
        local_data$last_channels = NULL
        local_data$epochs = NULL
        local_data$last_epoch = NULL
        local_data$rave_dir = NULL

        if(is.logical(sinfo$error) && sinfo$error == FALSE){
          utils = sinfo$adapter
          local_data$channels = utils$get_channels()
          local_data$epochs = epochs = utils$get_epochs()
          local_data$rave_dir = utils$get_from_subject('dirs')

          if(last_subject_id == subject_id){
            local_data$last_channels = last_entry('current_electrodes')
            local_data$last_epoch = last_entry('current_epoch', check = epochs)
          }
        }
      }
    })

    output$modal_error <- renderUI({
      error_msg = local_data$error
      if(!is.logical(error_msg) || error_msg != FALSE){
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
      error_msg = local_data$error
      validate(need(is.logical(error_msg) && error_msg == FALSE, message = ''))
      e = local_data$last_channels
      e %?<-% local_data$channels
      current_electrodes = rave:::deparse_selections(e)
      tagList(
        hr(),
        textInput(ns('electrode_text'), 'Electrodes', value = current_electrodes, placeholder = 'E.g. 1-5,8,11-20')
      )
    })

    output$modal_epochs <- renderUI({
      error_msg = local_data$error
      validate(need(is.logical(error_msg) && error_msg == FALSE, message = ''))
      e = local_data$last_epoch
      es = local_data$epochs
      e_range = last_entry('current_epoch_range', c(1,2))

      tagList(
        hr(),
        selectInput(ns('epoch'), 'Epoch Selection', choices = es, selected = e),
        fluidRow(
          column(6, numericInput(ns('pre'), 'Before onset', value = e_range[1], min = 0, step = 0.01)),
          column(6, numericInput(ns('post'), 'After onset', value = e_range[2], min = 0, step = 0.01))
        )
      )
    })

    output$plot3dui <- renderUI({
      error_msg = local_data$error
      validate(need(is.logical(error_msg) && error_msg == FALSE, message = ''))
      plotly::plotlyOutput(ns('plot3d'))
    })

    output$plot3d <- plotly::renderPlotly({
      error_msg = local_data$error
      if(is.logical(error_msg) && error_msg == FALSE){
        sid = input$subject_id
        loaded_electrodes = rave:::parse_selections(input$electrode_text)
        tbl = load_meta('electrodes', subject_id = sid)
        try({
          rave:::render_3d_electrodes(tbl = tbl, loaded_electrodes = loaded_electrodes)
        }, silent = T)
      }
    })

    env = new.env()

    local_data = reactiveValues(
      error = FALSE,
      channels = NULL
    )

    data_modal = function(){
      subject_list = check_subjects(check = F)
      subject_ids = unlist(subject_list, use.names = F)

      shiny::modalDialog(
        title = 'Data Pre-loading',
        easyClose = T,
        size = 'l',
        fluidRow(
          column(
            width = 3,
            selectInput(
              ns('subject_id'),
              'Subject ID',
              choices = subject_ids,
              selected = last_entry('current_subject_id', subject_ids[[1]], check = subject_ids)
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
      error_msg = local_data$error
      if(is.logical(error_msg) && error_msg == FALSE && is.list(local_data$rave_dir)){
        module_dir = local_data$rave_dir$module_data_dir
        l = c('power', 'phase', 'voltage')
        if(length(module_dir) && dir.exists(module_dir)){
          l = c(l, list.dirs(module_dir, full.names = F, recursive = F))
        }
        last_data_types = last_entry('current_data_types')
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
      electrodes = rave:::parse_selections(input$electrode_text)
      data_types = input$data_types
      subject_id = input$subject_id
      epoch = input$epoch
      epoch_range = c(input$pre, input$post)
      error_msg = local_data$error
      if(!is.logical(error_msg) || error_msg != FALSE){
        showNotification('This is an invalid subject. Please check data integrity.', type = 'error')
        return(NULL)
      }

      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error')
        return(NULL)
      }
      rave_prepare(
        subject = subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        attach = F,
        data_types = data_types
      )
      # register

      rave_hist$save(
        main_app = list(
          current_subject_id = subject_id,
          current_electrodes = electrodes,
          current_epoch = epoch,
          current_epoch_range = epoch_range,
          last_data_types = data_types
        )
      )
      global_reactives$has_data = TRUE
      global_reactives$force_refresh_all = Sys.time()
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
      global_reactives$has_data = TRUE
      global_reactives$force_refresh_all = Sys.time()
    }


  }



  return(list(
    header = header,
    control = control,
    server = server
  ))
}
