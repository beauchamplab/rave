# data selector
shiny_data_selector <- function(moduleId){
  subject_choices = list.dirs(rave_options('data_dir'), full.names = F, recursive = F)
  lapply(subject_choices, function(subject_id){
    tryCatch({
      subject = Subject$new(subject_id = subject_id)
      return(TRUE)
    }, error = function(e){
      logger('Subject [', subject_id, '] invalid. Reason:', level = 'WARNING')
      logger(e, level = 'WARNING')
      return(FALSE)
    })
  }) %>%
    unlist ->
    subject_exists
  subject_choices = subject_choices[subject_exists]
  last_entry = rave_hist$get_or_save(key = 'main_app', val = list())
  ns = shiny::NS(moduleId)
  get_epochs = function(subject_id){
    dirs = get_dir(subject_id = subject_id)
    epochs = list.files(dirs$meta_dir, pattern = 'epoch_[a-zA-Z0-9]+.csv')
    if(length(epochs)){
      epochs = str_match(epochs, 'epoch_([a-zA-Z0-9]+).csv')[,2]
      return(epochs)
    }else{
      return('')
    }
  }

  header = function(){
    tags$li(
      actionLink(ns('data_select'), 'Select Data', icon = shiny::icon('tasks'),
                 role = 'button', class = 'nav-item nav-link')
    )
  }
  control = function(){
    tags$li(
      actionLink(ns('auto_calculate'), 'Auto-Calculation ON',
                 icon = shiny::icon('calculator'), role = 'button'),
      class = 'control-sidebar-items'
    )
  }
  server = function(input, output, session, global_reactives){
    local_data = reactiveValues(
      current_subject_id = last_entry[['current_subject_id']],
      current_electrodes = last_entry[['current_electrodes']],
      current_epoch = last_entry[['current_epoch']],
      current_epoch_range = last_entry[['current_epoch_range']],
      mask_path = NULL
    )

    data_select_modal = function(){
      s_choices = subject_choices
      s_selected = local_data$current_subject_id
      e_value = rave:::deparse_selections(local_data$current_electrodes)
      e_selected = local_data$current_epoch
      e_range = local_data$current_epoch_range
      epochs = ''
      if(!is.null(s_selected) && s_selected %in% subject_choices){
        epochs = get_epochs(s_selected)
      }
      if(length(e_range) != 2){
        e_range = c(1, 2)
      }
      shiny::modalDialog(
        title = 'Data Selection',
        fluidRow(
          column(
            width = 4,
            selectInput(ns('subject_id'), 'Subject ID', choices = s_choices, selected = s_selected),
            hr(),
            textInput(ns('electrode_text'), 'Electrodes', value = e_value, placeholder = 'E.g. 1-5,8,11-20'),
            fileInput(ns('electrode_mask'), 'Electrode Mask', multiple = F),
            hr(),
            selectInput(ns('epoch'), 'Epoch Selection', choices = epochs, selected = e_selected),
            fluidRow(
              column(6, numericInput(ns('pre'), 'Before', value = e_range[1], min = 0, step = 0.01)),
              column(6, numericInput(ns('post'), 'After', value = e_range[2], min = 0, step = 0.01))
            ),
            checkboxGroupInput(ns('data_types'), 'Data Types',
                               selected = 'power',
                               choiceNames = c('Power Data', 'Phase Data'),
                               choiceValues = c('power', 'phase'))
          ),
          shinydashboard::tabBox(
            width = 8,
            tabPanel(
              title = 'Selected Electrodes'
            )
          )
        ),
        size = 'l',
        easyClose = T,
        footer = tagList(
          shiny::modalButton('Cancel'),
          actionButton(ns('data_import'), label = 'Import')
        )
      )
    }
    get_selected_electrodes = function(){
      s_choices = subject_choices
      subject_id = input$subject_id
      if(!is.null(subject_id) && subject_id %in% s_choices){
        electrodes = rave:::parse_selections(input$electrode_text)
        mask = input$electrode_mask
        current_subject_id = local_data$current_subject_id
        if(!is.null(mask)){
          if(is.null(current_subject_id) || current_subject_id!=subject_id ||
             is.null(local_data$mask_path) ||
             local_data$mask_path != mask$datapath){
            # use mask
            mask = read.table(mask$datapath, header = F, quote = '\t')
            electrodes = which(mask[[1]] > 0)
          }
        }
        # electrodes
        subject = Subject$new(subject_id = subject_id)
        electrodes = subject$filter_valid_electrodes(electrodes)
        return(electrodes)
      }
      return(NULL)
    }
    observeEvent(input$data_select, {
      shiny::showModal(data_select_modal())
    })
    observeEvent(input$subject_id, {
      # update electrodes
      if(!is.null(local_data$current_subject_id) &&
         local_data$current_subject_id == input$subject_id){
        value = rave:::deparse_selections(local_data$current_electrodes)
      }else if(!input$subject_id %in% subject_choices){
        return(NULL)
      }else{
        e = rave:::load_meta(meta_type = 'electrodes', subject_id = input$subject_id)
        value = rave:::deparse_selections(e$Channel)
      }
      updateTextInput(session, 'electrode_text', value = value)
      # epoch types
      epochs = get_epochs(subject_id = input$subject_id)
      updateSelectInput(session, 'epoch', choices = epochs)
    })
    observe({
      electrodes = get_selected_electrodes()
    })
    observeEvent(input$data_import, {
      electrodes = get_selected_electrodes()
      data_types = input$data_types
      if(length(data_types) == 0){
        showNotification('You must choose at lease one data type', type = 'error')
        return(NULL)
      }
      rave_prepare(
        subject = input$subject_id,
        electrodes = electrodes,
        epoch = input$epoch,
        time_range = c(input$pre, input$post),
        attach = F,
        data_types = data_types
      )
      # register
      local_data$current_subject_id = input$subject_id
      local_data$current_electrodes = electrodes
      local_data$current_epoch = input$epoch
      local_data$current_epoch_range = c(input$pre, input$post)
      if(!is.null(input$mask)){
        local_data$mask_path = mask$datapath
      }
      rave_hist$save(
        main_app = list(
          current_subject_id = input$subject_id,
          current_electrodes = electrodes,
          current_epoch = input$epoch,
          current_epoch_range = c(input$pre, input$post)
        )
      )
      global_reactives$has_data = TRUE
      global_reactives$force_refresh_all = Sys.time()
      removeModal()
    })

    # onload, check if data has been loaded into datarepo
    data_env = getDefaultDataRepository()
    data_loaded = rlang::env_has(data_env, nms = c(".repository", "electrodes", "frequencies", "meta", "phase", "power", "subject", "time_points", "trials", "valid_electrodes"),
                                 inherit = F)
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
