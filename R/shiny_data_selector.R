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

    output$modal_frequencies <- renderUI({
      validate(need(!local_data$error, message = ''))
      subject_code = input$subject_code
      project_name = input$project_name
      freqs = load_meta('frequencies', project_name = project_name, subject_code = subject_code)
      if(!is.null(freqs)){
        freqs = freqs$Frequency
        tagList(
          hr(),
          sliderInput(ns('frequency_range'), 'Frequency Range', min = min(freqs), max = max(freqs),
                      value = range(freqs), round = T, post = ' Hz', step = 1L, ticks = F)
        )
      }
    })

    output$plot3dui <- renderUI({
      if(!local_data$error){
        threejsr::threejsOutput(ns('plot3d'), height = '500px')
      }
    })
    output$plot3d <- threejsr::renderThreejs({
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
        wave_info = subject_info$adapter$get_from_subject('wavelet_log', list(), customized = T)
        if(length(wave_info)){
          wave_info = wave_info[[length(wave_info)]]
        }
        local_data$wavelet_info = wave_info
        local_data$volt_srate = subject_info$adapter$get_sample_rate()

        dirs = subject_info$adapter$get_from_subject('dirs')
        # local_data$data_types = c('power', 'phase', 'voltage')
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
        easyClose = F,
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
            uiOutput(ns('modal_epochs')),
            uiOutput(ns('modal_frequencies'))
          ),
          column(
            width = 9,
            uiOutput(ns('modal_error')),
            column(3, uiOutput(ns('modal_data'))),
            column(9, uiOutput(ns('modal_summary'))),
            column(12, uiOutput(ns('plot3dui')))
          )
        ),
        footer = tagList(
          shiny::modalButton('Cancel'),
          actionButtonStyled(ns('data_import'), label = 'Import', type = 'primary')
        )
      )
    }

    output$modal_data <- renderUI({
      if(!local_data$error){
        l = c('power', 'phase', 'voltage')
        last_data_types = last_entry('data_types', NULL, group = group)
        last_data_types = last_data_types[last_data_types %in% l]

        tagList(
          checkboxGroupInput(ns('data_types'), "Preloaded Data Types:", selected = NULL, choiceNames = c('Power', 'Phase', 'Voltage'), choiceValues = l)
        )
      }else{
        NULL
      }
    })

    output$modal_summary <- renderUI({
      if(!local_data$error){
        data_types = input$data_types
        electrode_text = input$electrode_text
        electrodes = rave:::parse_selections(electrode_text)
        epoch_range = c(input$pre, input$post)
        epoch = input$epoch
        reference = input$reference
        subject_code = input$subject_code
        project_name = input$project_name
        frequency_range = input$frequency_range
        wave_info = local_data$wavelet_info
        electrodes = electrodes[electrodes %in% wave_info$channels]
        ref_table = load_meta(meta_type = 'references', project_name = project_name, subject_code = subject_code, meta_name = reference)
        if(is.data.frame(ref_table)){
          electrodes = electrodes[electrodes %in% ref_table$Electrode[ref_table$Reference != '']]
        }

        wave_srate = wave_info$target_srate
        volt_srate = local_data$volt_srate
        electrode_text = deparse_selections(electrodes)
        freqs = wave_info$frequencies
        freqs = freqs[freqs %within% frequency_range]
        if(!length(freqs)){
          freqs = NA
        }

        trial = load_meta(meta_type = 'epoch', project_name = project_name, subject_code = subject_code, meta_name = epoch)
        if(is.data.frame(trial)){
          n_trials = nrow(trial)
        }else{
          n_trials = NA
        }

        base_size = 8.25 * n_trials * sum(epoch_range) * length(electrodes)
        power_usage = base_size * wave_srate * length(freqs)
        phase_usage = base_size * wave_srate * length(freqs)
        volt_usage = base_size * volt_srate

        if(length(data_types)){
          total_usage = power_usage * ('power' %in% data_types) +
            phase_usage * ('phase' %in% data_types) +
            volt_usage * ('voltage' %in% data_types)
          data_types_c = paste(data_types, collapse = ', ')
        }else{
          total_usage = 0
          data_types_c = ''
        }
        power_usage = to_ram_size(power_usage)
        phase_usage = to_ram_size(phase_usage)
        volt_usage = to_ram_size(volt_usage)

        max_mem_opt = rave_options('max_mem')
        max_mem_opt %?<-% Inf
        max_mem = max_mem_opt * 1000^3
        warn_msg = ''
        if(max_mem < total_usage){
          warn_msg = span(sprintf('WARNING: Data is too large to load! ("max_mem"=%.0fGB)', max_mem_opt), style = 'color:crimson')
        }

        total_usage = to_ram_size(total_usage)




        fluidRow(

          column(
            width = 5,
            strong(sprintf('%s/%s', project_name, subject_code)), br(),
            HTML(
              sprintf('Electrode: %s <br />Reference: %s <br />Frequency: %.0f~%.0f Hz <br />Epoch: %s (%.1f~%.1f s) <br />Number of trials: %d <br/>', electrode_text, reference, min(freqs), max(freqs), epoch, -epoch_range[1], epoch_range[2], n_trials)
            )
          ),
          column(
            width = 7,
            strong('Estimated Memory Needed:'), br(),
            HTML(
              sprintf(
                'Power: %.1f %s<br>Phase: %.1f %s<br>Voltage: %.1f %s',
                power_usage * ('power' %in% data_types),
                attr(power_usage, 'unit'),
                phase_usage * ('phase' %in% data_types),
                attr(phase_usage, 'unit'),
                volt_usage * ('voltage' %in% data_types),
                attr(volt_usage, 'unit')
              )
            ), br(),
            'Preload data: ', data_types_c, br(),
            sprintf('Estimated minimum RAM: %.1f~%.1f %s', total_usage, total_usage*2, attr(total_usage, 'unit')),
            br(),
            warn_msg
          )
        )
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
      frequency_range = input$frequency_range
      epoch = input$epoch
      epoch_range = c(input$pre, input$post)
      reference = input$reference
      subject_id = project_name %&% '/' %&% subject_code

      if('voltage' %in% data_types){
        data_types[data_types == 'voltage'] = 'volt'
      }
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
        frequency_range = frequency_range,
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
