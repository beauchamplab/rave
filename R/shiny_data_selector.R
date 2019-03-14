#' check subject validity tools
#' @param project_name project_name
#' @param subject_code subject_code
#' @param quiet logical
check_subjects2 <- function(
  project_name, subject_code, quiet = FALSE
){
  if(quiet){
    # do not print
    logger = function(...){}
  }

  logger('Checking: Project - ', project_name, ', Subject - ', subject_code)
  dirs = get_dir(subject_code = subject_code, project_name = project_name)
  re = list()
  # 1. Check folders

  # subject folder - 'project/subject'
  re[['subject_dir']] = dir.exists(dirs$subject_dir)

  # RAVE dir - 'project/subject/rave'
  re[['rave_dir']] = dir.exists(dirs$rave_dir)

  # Preprocessing dir - project/subject/rave/preprocess
  re[['preprocess_dir']] = dir.exists(dirs$preprocess_dir)

  # meta dir - project/subject/rave/meta
  re[['meta_dir']] = dir.exists(dirs$meta_dir)

  # chennel_dir - project/subject/rave/data
  re[['channel_dir']] = dir.exists(dirs$channel_dir)

  # power_dir - project/subject/rave/data/power
  re[['power_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'power'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'power', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'power', 'ref'))
    )
  )

  # phase_dir - project/subject/rave/data/phase
  re[['phase_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'phase'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'phase', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'phase', 'ref'))
    )
  )

  # volt_dir - project/subject/rave/data/voltage
  re[['volt_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'voltage'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'voltage', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'voltage', 'ref'))
    )
  )


  ## Preprocess information
  log_data = list()

  if(re[['rave_dir']]){
    # check if preprocess log is present
    if(re[['preprocess_dir']]){
      pre_yaml_file = file.path(dirs$preprocess_dir, 'rave.yaml')
      if(file.exists(pre_yaml_file)){
        pre_hist = yaml::read_yaml(pre_yaml_file)
        log_data[['preprocess']] = pre_hist
      }
    }

    # update log.yaml
    save_log = T
    yaml_file = file.path(dirs$rave_dir, 'log.yaml')
    if(file.exists(yaml_file)){
      log_data_old = yaml::read_yaml(yaml_file)
      if(!is.null(log_data[['preprocess']])){
        # compare
        if(identical(log_data_old[['preprocess']], log_data[['preprocess']], num.eq = T, ignore.environment = T, ignore.bytecode = T)){
          logger('Cached log.yaml shares the same information with preprocess log file. No need to re-cache')
          save_log = F
        }else{
          log_data_old[['preprocess']] = log_data[['preprocess']]
        }
        log_data = log_data_old
      }
    }

    # save to log.yaml
    if(save_log){
      logger('Creating/replacing log.yaml...')
      yaml::write_yaml(log_data, yaml_file, fileEncoding = 'utf-8')
    }

  }

  # if log_data is not null, then we have preprocess information
  if(is.null(log_data) || is.null(log_data[['preprocess']])){
    checklevel = 0
  }else{
    checklevel = log_data$preprocess$checklevel
    if(length(checklevel)!=1 || !is.numeric(checklevel)){
      checklevel = 0
    }
  }
  val = c(rep(T, checklevel), rep(F, 10))[1:4]
  key = c('started_preprocess', 'notch_filter', 'wavelet', 'reference')
  re[key] = as.list(val)


  # Check meta files
  re[['meta_electrode']] = file.exists(file.path(dirs$meta_dir, 'electrodes.csv'))
  re[['meta_time']] = file.exists(file.path(dirs$meta_dir, 'time_points.csv'))
  re[['meta_frequency']] = file.exists(file.path(dirs$meta_dir, 'frequencies.csv'))

  # Find epoch and referenced
  re[['meta_epoch']] = length(list.files(dirs$meta_dir, pattern = '^epoch_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')) > 0
  re[['meta_reference']] = length(list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')) > 0

  # check validity
  if((re$reference && !re$meta_reference) || (re$wavelet && !re$reference)){
    # create a default reference table with noref for all electrodes
    tbl = data.frame(
      Electrode = log_data$preprocess$channels,
      Group	= 'noref',
      Reference	= 'noref',
      Type = 'No Reference',
      stringsAsFactors = F
    )
    ref_file = file.path(dirs$meta_dir, 'reference_default.csv')
    if(!file.exists(ref_file)){
      write.csv(tbl, ref_file, row.names = FALSE)
      re$meta_reference = TRUE
      re$reference = TRUE
    }
  }

  # get references
  references = list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')

  if(re$reference || length(references)){
    references = list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')
    references = stringr::str_match(references, '^reference_([a-zA-Z0-9_]*)\\.[cC][sS][vV]$')[,2]
    re$reference = TRUE
  }else{
    references = ''
  }

  # Get epoch names
  if(re$meta_epoch){
    epochs = list.files(dirs$meta_dir, pattern = '^epoch_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')
    epochs = stringr::str_match(epochs, '^epoch_([a-zA-Z0-9_]*)\\.[cC][sS][vV]$')[,2]
  }else{
    epochs = ''
  }



  list(check = re, log = log_data, epochs = epochs, references = references)
}



#' check subject validity tools (use check_subjects2)
#' @param project_name project_name
#' @param subject_code subject_code
#' @param check check is internally used
#' @param folders folders to check
#' @param preprocess preprocess to check
#' @param Meta Meta to check
check_subjects <- function(
  project_name, subject_code, check = T,
  folders = c('Subject Folder', 'RAVE Folder', 'Preprocessing Folder', 'Meta Folder', 'Channel Folder'),
  preprocess = c('Started Preprocess', 'Notch Filter', 'Wavelet'),
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

      # Need to check if preprocess folder exists
      # dirs = get_dir(subject_code = subject_code, project_name = project_name)
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


#' Shiny modal to be used by init_app for data selection
#' @param module_id any character containing 0-9 a-z A-Z
shiny_data_selector <- function(module_id){

  fband = list(
    'Delta' = c(0.5, 3),
    'Theta' = c(4, 7),
    'Alpha' = c(8, 13),
    'Mu' = c(7.5, 12.5),
    'Beta' = c(16, 31),
    'Low Gamma' = c(32, 74),
    'High Gamma' = c(75, 150)
  )

  ns = shiny::NS(module_id)

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
    # Vars
    group = 'main_app2'
    local_data = reactiveValues(
      has_subject = FALSE,
      check_result = list(),
      load_mesh = TRUE,
      # Prevent mis-clicking
      # If "import" button is clicked multiple times, data will be reloaded multiple times.
      # prevent will be set to false only when modal expanded
      prevent_dblclick = TRUE
    )

    ##### Show modal when 'data_select' is clicked
    observeEvent(input$data_select, {
      shinyjs::addClass(selector = 'body', class = "rave-noscroll")
      shiny::showModal(
        shiny::modalDialog(
          title = 'Data Selection', size = 'l', easyClose = F, fade = F,
          footer = tagList(
            actionButton(ns('dismiss'), 'Cancel'),
            actionButtonStyled(ns('import'), 'Import', type = 'primary', icon = shiny::icon('angle-right'))
          ),
          # Style to make this modal bigger
          tags$style('.modal-lg { min-width: 80vw; }'),
          data_modal()
        )
      )
    })

    observeEvent(input$dismiss, {
      shinyjs::removeClass(selector = 'body', class = "rave-noscroll");
      removeModal()
    })

    ##### Modal layout
    data_modal = function(){
      # Gather data
      projects = get_projects()
      local_data$modal_refresh = Sys.time()
      local_data$prevent_dblclick = FALSE
      last_project = last_entry('project_name', default = NULL, group = group)
      if(length(projects) == 0){
        return(p(strong('No valid project detected. Make sure there is at least one project folder in your data directory!')))
      }
      if(length(last_project) != 1 || !last_project %in% projects){
        last_project = projects[1]
      }

      # Return UI
      fluidRow(
        # Responsive columns

        # Input - data selection panel
        div(
          class = 'col-sm-12 col-md-5',

          # Step 1: Select project and subject
          div(
            class = 'rave-grid-inputs margin-top-20 no-border-inputs',
            div(
              class = 'rave-grid-inputs-legend',
              'Project/Subject'
            ),
            div(
              style="flex-basis: 50%;",
              selectInput(ns('project_name'), 'Project', choices = projects, selected = last_project)
            ),
            div(
              style="flex-basis: 50%;",
              uiOutput(ns('ui_subject'))
            )
          ),

          # Step 2: select subset of data
          uiOutput(ns('ui_step2'))

        ),

        # Show Results
        div(
          class = 'col-sm-12 col-md-7',
          uiOutput(ns('ui_preview'))
        )
      )
    }


    ##### Reactives: dynamic inputs #####
    output$ui_subject = renderUI({
      projects = get_projects()
      project = input$project_name
      local_data$modal_refresh
      local_data$modal_refresh1 = Sys.time()

      if(length(project) == 1 && project %in% projects){

        # Get subject
        subjects = get_subjects(project)

        if(length(subjects) == 0){
          return(p(strong('No valid subject in this project')))
        }

        last_subject = last_entry('subject_code', default = NULL, group = group)
        if(length(last_subject) != 1 || !last_subject %in% subjects){
          last_subject = subjects[1]
        }
        return(
          selectInput(ns('subject_code'), 'Subject', choices = subjects, selected = last_subject)
        )
      }else{
        return(NULL)
      }
    })

    observe({
      # Get project and subject first
      project = input$project_name
      subject = input$subject_code

      if(is.null(project) || is.blank(project) || is.null(subject) || is.blank(subject)){
        local_data$has_subject = FALSE
        local_data$check_result = NULL
        return()
      }

      # Check validity of subject
      tryCatch({
        check_result = check_subjects2(project_name = project, subject_code = subject)
        # We need channel_dir, meta_dir, notch_filter, wavelet, meta_electrode, meta_epoch, meta_frequency
        # if reference == T, we need meta_reference
        must_true = c('channel_dir', 'meta_dir', 'notch_filter', 'wavelet',
                      'meta_electrode', 'meta_epoch', 'meta_frequency')
        if(all(unlist(check_result$check[must_true]))){
          local_data$has_subject = TRUE
          local_data$check_result = check_result
        }else{
          local_data$has_subject = FALSE
          local_data$check_result = check_result
        }
      }, error = function(e){
        local_data$has_subject = FALSE
        local_data$check_result = NULL
      })

    })

    # step 2: subset data
    output$ui_step2 = renderUI({

      local_data$modal_refresh1

      validate(need(local_data$has_subject && is.list(local_data$check_result), message = ''))
      check_result = local_data$check_result


      # Epoch files
      last_epoch = last_entry('epoch_name', NULL, group = group)
      if(length(last_epoch) !=1 || !last_epoch %in% check_result$epochs){
        last_epoch = NULL
      }

      # reference files
      last_reference = last_entry('reference_name', NULL, group = group)
      if(length(last_reference) !=1 || !last_reference %in% check_result$references){
        last_entry('reference_name', 'default', group = group, save = T)
        last_reference = 'default'
      }

      # epoch_range
      last_time_range = last_entry('time_range', NULL, group = group)
      if(length(last_time_range)!=2){
        last_time_range = c(1,2)
      }
      last_time_range[1] = max(last_time_range[1], 0)
      last_time_range[2] = max(last_time_range[2], 0)

      # Electrodes
      last_electrodes = last_entry('electrodes', '', group = group)
      if(!isTRUE(check_result$log$preprocess$subject_code == last_entry('subject_code', '', group = group))){
        last_electrodes = deparse_selections(check_result$log$preprocess$channels)
      }

      # Frequencies
      freqs = tail(check_result$log$preprocess$wavelet_log, 1)[[1]][['frequencies']]

      # Return
      tagList(
        div(
          ##### Epoch UI #####
          id = ns('epoch-chunk'),
          class = 'rave-grid-inputs margin-top-20 no-border-inputs',
          tooltip = ' ',
          flow = 'right',
          `tooltip-image` = TRUE,
          div(
            class = 'tooltip-content',
            style = 'background-image:url("Dipterix-0.0.1/images/data_selectior_epoch.gif")',
            p(
              "`Epoch table` defines experiment onset and trial condition types. Usually these files are stored in [subject]/rave/meta/epoch_[names].csv. `Pre` means seconds before onset. `Post` is the seconds after onset."
            )
          ),
          div(
            class = 'rave-grid-inputs-legend',
            'Epoch Selection'
          ),
          div(
            style="flex-basis: 50%;",
            selectInput(ns('epoch'), 'Epoch Table', choices = check_result$epochs, selected = last_epoch)
          ),
          div(
            style="flex-basis: 25%;",
            numericInput(ns('epoch_pre'), 'Pre', min = 0, value = last_time_range[1])
          ),
          div(
            style="flex-basis: 25%;",
            numericInput(ns('epoch_post'), 'Post', min = 0, value = last_time_range[2])
          ),
          div(
            style="flex-basis: 100%;",
            tags$small(textOutput(ns('epoch_txt')), style = 'color:#a1a1a1;')
          )
        ),
        div(
          ####### Frequency UI ######
          id = ns('frequency-chunk'),
          class = 'rave-grid-inputs margin-top-20 no-border-inputs',
          tooltip = ' ',
          flow = 'right',
          div(
            class = 'tooltip-content',
            style = 'width: 17em; max-width: 300px; text-align: left; padding: 0 1em;',
            p(h4('Presets:'), br(),
              HTML(paste(sapply(seq_along(fband), function(ii){
              nm = names(fband)[[ii]]
              b = fband[[ii]]
              sprintf('%s wave: %.1f - %.1f', nm, b[1], b[2])
            }), collapse = '<br />')))
          ),
          div(
            class = 'rave-grid-inputs-legend',
            'Frequency'
          ),
          div(
            style="flex-basis: 60%;",
            sliderInput(ns('frequencies'), 'Frequency Range', min = max(min(freqs)-1, 0), ticks = F,
                        max = max(freqs)+1, value = range(freqs), step = 0.1, round = TRUE, post = 'Hz')
          ),
          div(
            style="flex-basis: 40%;",
            tags$label('Presets'),
            p(
              tags$label(
                class = 'label label-default bg-red', style = 'display: inline-block;',
                tags$a('Delta', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_1'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-orange', style = 'display: inline-block;',
                tags$a('Theta', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_2'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-yellow', style = 'display: inline-block;',
                tags$a('Alpha', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_3'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-green', style = 'display: inline-block;',
                tags$a('Beta', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_5'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-blue', style = 'display: inline-block;',
                tags$a('Low Gamma', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_6'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-purple', style = 'display: inline-block;',
                tags$a('High Gamma', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_7'), href = '#')
              )
            )
          ),
          div(
            style="flex-basis: 100%;",
            tags$small(textOutput(ns('frequency_txt')), style = 'color:#a1a1a1;')
          )
        ),
        div(
          ##### Electrodes UI ####
          id = ns('electrode-chunk'),
          class = 'rave-grid-inputs margin-top-20 no-border-inputs',
          tooltip = '',
          flow = 'right',
          `tooltip-image` = TRUE,
          div(
            class = 'rave-grid-inputs-legend',
            'Electrode & Reference'
          ),
          div(
            style="flex-basis: 100%;",
            selectInput(ns('reference'), 'Reference Table', choices = check_result$references, selected = last_reference)
          ),
          div(
            style="flex-basis: 50%;",
            textInput(ns('electrodes'), 'Electrodes', placeholder = 'Example: 1-4,6', value = last_electrodes)
          ),
          div(
            style="flex-basis: 50%;",
            fileInput(ns('mask'), 'or Mask File')
          ),
          div(
            style="flex-basis: 100%;",
            tags$small(textOutput(ns('electrode_txt')), style = 'color:#a1a1a1;')
          ),
          div(
            style="flex-basis: 100%;",
            uiOutput(ns('ui_mask'))
          )

        )
      )
    })

    lapply(1:7, function(ii){
      observeEvent(input[['freq_preset_' %&% ii]], {
        updateSliderInput(session = session, inputId = 'frequencies', value = fband[[ii]])
      })
    })

    observe({
      local_data$mask = mask_file = input$mask
      check_result = isolate(local_data$check_result)
      tryCatch({
        mask_tbl = read.csv(mask_file$datapath, header = T)
        if(!is.integer(mask_tbl[,1])){
          # the first column is not electrode, add the column "Electrode" to table
          mask_tbl$Electrode = seq_len(nrow(mask_tbl))
          nms = names(mask_tbl); nms = nms[nms != 'Electrode']
          mask_tbl = mask_tbl[,c('Electrode', nms)]
        }

        mask_tbl = mask_tbl[mask_tbl$Electrode %in% check_result$log$preprocess$channels, ]

        if(nrow(mask_tbl) == 0 || ncol(mask_tbl) <= 1){
          local_data$mask_table = NULL
        }else{
          local_data$mask_table = mask_tbl
        }
      }, error = function(e){
        if(length(mask_file$datapath) == 1){
          showNotification(p('Cannot parse mask file'), type = 'error')
        }
        local_data$mask_table = NULL
      })
    })

    output$frequency_txt <- renderText({
      validate(need(local_data$has_subject && is.list(local_data$check_result), message = ''))
      check_result = local_data$check_result
      w = check_result$log$preprocess$wavelet_log
      freqs = w[[length(w)]]$frequencies
      f = input$frequencies

      n_tf = length(freqs)
      freqs = freqs[freqs %within% f]

      n_f = length(freqs)
      if(n_f){
        sprintf('%d out of %d frequencies selected, actual range: %.0fHz - %.0fHz', n_f, n_tf, min(freqs), max(freqs))
      }else{
        sprintf('Warning: No frequency selected, total number of frequencies: %d', n_tf)
      }
    })

    output$electrode_txt <- renderText({
      validate(need(local_data$has_subject && is.list(local_data$check_result), message = ''))
      txt = input$electrodes
      check_result = local_data$check_result
      if(is.null(check_result)){
        return('')
      }

      # try to load reference table

      es = parse_selections(txt)
      sel = es %in% check_result$log$preprocess$channels

      ref = input$reference
      if(length(ref) == 1 && ref %in% check_result$references){
        # load reference
        project = isolate(input$project_name)
        subject = isolate(input$subject_code)

        ref_tbl = load_meta('references', project_name = project, subject_code = subject, meta_name = ref)
        sel = sel & (es %in% ref_tbl$Electrode[ref_tbl$Reference != ''])
      }

      es = es[sel]
      ne = length(es)
      txt = deparse_selections(es)
      if(ne){
        sprintf('%d electrodes selected (%s)', ne, txt)
      }else{
        'No electrode selected'
      }
    })

    output$ui_mask <- renderUI({
      validate(need(!is.null(local_data$mask_table), message = ''))
      mask_table = local_data$mask_table
      vars = c('[NONE..]', names(mask_table))
      # Only three selection is allowed
      div(
        actionLink(ns('toggle_filter'), 'Show/Hide Filters', onclick=sprintf('$("#%s").toggle();', ns('mask_filters'))),
        div(
          id = ns('mask_filters'), style = 'display:none;',
          tagList(lapply(seq_len(3), function(ii){
            div(
              class = 'rave-grid-inputs no-border',
              div(
                style="flex-basis: 25%;",
                local({
                  if(ii > 1){
                    selectInput(ns('filter_lgt_' %&% ii), 'Filter ' %&% ii, choices = c('AND', 'OR'), selected = 'OR')
                  }else{
                    tags$label('Filter 1')
                  }
                })
              ),
              div(
                style="flex-basis: 25%;",
                selectInput(ns('filter_col_' %&% ii), 'Key', choices = vars, selected = vars[1])
              ),
              div(
                style="flex-basis: 25%;",
                selectInput(ns('filter_op_' %&% ii), 'Operator',
                            choices = c('=', '>', '<', '>=', '<=', 'IN', 'NOT IN', 'BETWEEN'),
                            selected = 'NOT IN')
              ),
              div(
                style="flex-basis: 25%;",
                textInput(ns('filter_val_' %&% ii), 'Value', value = '')
              )
            )
          }))
        )
      )
    })

    observe({
      # filters
      mask_table = local_data$mask_table
      if(is.null(mask_table)){
        return()
      }
      res = as.integer(mask_table[,1])
      txt = deparse_selections(res)
      try({
        sel = rep(TRUE, length(res))
        for(ii in 1:3){
          lgt = input[['filter_lgt_' %&% ii]]
          var = input[['filter_col_' %&% ii]]
          op = input[['filter_op_' %&% ii]]
          val = input[['filter_val_' %&% ii]]

          if(var %in% names(mask_table)){
            x = mask_table[[var]]
            if(is.numeric(x)){
              val = suppressWarnings({
                r = unlist(stringr::str_split(val, '[^0-9\\.]+'))
                r = r[!is.blank(r)]
                r = as.numeric(r)
                r
              })
              if(any(is.na(val))){ next }
            }else{
              val = unlist(stringr::str_split(val, '[,\\ ]+'))
            }
            subsel = switch (op,
                             '=' = { x == val[1] },
                             '>' = { x > val[1] },
                             '>=' = { x >= val[1] },
                             '<' = { x < val[1] },
                             '<=' = { x <= val[1] },
                             'IN' = { x %in% val },
                             'NOT IN' = { !x %in% val },
                             'BETWEEN' = { x %within% val }
            )

            if(ii == 1){
              sel = subsel
            }else{
              fun = (c(`&`, `|`)[c('AND', 'OR') %in% lgt])
              if(!length(fun)){
                fun = `|`
              }
              sel = fun[[1]](sel, subsel)
            }
          }
        }

        res = res[sel]
        txt = deparse_selections(res)
        last_electrodes = last_entry('electrodes', txt, group = group, save = T)
      }, silent = T)
      updateTextInput(session = session, inputId = 'electrodes', value = txt)
    })


    output$epoch_txt <- renderText({
      project = input$project_name
      subject = input$subject_code
      epoch = input$epoch
      time_range = c(input$epoch_pre, input$epoch_post)

      fn = function(...){return('')}
      tryCatch({
        epoch_table = load_meta(meta_type = 'epoch', meta_name = epoch, project_name = project, subject_code = subject)
        n_trials = nrow(epoch_table)
        n_cond = length(unique(epoch_table$Condition))
        n_t = (time_range[2] + time_range[1])
        s = sprintf('%d trials, %d unique condition types, %.1f seconds',
                    n_trials, n_cond, n_t)
        return(s)
      }, error = fn, warning = fn)
    })


    ##### Dynamic previews #####
    output$ui_preview <- renderUI({
      check_result = local_data$check_result
      if(!local_data$has_subject){
        # Show error message
        if(is.null(check_result)){
          return(p(
            style = 'color:#a1a1a1',
            'No subject or some errors occur during checking subject. Please report bug if you are sure that the subject exists.'
          ))
        }else{
          # Subject exists, but some procedure needs to be done before importing
          must_true = list(
            'channel_dir' = 'No data folder found (preprocess)',
            'meta_dir' = 'No meta directory found',
            'notch_filter' = 'Needs Notch filter (preprocess)',
            'wavelet' = 'Needs wavelet transformation (preprocess)',
            'meta_electrode' = 'Meta data has no electrode information (electrodes.csv)',
            'meta_epoch' = 'Meta data has no epoch information',
            'meta_frequency' = 'Meta data has no frequency information (wavelet?)'
          )
          r = check_result$check[names(must_true)]
          r = must_true[!unlist(r)]
          return(div(p('This subject failed the data check.'), tags$ul(
            tagList(lapply(r, tags$li))
          )))
        }
      }else{
        # Subject should be valid, detect and preview
        # 3. Brain-mesh
        # How many trials
        fluidRow(
          column(
            12,
            div(
              class = 'rave-grid-inputs margin-top-20',
              div(
                class = 'rave-grid-inputs-legend',
                'Load Estimation'
              ),
              div(
                style="flex-basis: 100%;",
                uiOutput(ns('ui_summary'))
              )
            )
          ),
          column(
            12,
            div(
              class = 'rave-grid-inputs margin-top-20',
              style = 'padding: 5px 0px 5px 5px;',
              div(
                class = 'rave-grid-inputs-legend',
                '3D Viewer'
              ),
              div(
                style="display: block;width: 100%;flex-basis: 100%;",
                div(
                  style = 'position: absolute; z-index:100; ',
                  checkboxInput(ns('load_mesh'), 'Load Mesh', value = isolate(local_data$load_mesh))
                ),
                threejsBrainOutput(ns('three_viewer'), height = '500px')
                # threejsOutput(ns('three_viewer'), height = '500px')
              )
            )
          )
        )
      }

    })

    output$ui_summary <- renderUI({
      validate(need(local_data$has_subject, message = ''))
      check_result = local_data$check_result

      project = input$project_name
      subject = input$subject_code

      # Trial
      epoch = input$epoch
      epoch_table = load_meta(meta_type = 'epoch', meta_name = epoch, project_name = project, subject_code = subject)
      n_trials = nrow(epoch_table)

      # Frequency
      w = check_result$log$preprocess$wavelet_log
      w = w[[length(w)]]
      freqs = w$frequencies[w$frequencies %within% input$frequencies]
      n_freqs = length(freqs)

      # Time Points
      time_range = c(input$epoch_pre, input$epoch_post)
      total_time = sum(time_range)
      n_time_wave = w$target_srate * total_time + 1
      n_time_volt = check_result$log$preprocess$srate * total_time + 1

      # Electrodes
      elec = input$electrodes
      elec = parse_selections(elec)
      elec = elec[elec %in% check_result$log$preprocess$channels]
      n_electrodes = length(elec)

      # usage
      s_volt = prod(n_trials, n_time_volt, n_electrodes) * 8.25 * 3
      s_power = prod(n_trials, n_freqs, n_time_wave, n_electrodes) * 8.25 * 3

      # # SUMA brain?
      # suma_dir = get_dir(subject_code = subject, project_name = project)$suma_dir
      # spec_file = file.path(suma_dir, rave_options('suma_spec_file'))
      # brain_size = 0
      # if(file.exists(spec_file)){
      #   spec_parsed = suma_spec_parse(subject = project %&% '/' %&% subject)
      #   sv = unique(unlist(spec_parsed)['SurfaceVolume'])
      #   if(length(sv)){
      #     sv = paste0(sv[1], '.brik')
      #   }
      #   if(!is.null(find_path(sv))){
      #     sv = find_path(sv)
      #   }else{
      #     sv = unlist(stringr::str_split(sv, '/'))
      #     sv = file.path(suma_dir, sv[length(sv)])
      #   }
      #
      #   if(!is.null(sv) && file.exists(sv)){
      #     brain_size = file.info(sv)$size
      #   }
      # }

      drive_speed = rave_options('drive_speed')
      if(length(drive_speed) >= 2){
        drive_speed = as.numeric(drive_speed[2])
      }else{
        drive_speed = NA
      }



      p(
        strong('Voltage: '), sprintf('%d Trials x %d Timepoints x %d Electrodes (%s)',
                                     n_trials, n_time_volt, n_electrodes, to_ram_size(s_volt)), br(),
        strong('Power/Phase: '), sprintf('%d Trials x %d Frequencies x %d Timepoints x %d Electrodes (%s each)',
                                         n_trials, n_freqs, n_time_wave, n_electrodes, to_ram_size(s_power)), br(),
        # strong('Brain: '), ifelse(brain_size > 0,
        #                           sprintf('surface volume, %s', to_ram_size(brain_size)),
        #                           'no surface volume file found'), br(),
        strong('Estimated Resource Required: '), sprintf('%s (memory)',
                                                         to_ram_size(max(s_volt, s_power))), br(),
        strong('Estimated Loading time: '), sprintf(
          'Power (%.0f sec), Phase (%.0f sec), Voltage(%.0f sec)',
          s_power * drive_speed / 1000^2 * 2,
          s_power * drive_speed / 1000^2 * 2,
          s_volt * drive_speed / 1000^2 * 2
        )
      )
    })

    # Local environment to store temporary SUMA brain
    # brain_env = new.env()

    # output$three_viewer <- renderThreejs({
    #   validate(need(local_data$has_subject, message = ''))
    #   project = input$project_name
    #   subject = input$subject_code
    #   subject_id = sprintf('%s/%s', project, subject)
    #   ref = input$reference
    #   elec = input$electrodes
    #   load_mesh = input$load_mesh
    #
    #
    #   brain = brain_env[[subject_id]]
    #   if(is.null(brain)){
    #     s = Subject$new(project_name = project, subject_code = subject, reference = ref, strict = FALSE)
    #     brain = RaveBrain$new(subject = s)
    #     brain_env[[subject_id]] = brain
    #   }
    #
    #   if(load_mesh && brain$mesh_count == 0){
    #     brain$import_spec(nearest_face = F)
    #   }
    #
    #
    #   brain = brain$copy()
    #   elec = parse_selections(elec)
    #   s = brain$.__enclos_env__$private$subject
    #   valid_e = s$filter_valid_electrodes(elec)
    #   invalid_e = s$filter_all_electrodes(elec)
    #   invalid_e = invalid_e[!invalid_e %in% valid_e]
    #
    #   lapply(s$electrodes$Electrode, function(ii){
    #     if(ii %in% valid_e){
    #       brain$set_electrode_value(which = ii, value = -1)
    #     }else if (ii %in% invalid_e){
    #       brain$set_electrode_value(which = ii, value = 1)
    #     }
    #   })
    #
    #   brain$view(control_gui = F, width = '100%', height = '500px', center = T, show_mesh = load_mesh)
    # })

    output$three_viewer <- renderBrain({
      validate(need(local_data$has_subject, message = ''))
      project = input$project_name
      subject = input$subject_code
      subject_id = sprintf('%s/%s', project, subject)
      ref = input$reference
      elec = parse_selections(input$electrodes)
      load_mesh = input$load_mesh

      subject = as_subject(subject_id, reference = ref)

      brain = rave_brain2()
      brain$load_electrodes(subject = subject)

      if(load_mesh){
        brain$load_surfaces(subject = subject)
      }

      valid_e = subject$filter_valid_electrodes(elec)
      invalid_e = subject$filter_all_electrodes(elec)
      invalid_e = invalid_e[!invalid_e %in% valid_e]

      tbl = subject$electrodes


      for(e in valid_e){
        brain$set_electrode_value(subject = subject, electrode = e, value = -1, time = 0,
                                  message = paste('Reference Group:', tbl$Group[tbl$Electrode == e]))
      }
      for(e in invalid_e){
        brain$set_electrode_value(subject = subject, electrode = e, value = 1, time = 0,
                                  message = paste('Reference Group:', tbl$Group[tbl$Electrode == e], '(electrode not used)'))
      }
      brain$view(control_panel = F)
    })


    observeEvent(input$import, {
      check_result = local_data$check_result
      if(local_data$prevent_dblclick){
        return()
      }
      if(!local_data$has_subject || is.null(check_result)){
        showNotification(p('Invalid subject. Please make sure you have run preprocess and generate epoch files.'), type = 'error', id = ns('data_import'))
        return()
      }
      subject_code = input$subject_code
      project_name = input$project_name
      frequencies = input$frequencies
      epoch = input$epoch
      epoch_range = c(input$epoch_pre, input$epoch_post)
      reference = input$reference
      subject_id = project_name %&% '/' %&% subject_code
      electrodes = parse_selections(input$electrodes)

      tmp_subject = Subject$new(project_name = project_name, subject_code = subject_code, reference = reference, strict = FALSE)
      electrodes = tmp_subject$filter_valid_electrodes(electrodes)

      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error', id = ns('data_import'))
        return(NULL)
      }

      freqs = tmp_subject$frequencies$Frequency %within% frequencies
      if(!sum(freqs)){
        showNotification('No frequency found in your selected frequency band', type = 'error', id = ns('data_import'))
        return(NULL)
      }

      if(sum(epoch_range) == 0){
        showNotification('Please select valid time range', type = 'error', id = ns('data_import'))
        return(NULL)
      }

      rave_prepare(
        subject = subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        frequency_range = frequencies,
        reference = reference,
        attach = F,
        data_types = NULL
      )
      # register

      last_entry('project_name', project_name, save = T, group = group)
      last_entry('subject_code', subject_code, save = T, group = group)
      last_entry('electrodes', deparse_selections(electrodes), save = T, group = group)
      last_entry('epoch_name', epoch, save = T, group = group)
      last_entry('time_range', epoch_range, save = T, group = group)
      last_entry('reference_name', reference, save = T, group = group)

      # refresh UIs
      global_reactives$force_refresh_all = Sys.time()
      global_reactives$has_data = Sys.time()

      shinyjs::removeClass(selector = 'body', class = "rave-noscroll");
      removeModal()
      # Remove
      local_data$prevent_dblclick = TRUE
      logger('Subject loaded, trigger module to refresh...')
    })


    ##### End of server #####

    # onload, check if data has been loaded into datarepo
    data_env = getDefaultDataRepository()
    on.exit({rm(data_env)})
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
    if(all(data_loaded)){
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


