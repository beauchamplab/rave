check_subjects2 <- function(
  project_name, subject_code
){

  logger('Checking: Project - ', project_name, ', Subject - ', subject_code)
  dirs = get_dir(subject_code = subject_code, project_name = project_name)
  re = list()
  # 1. Check folders

  # subject folder - 'project/subject'
  re[['subject_dir']] = file.exists(dirs$subject_dir)

  # RAVE dir - 'project/subject/rave'
  re[['rave_dir']] = file.exists(dirs$rave_dir)

  # Preprocessing dir - project/subject/rave/preprocess
  re[['preprocess_dir']] = file.exists(dirs$preprocess_dir)

  # meta dir - project/subject/rave/meta
  re[['meta_dir']] = file.exists(dirs$meta_dir)

  # chennel_dir - project/subject/rave/data
  re[['channel_dir']] = file.exists(dirs$channel_dir)

  ## Preprocess information
  log_data = NULL

  if(re[['rave_dir']]){
    yaml_file = file.path(dirs$rave_dir, 'log.yaml')

    if(file.exists(yaml_file)){
      log_data = yaml::read_yaml(yaml_file)
    }else if(re[['preprocess_dir']]){

      # Check if preprocess dir has it
      pre_yaml_file = file.path(dirs$preprocess_dir, 'rave.yaml')
      if(file.exists(pre_yaml_file)){
        pre_hist = yaml::read_yaml(pre_yaml_file)
        log_data = list(
          preprocess = pre_hist
        )
        # save to log.yaml
        yaml::write_yaml(log_data, yaml_file, fileEncoding = 'utf-8')
      }
    }

  }

  # if log_data is not null, then we have preprocess information
  if(is.null(log_data)){
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
  re[['meta_epoch']] = length(list.files(dirs$meta_dir, pattern = '^epoch_.*\\.[cC][sS][vV]$')) > 0
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
    epochs = list.files(dirs$meta_dir, pattern = '^epoch_.*[cC][sS][vV]$')
    epochs = stringr::str_match(epochs, '^epoch_(.*)\\.[cC][sS][vV]$')[,2]
  }else{
    epochs = ''
  }


  list(check = re, log = log_data, epochs = epochs, references = references)
}






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
    group = 'main_app'
    local_data = reactiveValues(
      has_subject = FALSE,
      check_result = list()
    )

    ##### Show modal when 'data_select' is clicked
    observeEvent(input$data_select, {
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

    observeEvent(input$dismiss, { removeModal() })

    ##### Modal layout
    data_modal = function(){
      # Gather data
      projects = get_projects()
      local_data$modal_refresh = Sys.time()
      last_project = last_entry('project_name', default = NULL, group = group)
      if(length(projects) == 0){
        return(p(strong('No valid project detected. Make sure there is at least one project folder in your data directory!')))
      }
      if(length(projects) != 1 || !last_project %in% projects){
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

      local_data$modal_refresh

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
            style = 'background-image:url("Dipterix-0.0.1/images/data_selectior_epoch.png")',
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
            p(HTML(paste(sapply(seq_along(fband), function(ii){
              nm = names(fband)[[ii]]
              b = fband[[ii]]
              sprintf('%s wave: %.1f - %.1f', nm, b[1], b[2])
            }), collapse = '<br />')))
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
                tags$a('Mu', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_4'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-blue', style = 'display: inline-block;',
                tags$a('Beta', style = 'color:white!important', class = 'action-button',
                       id = ns('freq_preset_5'), href = '#')
              ),
              tags$label(
                class = 'label label-default bg-aqua', style = 'display: inline-block;',
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

      es = parse_selections(txt)
      es = es[es %in% check_result$log$preprocess$channels]
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
      txt = rave::deparse_selections(res)
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
        txt = rave::deparse_selections(res)
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
                h4('Resource Usage'),
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
                threejsr::threejsOutput(ns('three_viewer'), height = '500px')
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

      # SUMA brain?
      suma_dir = get_dir(subject_code = subject, project_name = project)$suma_dir
      spec_file = file.path(suma_dir, rave_options('suma_spec_file'))
      brain_size = 0
      if(file.exists(spec_file)){
        spec_parsed = rave:::suma_spec_parse(subject = project %&% '/' %&% subject)
        sv = unique(unlist(spec_parsed)['SurfaceVolume'])
        if(length(sv)){
          sv = paste0(sv[1], '.brik')
        }
        if(!is.null(find_path(sv))){
          sv = find_path(sv)
        }else{
          sv = unlist(stringr::str_split(sv, '/'))
          sv = file.path(suma_dir, sv[length(sv)])
        }

        if(!is.null(sv) && file.exists(sv)){
          brain_size = file.info(sv)$size
        }
      }

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
        strong('Brain: '), ifelse(brain_size > 0,
                                  sprintf('surface volume, %s', to_ram_size(brain_size)),
                                  'no surface volume file found'), br(),
        strong('Estimated Resource Required: '), sprintf('%s (memory)',
                                                         to_ram_size(brain_size + max(s_volt, s_power))), br(),
        strong('Estimated Loading time: '), sprintf(
          'Power (%.0f sec), Phase (%.0f sec), Voltage(%.0f sec)',
          s_power * drive_speed / 1000^2 * 2,
          s_power * drive_speed / 1000^2 * 2,
          s_volt * drive_speed / 1000^2 * 2
        )
      )
    })

    # Local environment to store temporary SUMA brain
    brain_env = new.env()

    output$three_viewer <- threejsr::renderThreejs({
      validate(need(local_data$has_subject, message = ''))
      project = input$project_name
      subject = input$subject_code
      subject_id = sprintf('%s/%s', project, subject)
      ref = input$reference
      elec = input$electrodes
      load_mesh = input$load_mesh


      brain = brain_env[[subject_id]]
      if(is.null(brain)){
        s = Subject$new(project_name = project, subject_code = subject, reference = ref)
        brain = RaveBrain$new(subject = s)
        brain_env[[subject_id]] = brain
      }

      if(load_mesh && brain$mesh_count == 0){
        brain$import_spec()
      }


      brain = brain$copy()
      elec = parse_selections(elec)
      s = brain$.__enclos_env__$private$subject
      valid_e = s$filter_valid_electrodes(elec)
      invalid_e = s$filter_all_electrodes(elec)
      invalid_e = invalid_e[!invalid_e %in% valid_e]

      lapply(s$electrodes$Electrode, function(ii){
        if(ii %in% valid_e){
          brain$set_electrode_value(which = ii, value = -1)
        }else if (ii %in% invalid_e){
          brain$set_electrode_value(which = ii, value = 1)
        }
      })

      brain$view(control_gui = F, width = '100%', height = '500px', center = T, show_mesh = load_mesh)
    })


    observeEvent(input$import, {
      check_result = local_data$check_result
      subject_code = input$subject_code
      project_name = input$project_name
      frequencies = input$frequencies
      epoch = input$epoch
      epoch_range = c(input$epoch_pre, input$epoch_post)
      reference = input$reference
      subject_id = project_name %&% '/' %&% subject_code
      electrodes = parse_selections(input$electrodes)

      tmp_subject = Subject$new(project_name = project_name, subject_code = subject_code, reference = reference)
      electrodes = tmp_subject$filter_valid_electrodes(electrodes)

      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error')
        return(NULL)
      }

      freqs = tmp_subject$frequencies$Frequency %within% frequencies
      if(!sum(freqs)){
        showNotification('No frequency found in your selected frequency band', type = 'error')
        return(NULL)
      }

      if(sum(epoch_range) == 0){
        showNotification('Please select valid time range', type = 'error')
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
      removeModal()
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




# data selector
shiny_data_selector_old <- function(moduleId){
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
    group = 'main_app2'
    local_data = reactiveValues(
      error = TRUE,
      error_info = list(
        'Wait... ' = 'Initializing'
      ),
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
        logger('Subject was imported last time')
        e = last_entry('electrodes', NULL, group = group)
        if(is.character(e)){
          e = parse_selections(e)
        }
      }else{
        e = local_data$electrodes
      }

      e = rave:::deparse_selections(e)

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
        tagList(
          threejsr::threejsOutput(ns('plot3d'), height = '500px'),
          div( style = 'width:100%;',
            div(style = 'float:right;',
                actionLink(ns('load_mesh'), 'Load brain surface'))
          )
        )

      }
    })



    env = new.env()
    observeEvent(input$load_mesh, {
      env$load_mesh = T
      local_data$refresh_mesh = Sys.time()
    })
    output$plot3d <- threejsr::renderThreejs({
      local_data$refresh_mesh
      if(!local_data$error){
        project_name = input$project_name
        subject_code = input$subject_code
        es = rave:::parse_selections(input$electrode_text)

        env$load_mesh %?<-% FALSE
        if(env$load_mesh){
          env$load_mesh = FALSE
          brain = RaveBrain$new(subject = sprintf('%s/%s', project_name, subject_code))
          tryCatch({
            brain$import_spec()
          }, error = function(e){
            showNotification(p('Cannot load mesh from spec file. Make sure ', rave_options('suma_spec_file'), ' exists and all files are ASCII encoded.'),
                             type = 'error', id = ns('noti'))
          })

        }else{
          brain = RaveBrain$new()
          tbl = load_meta('electrodes', project_name = project_name, subject_code = subject_code)
          brain$load_electrodes(tbl = tbl)
        }


        # Render electrodes if selected
        lapply(es, brain$set_electrode_value, value = -1, keyframe = 0)

        brain$view(control_gui = F)
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
          subjects = sc
          local_data$last_subject = TRUE
        }else if(!sc %in% subjects){
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
        tags$form(
          id = 'data-selector',
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
          )
        ),
        footer = tagList(
          shiny::modalButton('Cancel'),
          actionButtonStyled(ns('data_import'), label = 'Import', type = 'primary', form = 'data-selector', `data-value`="13")
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
      if(length(local_data$error) && !local_data$error){
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
        if(length(electrodes)){
          electrode_text = deparse_selections(electrodes)
        }else{
          electrode_text = NA
        }

        wave_srate = wave_info$target_srate
        volt_srate = local_data$volt_srate
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

        est_time = 0.1 * length(electrodes)
        drive_speed = rave_options('drive_speed')
        if(length(drive_speed) == 2){
          drive_speed = drive_speed[2]
        }
        if(!is.numeric(drive_speed)){
          drive_speed = NA
        }

        if(length(data_types)){
          total_usage = power_usage * ('power' %in% data_types) +
            phase_usage * ('phase' %in% data_types) +
            volt_usage * ('voltage' %in% data_types)
          data_types_c = paste(data_types, collapse = ', ')
          est_time = est_time + drive_speed * total_usage / 1000000
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
            sprintf('Estimated time: %.1f seconds', est_time * 3),
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

      # refresh UIs
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
