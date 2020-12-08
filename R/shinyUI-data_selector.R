
shiny_data_selector <- function(module_id, data_env = getDefaultDataRepository()){
  
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
  
  
  
  server = function(input, output, session, global_reactives, clear_cache = NULL){
    # Vars
    group = 'main_app2'
    local_data = reactiveValues(
      has_subject = FALSE,
      check_result = list(),
      load_mesh = !isFALSE(last_entry('load_mesh', TRUE, save = FALSE, group = group)),
      # Prevent mis-clicking
      # If "import" button is clicked multiple times, data will be reloaded multiple times.
      # prevent will be set to false only when modal expanded
      prevent_dblclick = TRUE
    )
    local_env = dipsaus::fastmap2()
    
    ##### Show modal when 'data_select' is clicked
    open_modal <- function(){
      shinyjs::addClass(selector = 'body', class = "rave-noscroll")
      local_env$is_open = TRUE
      shiny::showModal(
        shiny::modalDialog(
          title = 'Data Selection', size = 'l', easyClose = FALSE, fade = FALSE,
          footer = tagList(
            actionButton(ns('dismiss'), 'Cancel'),
            dipsaus::actionButtonStyled(ns('import'), 'Load Data', type = 'primary', icon = shiny::icon('angle-right'))
          ),
          # Style to make this modal bigger
          tags$style('.modal-lg { min-width: 80vw; }'),
          data_modal()
        )
      )
    }
    
    dismiss_modal <- function(){
      shinyjs::removeClass(selector = 'body', class = "rave-noscroll");
      removeModal()
      local_env$is_open = FALSE
    }
    
    observeEvent(input$data_select, {
      open_modal()
    })
    
    observeEvent(input$dismiss, {
      dismiss_modal()
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
              selectInput(ns('project_name'), 'Project', choices = projects, selected = last_project),
              checkboxInput(ns('group_module'), 'Load for group analysis'),
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
        last_electrodes = dipsaus::deparse_svec(check_result$log$preprocess$channels)
      }
      
      # Frequencies
      freqs = utils::tail(check_result$log$preprocess$wavelet_log, 1)[[1]][['frequencies']]
      
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
            numericInput(ns('epoch_pre'), 'Pre', min = 0, value = last_time_range[1], step = 0.1)
          ),
          div(
            style="flex-basis: 25%;",
            numericInput(ns('epoch_post'), 'Post', min = 0, value = last_time_range[2], step = 0.1)
          ),
          div(
            style="flex-basis: 100%;",
            tags$small(textOutput(ns('epoch_txt')), style = 'color:#a1a1a1;')
          )
        ),
        # div(
        #   ####### Frequency UI ######
        #   id = ns('frequency-chunk'),
        #   class = 'rave-grid-inputs margin-top-20 no-border-inputs',
        #   tooltip = ' ',
        #   flow = 'right',
        #   div(
        #     class = 'tooltip-content',
        #     style = 'width: 17em; max-width: 300px; text-align: left; padding: 0 1em;',
        #     p(h4('Presets:'), br(),
        #       HTML(paste(sapply(seq_along(fband), function(ii){
        #       nm = names(fband)[[ii]]
        #       b = fband[[ii]]
        #       sprintf('%s wave: %.1f - %.1f', nm, b[1], b[2])
        #     }), collapse = '<br />')))
        #   ),
        #   div(
        #     class = 'rave-grid-inputs-legend',
        #     'Frequency'
        #   ),
        #   div(
        #     style="flex-basis: 60%;",
        #     sliderInput(ns('frequencies'), 'Frequency Range', min = max(min(freqs)-1, 0), ticks = F,
        #                 max = max(freqs)+1, value = range(freqs), step = 0.1, round = TRUE, post = 'Hz')
        #   ),
        #   div(
        #     style="flex-basis: 40%;",
        #     tags$label('Presets'),
        #     p(
        #       tags$label(
        #         class = 'label label-default bg-red', style = 'display: inline-block;',
        #         tags$a('Delta', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_1'), href = '#')
        #       ),
        #       tags$label(
        #         class = 'label label-default bg-orange', style = 'display: inline-block;',
        #         tags$a('Theta', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_2'), href = '#')
        #       ),
        #       tags$label(
        #         class = 'label label-default bg-yellow', style = 'display: inline-block;',
        #         tags$a('Alpha', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_3'), href = '#')
        #       ),
        #       tags$label(
        #         class = 'label label-default bg-green', style = 'display: inline-block;',
        #         tags$a('Beta', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_5'), href = '#')
        #       ),
        #       tags$label(
        #         class = 'label label-default bg-blue', style = 'display: inline-block;',
        #         tags$a('Low Gamma', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_6'), href = '#')
        #       ),
        #       tags$label(
        #         class = 'label label-default bg-purple', style = 'display: inline-block;',
        #         tags$a('High Gamma', style = 'color:white!important', class = 'action-button',
        #                id = ns('freq_preset_7'), href = '#')
        #       )
        #     )
        #   ),
        #   div(
        #     style="flex-basis: 100%;",
        #     tags$small(textOutput(ns('frequency_txt')), style = 'color:#a1a1a1;')
        #   )
        # ),
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
          
        ),
        div(
          #### Load Estimation ####
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
      )
    })
    
    lapply(1:7, function(ii){
      observeEvent(input[[paste0('freq_preset_', ii)]], {
        updateSliderInput(session = session, inputId = 'frequencies', value = fband[[ii]])
      })
    })
    
    observe({
      local_data$mask = mask_file = input$mask
      check_result = isolate(local_data$check_result)
      tryCatch({
        mask_tbl = utils::read.csv(mask_file$datapath, header = T)
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
      # f = input$frequencies
      
      n_tf = length(freqs)
      # freqs = freqs[freqs %within% f]
      
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
      
      es = dipsaus::parse_svec(txt)
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
      txt = dipsaus::deparse_svec(es)
      if(ne){
        if(isTRUE(input$group_module)){
          sprintf('For group analysis, only *one* electrode is required to be loaded')
        } else {
          sprintf('%d electrodes selected (%s)', ne, txt)
        }
        
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
                    selectInput(ns(paste0('filter_lgt_' , ii)), paste0('Filter ', ii), choices = c('AND', 'OR'), selected = 'OR')
                  }else{
                    tags$label('Filter 1')
                  }
                })
              ),
              div(
                style="flex-basis: 25%;",
                selectInput(ns(paste0('filter_col_', ii)), 'Key', choices = vars, selected = vars[1])
              ),
              div(
                style="flex-basis: 25%;",
                selectInput(ns(paste0('filter_op_', ii)), 'Operator',
                            choices = c('=', '>', '<', '>=', '<=', 'IN', 'NOT IN', 'BETWEEN'),
                            selected = 'NOT IN')
              ),
              div(
                style="flex-basis: 25%;",
                textInput(ns(paste0('filter_val_', ii)), 'Value', value = '')
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
      txt = dipsaus::deparse_svec(res)
      try({
        sel = rep(TRUE, length(res))
        for(ii in 1:3){
          lgt = input[[paste0('filter_lgt_', ii)]]
          var = input[[paste0('filter_col_' , ii)]]
          op = input[[paste0('filter_op_' , ii)]]
          val = input[[paste0('filter_val_' , ii)]]
          
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
        txt = dipsaus::deparse_svec(res)
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
        
        # How many trials
        fluidRow(
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
                threeBrain::threejsBrainOutput(ns('three_viewer'), height = '600px')
                # threejsOutput(ns('three_viewer'), height = '500px')
              )
            )
          )
        )
      }
    })
    
    observe({
      load_mesh = input$load_mesh
      if(length(load_mesh) && is.logical(load_mesh)){
        local_data$load_mesh = load_mesh
        last_entry('load_mesh', !isFALSE(load_mesh), save = TRUE, group = group)
      }
    })
    
    output$ui_summary <- renderUI({
      validate(need(local_data$has_subject, message = ''))
      check_result = local_data$check_result
      
      project = input$project_name
      subject = input$subject_code
      
      # check if subject exists
      sub_dir = file.path(rave_options('data_dir'), project, subject, 'rave')
      if(!dir.exists(sub_dir)){
        return(p(
          'File path not found:', br(), sub_dir
        ))
      }
      
      # Trial
      epoch = input$epoch
      # Check epoch file
      msg = check_epoch(sprintf('%s/%s', project, subject), epoch_name = epoch)
      if(!isTRUE(msg)){
        msg = msg$message
        
        sub = as_subject(sprintf('%s/%s', project, subject), strict = FALSE)
        blocks = sub$preprocess_info('blocks')
        blocks = c(rep(blocks[1], 4), blocks[-1])
        examp = data.frame(
          Block = blocks,
          Time = c('5.12', '10.4', '...', rep('', length(blocks) - 3)),
          Trial = seq_along(blocks),
          Condition = c('cond1', 'cond2', '...', rep('', length(blocks) - 3))
        )
        examp = utils::capture.output({print(examp)})
        
        time_range = ''
        time_points = load_meta('time_points', project, subject)
        if(is.data.frame(time_points)){
          time_range = lapply(split(time_points, time_points$Block), function(s){
            rg = range(s$Time)
            
            tags$li(
              sprintf('Block %s, Time range: %.2f ~ %.2f seconds', unique(s$Block), rg[1], rg[2])
            )
          })
          time_range = tags$ul(tagList(time_range))
        }
        
        return(p(
          'Warning: block numbers do not match exactly, verify that this is OK (for instance, it is OK if a leading zero is missing).',
          br(), 'To continue loading data anyway, click the "Load Data" button ', br(),
          strong(msg), br(), 'Here is an example of epoch file:', br(),
          tags$pre(
            paste(examp, collapse = '\n')
          ),br(),
          '* Note: Time is relative to the start of the block. Trial is sequential across Blocks. Non-sequential trial numbers are OK, but they must be unique.',
          time_range
          
        ))
      }
      
      
      epoch_table = load_meta(meta_type = 'epoch', meta_name = epoch, project_name = project, subject_code = subject)
      
      n_trials = nrow(epoch_table)
      
      # Frequency
      w = check_result$log$preprocess$wavelet_log
      w = w[[length(w)]]
      freqs = w$frequencies#[w$frequencies %within% input$frequencies]
      n_freqs = length(freqs)
      
      # Time Points
      time_range = c(input$epoch_pre, input$epoch_post)
      total_time = sum(time_range)
      n_time_wave = w$target_srate * total_time + 1
      n_time_volt = check_result$log$preprocess$srate * total_time + 1
      
      # Electrodes
      elec = input$electrodes
      elec = dipsaus::parse_svec(elec)
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
      
      
      n_trials = as.integer(n_trials)
      n_time_volt = as.integer(n_time_volt)
      n_electrodes = as.integer(n_electrodes)
      n_time_wave = as.integer(n_time_wave)
      
      p(
        strong('Voltage: '), sprintf('%d Trials x %d Timepoints x %d Electrodes (%s)',
                                     n_trials, n_time_volt, n_electrodes, dipsaus::to_ram_size(s_volt)), br(),
        strong('Power/Phase: '), sprintf('%d Trials x %d Frequencies x %d Timepoints x %d Electrodes (%s each)',
                                         n_trials, n_freqs, n_time_wave, n_electrodes, dipsaus::to_ram_size(s_power)), br(),
        # strong('Brain: '), ifelse(brain_size > 0,
        #                           sprintf('surface volume, %s', dipsaus::to_ram_size(brain_size)),
        #                           'no surface volume file found'), br(),
        strong('Estimated Resource Required: '), sprintf('%s (memory)',
                                                         dipsaus::to_ram_size(max(s_volt, s_power))), br(),
        strong('Estimated Loading time: '), sprintf(
          'Power (%.0f sec), Phase (%.0f sec), Voltage(%.0f sec)',
          s_power / drive_speed / 1000^2 * 2,
          s_power / drive_speed / 1000^2 * 2,
          s_volt / drive_speed / 1000^2 * 2
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
    #   elec = dipsaus::parse_svec(elec)
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
    
    proxy = threeBrain::brain_proxy('three_viewer')
    output$three_viewer <- threeBrain::renderBrain({
      project = input$project_name
      subject = input$subject_code
      subject_id = sprintf('%s/%s', project, subject)
      ref = input$reference
      elec = dipsaus::parse_svec(input$electrodes)
      load_mesh = input$load_mesh
      
      # Check whether subject exists
      dirs = get_dir(subject_id = subject_id)
      
      validate(
        need(local_data$has_subject, message = ''),
        need(dir.exists(dirs$subject_dir), message = 'Subject does not exist!')
      )
      
      subject = as_subject(subject_id, reference = ref, strict = FALSE)
      
      brain = rave_brain2( subject = subject, surfaces = 'pial', 
                           compute_template = FALSE, 
                           usetemplateifmissing = FALSE )
      
      if( is.null(brain) ){
        return(NULL)
      }
      
      
      valid_e = subject$filter_valid_electrodes(elec)
      invalid_e = subject$filter_all_electrodes(elec)
      invalid_e = invalid_e[!invalid_e %in% valid_e]
      
      f = factor(c('Loading', 'Bad', 'Not Loaded'), levels = c('Loading', 'Bad', 'Not Loaded'))
      tbl = subject$electrodes
      tbl$Value = f[3]
      tbl$Value[tbl$Electrode %in% valid_e] = f[1]
      tbl$Value[tbl$Electrode %in% invalid_e] = f[2]
      
      is_template = FALSE
      if(length(brain$template_object)){
        brain = brain$template_object
        is_template = TRUE
      }
      brain$set_electrode_values(table_or_path = tbl[, c('Electrode', 'Value')])
      
      if(is_template){
        for(e in brain$electrodes$objects){
          e$name = paste(stringr::str_replace(e$name, '^[^,]*, ', ''), '(template brain)')
        }
      }
      
      for(e in valid_e){
        if( !is.null(brain$electrodes$objects[[e]]) ){
          brain$electrodes$objects[[e]]$custom_info = paste('Reference Group:', tbl$Group[tbl$Electrode == e])
        }
      }
      for(e in invalid_e){
        if( !is.null(brain$electrodes$objects[[e]]) ){
          brain$electrodes$objects[[e]]$custom_info = paste('Reference Group:', tbl$Group[tbl$Electrode == e], '(electrode not used)')
        }
      }
      
      zoom = shiny::isolate({
        proxy$main_camera$zoom
      })
      theme = get_rave_theme()$themes[[1]]
      background = ifelse(theme == 'dark', '#1E1E1E', '#FFFFFF')
      brain$plot(control_panel = FALSE, side_canvas = FALSE, 
                 default_colormap = 'Value', volumes = FALSE, 
                 surfaces = load_mesh, start_zoom = zoom, 
                 timestamp = FALSE, background = background,
                 palettes = list(Value = c('navyblue', 'red', '#e2e2e2')))
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
      
      epoch = input$epoch
      epoch_range = c(input$epoch_pre, input$epoch_post)
      reference = input$reference
      subject_id = paste0(project_name , '/' , subject_code)
      electrodes = dipsaus::parse_svec(input$electrodes)
      
      tmp_subject = Subject$new(project_name = project_name, subject_code = subject_code, reference = reference, strict = FALSE)
      electrodes = tmp_subject$filter_valid_electrodes(electrodes)
      
      if(length(electrodes) == 0){
        showNotification('You must select at least one electrode', type = 'error', id = ns('data_import'))
        return(NULL)
      }
      
      frequencies = tmp_subject$frequencies$Frequency #input$frequencies
      freqs = tmp_subject$frequencies$Frequency %within% frequencies
      if(!sum(freqs)){
        showNotification('No frequency found in your selected frequency band', type = 'error', id = ns('data_import'))
        return(NULL)
      }
      
      if(sum(epoch_range) == 0){
        showNotification('Please select valid time range', type = 'error', id = ns('data_import'))
        return(NULL)
      }
      
      # register
      
      last_entry('project_name', project_name, save = TRUE, group = group)
      last_entry('subject_code', subject_code, save = TRUE, group = group)
      last_entry('electrodes', dipsaus::deparse_svec(electrodes), save = TRUE, group = group)
      last_entry('epoch_name', epoch, save = TRUE, group = group)
      last_entry('time_range', epoch_range, save = TRUE, group = group)
      last_entry('reference_name', reference, save = TRUE, group = group)
      clear_cache(levels = 1)
      if(isTRUE(input$group_module)){
        electrodes = electrodes[1]
      }
      gc()
      rave_prepare(
        subject = subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        frequency_range = frequencies,
        reference = reference,
        attach = FALSE,
        data_types = NULL
      )
      
      # refresh UIs
      global_reactives$force_refresh_all = Sys.time()
      global_reactives$has_data = Sys.time()
      
      shinyjs::removeClass(selector = 'body', class = "rave-noscroll");
      removeModal()
      # Remove
      local_data$prevent_dblclick = TRUE
      catgl('Subject loaded, trigger module to refresh...')
    })
    
    
    ##### End of server #####
    
    # onload, check if data has been loaded into datarepo
    data_loaded = rlang::env_has(
      data_env,
      nms = c(
        ".private",
        "data_check",
        "module_tools",
        "preload_info",
        "subject"
      ),
      inherit = FALSE
    )
    if(all(data_loaded)){
      global_reactives$force_refresh_all = Sys.time()
      global_reactives$has_data = Sys.time()
    } else {
      open_modal()
    }
    
  }
  
  launch = function(){
    shinyjs::click(ns('data_select'))
  }
  
  
  return(list(
    header = header,
    control = control,
    server = server,
    launch = launch
  ))
}

