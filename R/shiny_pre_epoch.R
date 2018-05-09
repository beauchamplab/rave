

rave_pre_epoch <- function(module_id = 'EPOCH_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    shinydashboard::tabBox(
      id = ns('sidebar'),
      width = sidebar_width,
      shiny::tabPanel(
        title = 'Epoching',
        fluidRow(
          column(
            12,
            uiOutput(ns('inner_ui')), # need notch to be applied
            uiOutput(ns('inner_ui2')),
            uiOutput(ns('inner_ui3')),
            uiOutput(ns('inner_ui4'))
          )
        )
      )
    ),
    shinydashboard::tabBox(
      id = ns('main'),
      width = 12 - sidebar_width,
      shiny::tabPanel(
        title = 'Select',
        fluidRow(
          column(6,
                 plotOutput(ns('console_plot'), click = ns('console_plot_clicked'))),
          column(6,
                 DT::dataTableOutput(ns('epoch_tmp')),
                 uiOutput(ns('epoch_tmp_ui'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Check',
        fluidRow(
          column(6,
                 plotOutput(ns('epoch_plot'), brush = ns('epoch_plot_brush')),
                 plotOutput(ns('epoch_plot_sub'))
                 ),
          column(6,DT::dataTableOutput(ns('epoch_table')),
                 uiOutput(ns('epoch_table_ui'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Preview & Export',
        fluidRow(
          column(12,
                 div(
                   textInput(ns('save_name'), 'Epoch Name:'),
                   actionLink(ns('save'), 'Export Epoch'),
                   textOutput(ns('save_path'), inline = T)
                 ),
                 DT::dataTableOutput(ns('epoch_preview'))
          )
        )
      )
    )
  )

  server = function(input, output, session, user_data){
    default_epoch = data.frame(
      Block = NULL,
      Trial = NULL,
      Onset = NULL,
      stringsAsFactors = F
    )

    local_data = reactiveValues(
      save = Sys.Date(),
      trials = NULL,
      epochs = list(),
      epoch_saved = default_epoch,
      epoch_table = default_epoch,
      epoch_tmp = default_epoch,
      dat = NULL,
      plot_signal = NULL,
      raw_signal = NULL,
      thred = NULL,
      selected_rows = NULL,
      epoch_page = 1,
      sample_rate = 30000,
      epoch_file = NULL,
      epoch_table_start = 0
    )

    generate_epoch = function(epochs){
      tbl = default_epoch
      if(length(epochs)){
        for(i in 1:length(epochs)){
          epo = epochs[[i]]
          if(nrow(epo)){
            epo = epo[, c('Block', 'Trial', 'Onset')]
            epo$Trial = 1:nrow(epo)
            tbl = rbind(tbl, epo)
          }
        }
      }
      tbl
    }
    output$epoch_preview <- DT::renderDataTable({
      tbl = generate_epoch(epochs = local_data$epochs)

      if(nrow(tbl)){
        DT::formatRound(
          DT::datatable(tbl, options = list(
            pageLength = 50
          )),
          columns=c('Onset'), digits=4
        )
      }

    })

    stage_epoch = function(current_block = NULL, saved = NULL){
      if(is.null(current_block)){
        current_block = isolate(input$current_block)
      }
      if(length(current_block) != 1){
        return()
      }
      if(is.null(saved)){
        saved = isolate(local_data$epoch_saved)
      }else{
        if(nrow(saved)){
          saved$Staged = 1:nrow(saved)
        }
        local_data$epoch_saved = saved
      }

      local_data$epochs[[current_block]] = saved
    }

    observeEvent(input$current_block, {
      current_block = input$current_block
      local_data$epoch_saved = get_val(local_data$epochs, current_block, default = default_epoch)
      local_data$epoch_tmp = default_epoch
      local_data$epoch_table = default_epoch

      epoch_file = isolate(local_data$epoch_file)
      if(length(epoch_file) == 1){
        local_data$epoch_file = str_replace(epoch_file, 'Datafile[^_]*', sprintf('Datafile%s', current_block))
      }else{
        local_data$epoch_file = NULL
      }
    })


    output$inner_ui <- renderUI({
      validate(
        need(user_data$has_notch, 'Please Apply Notch Filter First.')
      )

      tagList(

        selectInput(ns('current_block'), 'Block', choices = user_data$subject$blocks),
        numericInput(ns('total_trials'), 'Number of Trials:', min = 0, value = 0),
        hr()
      )
    })

    output$inner_ui2 <- renderUI({
      current_block = input$current_block
      if(user_data$has_notch && length(current_block)){
        s = user_data$subject
        all_files = list.files(file.path(s$dirs$pre_subject_dir, current_block))
        excl_files = sprintf('%sDatafile%s_ch%s.mat', s$subject_code, current_block, s$channels)

        selected = isolate(local_data$epoch_file)
        if(length(selected) != 1 || !selected %in% all_files){
          selected = all_files[!all_files %in% excl_files]
          if(length(selected)){
            selected = selected[1]
          }
        }

        tagList(
          selectInput(ns('epoch_file'), 'Epoch File:', choices = all_files, selected = selected)
        )
      }
    })

    output$inner_ui3 <- renderUI({
      s = user_data$subject

      if(is(s, 'SubjectInfo2') && length(input$epoch_file)){
        epoch_raw = file.path(s$dirs$pre_subject_dir, input$current_block, input$epoch_file)
        dat = R.matlab::readMat(epoch_raw)
        local_data$dat = dat
        choices = c(names(dat), 'Customized')
        tagList(
          selectInput(ns('plot_var'), 'Variable to be ploted:', choices = choices,
                      selected = isolate(local_data$plot_var))
        )
      }else{
        NULL
      }

    })

    output$inner_ui4 <- renderUI({
      plot_var = input$plot_var
      dat = local_data$dat

      if(length(plot_var)){

        vars = paste('-', names(dat), collapse = ',\n')
        if(plot_var == 'Customized'){
          sample_rate = 100
        }else{
          # guess if sample rate is too low
          sample_rate = get_val(isolate(local_data$sample_rate), default = 30000)
          ecog_srate = user_data$subject$srate
          if(sample_rate < ecog_srate){
            sample_rate = ecog_srate
          }
        }

        tagList(
          div(
            class = ifelse(plot_var == 'Customized', '', 'hidden'),
            textAreaInput(ns('custom_code'), 'Customized Code: ',
                          rows = 10, resize = 'vertical',
                          placeholder = sprintf('Use the following variable(s):%s', vars))
          ),
          numericInput(ns('sample_rate'), 'Variable sample rate', min = 0, value = sample_rate),
          numericInput(ns('plot_range'), 'Plot range:', min = 0, value = 0),
          numericInput(ns('lag'), 'Difference:', value = 0L, step = 1L),
          checkboxInput(ns('is_symmetric'), 'Symmetric', value = FALSE),
          numericInput(ns('symmetric'), 'Symmetric by:', value = 0),
          selectInput(ns('direction'), 'Threshold Direction', choices = c('Above', 'Below'), selected = 'Above'),
          numericInput(ns('min_trial_duration'), 'Minimal trial duration (s):', min = 0L, value = 0L)
        )
      }
    })

    output$epoch_tmp_ui <- renderUI({
      validate(need(nrow(local_data$epoch_tmp) > 0, 'Click the plot left to select data.'))
      div(
        actionButton(ns('toggle_sel'), 'Toggle selection'),
        actionButton(ns('clear_sel'), 'Clear all'),
        actionButton(ns('stage_sel'), 'Stage selection')
      )
    })

    output$epoch_table_ui <- renderUI({
      validate(need(nrow(local_data$epoch_table) > 0, "You don't have any epoch data selected"))
      div(
        actionButton(ns('reset_sel_1'), 'Clear unstaged selection'),
        actionButton(ns('clear_sel_1'), 'Clear all'),
        actionButton(ns('stage_sel_1'), 'Stage selection')
      )
    })

    epoch_tmp_proxy = DT::dataTableProxy('epoch_tmp')
    epoch_table_proxy = DT::dataTableProxy('epoch_table')

    observeEvent(input$clear_sel_1, {
      DT::selectRows(epoch_table_proxy, 1:nrow(local_data$epoch_table))
    })

    observeEvent(input$reset_sel_1, {
      DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
    })

    # cache input
    observe({
      local_data$sample_rate = input$sample_rate
      local_data$selected_rows = input$epoch_tmp_rows_selected
      local_data$epoch_file = input$epoch_file
      local_data$plot_var = input$plot_var
    })

    observeEvent(input$clear_sel, {
      DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
    })

    observeEvent(input$stage_sel, {
      stage_epoch(saved = local_data$epoch_table)
      DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
    })
    observeEvent(input$stage_sel_1, {
      stage_epoch(saved = local_data$epoch_table)
      DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
    })





    observeEvent(input$epoch_table_rows_selected, {
      tbl = isolate(local_data$epoch_table)
      sel = input$epoch_table_rows_selected
      saved = isolate(local_data$epoch_saved)


      tmp_sel = isolate(local_data$selected_rows)
      if(length(sel)){
        local_data$epoch_table_start = max(min(sel)-2, 0)

        row_sel = tbl$Row[sel]
        row_sel = row_sel[!is.na(row_sel)]


        if(length(row_sel)){
          tmp_sel = tmp_sel[!tmp_sel %in% row_sel]
          if(length(tmp_sel)){
            DT::selectRows(epoch_tmp_proxy, as.numeric(tmp_sel))
          }else{
            DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
          }
        }
        # remove staged rows
        unstage = tbl$Staged[sel]
        unstage = unstage[!is.na(unstage)]
        if(length(unstage)){
          saved = saved[!saved$Staged %in% unstage, ]
          if(nrow(saved)){
            saved$Staged = 1:nrow(saved)
          }
          stage_epoch(saved = saved)
        }



      }
    }, priority = 10L)


    output$epoch_plot_sub <- renderPlot({
      e = input$epoch_plot_brush
      srate = input$sample_rate
      raw_s = local_data$raw_signal
      tbl = local_data$epoch_table
      validate(need(!zero_length(e, srate, raw_s), ''))

      xrange = c(e$xmin, e$xmax)

      yrange = c(e$ymin, e$ymax)
      tp = as.integer(xrange * srate)
      tp[1] = max(1, tp[1])
      tp[2] = max(tp[2], tp[1] + 10)
      ind = as.integer(seq(tp[1], tp[2], length.out = min(diff(tp) + 1, 20000)))

      s = raw_s[ind]
      t = ind / srate
      if(nrow(tbl)){
        win = is_within(tbl$Onset, range(t))
      }else{
        win = FALSE
      }
      nonset = sum(win)
      plot(
        t, s, type = 'l', col ='grey', main = sprintf('%.3f - %.3f (seconds, total %d trials)', min(t), max(t), nonset),
        xlab = 'Time (s)', ylab = 'Value', las = 1,
        cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.2
      )
      if(nonset > 0){
        abline(v = tbl$Onset[win], col = 'red')
        local_data$epoch_table_start = max(which(win)[1] - 1, 0)
      }
    })


    observeEvent(input$toggle_sel, {
      sel = local_data$selected_rows
      tmp_tbl = local_data$epoch_tmp
      n_row = nrow(tmp_tbl)
      if(n_row){
        n_row = 1:n_row
        sel = n_row[!n_row %in% sel]
        if(length(sel)){
          DT::selectRows(epoch_tmp_proxy, sel)
        }else{
          DT::reloadData(epoch_tmp_proxy, resetPaging = F, clearSelection = 'all')
        }
      }
    })

    observe({
      tmp = local_data$epoch_tmp
      tbl = local_data$epoch_saved
      sel = local_data$selected_rows
      if(length(sel)){
        tmp = tmp[sel, ]; tmp$Row = sel
        if(nrow(tbl)){
          new_tbl = merge(tbl[, c('Block', 'Onset', 'Staged')], tmp[, c('Block', 'Onset', 'Row')], by = c('Block', 'Onset'), all = TRUE)
        }else{
          tmp$Staged = NA
          new_tbl = tmp
        }
        new_tbl = new_tbl[order(new_tbl$Block, new_tbl$Onset), ]
        new_tbl$Trial = 1:nrow(new_tbl)
        local_data$epoch_table = new_tbl
      }else{
        local_data$epoch_table = tbl
      }
    }, priority = 0L)

    observe({
      plot_var = input$plot_var
      dat = local_data$dat
      local_data$custom_signal = NULL
      if(!zero_length(plot_var, dat) && plot_var == 'Customized'){
        tryCatch({
          env = new.env(parent = globalenv())
          srate = as.integer(input$sample_rate)
          env$to_signal <- function(time, value = 1){
            ind = round(time * srate)
            s = rep(0, max(ind) + srate)
            s[ind] = value
            s
          }
          env$.code = parse(text = input$custom_code)
          local_data$custom_signal = eval(env$.code, envir = dat, enclos = env)
        }, error = function(e){
          logger(e$message, level = 'WARNING')
          return(NULL)
        })
      }

    }, priority = 100L)

    observe({
      local_data$plot_signal = NULL
      local_data$raw_signal = NULL
      lag = get_val(input$lag, default = 0)
      plot_var = input$plot_var
      is_sym = get_val(input$is_symmetric, default = F)
      if(!zero_length(local_data$dat, plot_var, lag, na.rm = 3)){
        if(plot_var == 'Customized'){
          raw_s = s = local_data$custom_signal
        }else{
          raw_s = s = local_data$dat[[plot_var]]
        }
        if(length(s) == 0){
          return()
        }
        if(lag > 0){
          s = s[1:(length(s) - lag)] - s[-(1:lag)]
        }else if(lag < 0){
          s = c(s, rep(NA, -lag)) - c(rep(NA, -lag), s)
          s[is.na(s)] = 0
        }

        if(is_sym){
          sym = get_val(input$symmetric, default = 0)
          s = abs(s - sym)
        }
        local_data$plot_signal = s
        local_data$raw_signal = raw_s
      }
    })

    output$epoch_plot <- renderPlot({
      ylim = get_val(input$plot_range, default = 0)
      s = local_data$raw_signal
      if(length(s)){

        s = s[seq(1, length(s), by = local_data$sample_rate / 100)]

        s_l = length(s)
        time = (1:s_l) / 100
        if(ylim > 0){
          plot(time, s, type = 'l', ylim = c(-ylim, ylim))
        }else{
          plot(time, s, type = 'l')
        }




        epoch_table = local_data$epoch_table
        if(nrow(epoch_table)){

          with(epoch_table, {
            # unstaged
            unstaged = is.na(Staged)
            if(sum(unstaged)){
              abline(v = Onset[unstaged], col = 'red')
            }

            if(sum(!unstaged)){
              abline(v = Onset[!unstaged], col = 'blue')
            }
          })

        }
      }
    })

    output$console_plot <- renderPlot({
      ylim = get_val(input$plot_range, default = 0)
      s = local_data$plot_signal
      is_sym = input$is_symmetric
      if(length(s) && length(ylim) && length(is_sym)){

        s = s[seq(1, length(s), by = local_data$sample_rate / 100)]

        s_l = length(s)
        time = (1:s_l) / 100
        if(ylim > 0){
          if(is_sym){
            ylim = c(0, ylim)
          }else{
            ylim = c(-ylim, ylim)
          }
          plot(time, s, type = 'l', ylim = ylim)
        }else{
          plot(time, s, type = 'l')
        }

        if(length(local_data$thred) == 1){
          abline(h = local_data$thred, col = 'red')
        }
      }
    })

    observeEvent(input$console_plot_clicked, {
      e = input$console_plot_clicked
      if(!is.null(e)){
        local_data$thred = e$y
      }
    })

    output$epoch_table <- DT::renderDataTable({
      tbl = local_data$epoch_table
      rownames(local_data$epoch_table) = NULL
      if(nrow(tbl)){
        tbl$Staged = !is.na(tbl$Staged)
        tbl = tbl[, c('Block', 'Trial', 'Onset', 'Staged')]
        tbl$Trial = 1:nrow(tbl)
        tbl$Since_Last_Onset = c(NA, diff(tbl$Onset))
        DT::formatRound(
          DT::datatable(tbl, options = list(
            displayStart = min(max(0, local_data$epoch_table_start), nrow(tbl)-1)
          )),
          columns=c('Onset', 'Since_Last_Onset'), digits=4
        )
      }else{
        data.frame()
      }

    })



    output$epoch_tmp <- DT::renderDataTable({
      tbl = local_data$epoch_tmp

      if(nrow(tbl)){
        tbl$Since_Last_Onset = c(NA, diff(tbl$Onset))
        DT::formatRound(
          DT::datatable(tbl),
          columns=c('Onset', 'Since_Last_Onset'), digits=4
        )
      }
    })



    observe({
      s = local_data$plot_signal
      thred = local_data$thred
      sample_rate = local_data$sample_rate
      direction = input$direction
      min_trial_duration = input$min_trial_duration
      current_block = input$current_block
      if(length(min_trial_duration) != 1){
        min_trial_duration = 0
      }
      tryCatch({
        if(input$direction == 'Above'){
          sel = s >= thred
        }else{
          sel = s <= thred
        }
        max_lag = max(1, min_trial_duration * sample_rate)
        tp = rave:::deparse_selections(which(sel), concatenate = F, max_lag = max_lag)
        tp = as.integer(str_extract(tp, '^[0-9]*'))
        tp = tp[!is.na(tp)]
        local_data$epoch_tmp = data.frame(
          Block = current_block,
          Trial = 1:length(tp),
          Onset = tp / sample_rate,
          stringsAsFactors = F
        )
      }, error = function(e){
        local_data$epoch_tmp = default_epoch
      })


    })

    observe({
      s = user_data$subject
      if(is(s, 'SubjectInfo2')){
        meta_dir = s$dirs$meta_dir
        meta_dir = try_normalizePath(meta_dir)
        name = get_val(input$save_name, default = 'Temp', .invalids = c('null', 'na', 'blank'))
        local_data$export_path = file.path(meta_dir, sprintf('epoch_%s.csv', name))
      }else{
        local_data$export_path = NULL
      }
    })
    observeEvent(input$save, {
      if(length(local_data$export_path)){
        write.csv(generate_epoch(epochs = local_data$epochs), file = local_data$export_path, row.names = F)
      }
    })
    output$save_path <- renderText({
      validate(need(length(local_data$export_path) == 1, ''))
      local_data$export_path
    })

  }

  return(list(body = body, server = server))
}
