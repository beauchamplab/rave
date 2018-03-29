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
        title = 'Selection',
        fluidRow(
          column(6,
                 plotOutput(ns('console_plot'), click = ns('console_plot_clicked'))),
          column(6,DT::dataTableOutput(ns('epoch_tmp')))
        )
      ),
      shiny::tabPanel(
        title = 'Preview',
        fluidRow(
          column(6,
                 plotOutput(ns('epoch_plot'))),
          column(6,DT::dataTableOutput(ns('epoch_table')))
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
      epoch_saved = default_epoch,
      epoch_table = default_epoch,
      epoch_tmp = default_epoch,
      dat = NULL,
      plot_signal = NULL,
      thred = NULL
    )

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
      if(user_data$has_notch && length(input$current_block)){
        s = user_data$subject
        all_files = list.files(file.path(s$dirs$pre_subject_dir, input$current_block))
        excl_files = sprintf('%sDatafile%s_ch%s.mat', s$subject_code, input$current_block, s$channels)
        selected = all_files[!all_files %in% excl_files]
        if(length(selected)){
          selected = selected[1]
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
        print(epoch_raw)
        dat = R.matlab::readMat(epoch_raw)
        local_data$dat = dat
        choices = names(dat)
        tagList(
          selectInput(ns('plot_var'), 'Variable to be ploted:', choices = choices),
          numericInput(ns('sample_rate'), 'Variable sample rate', min = 0, value = 30000),
          numericInput(ns('lag'), 'Delta:', value = 0L, step = 1L),
          numericInput(ns('plot_range'), 'Plot range:', min = 0, value = 0)
        )
      }else{
        NULL
      }

    })


    observe({
      tbl = isolate(local_data$epoch_table)
      sel = input$epoch_table_rows_selected
      if(length(sel)){
        new_tbl = tbl[-sel,]
        n_trial = nrow(new_tbl)
        if(n_trial){
          new_tbl$Trial = 1:n_trial
        }
        local_data$epoch_table = new_tbl
      }
    })

    observe({
      tmp = local_data$epoch_tmp
      tbl = isolate(local_data$epoch_saved)
      sel = input$epoch_tmp_rows_selected
      if(length(sel)){
        new_tbl = rbind(tbl, tmp[sel, ])
        new_tbl = new_tbl[order(new_tbl$Block, new_tbl$Onset), ]
        new_tbl$Trial = 1:nrow(new_tbl)
        local_data$epoch_table = new_tbl
      }
    })

    observe({
      local_data$plot_data = NULL
      if(length(input$plot_var)){
        s = local_data$dat[[input$plot_var]]
        lag = as.integer(input$lag)
        if(length(s)){
          s = as.vector(s)


          if(lag > 0){
            s = s[1:(length(s) - lag)] - s[-(1:lag)]
          }else if(lag < 0){
            s = c(s, rep(NA, -lag)) - c(rep(NA, -lag), s)
            s[is.na(s)] = 0
          }

          local_data$plot_data = s
        }
      }
    })

    output$epoch_plot <- renderPlot({
      ylim = input$plot_range
      s = local_data$plot_data
      if(length(s)){

        s = s[seq(1, length(s), by = input$sample_rate / 100)]

        s_l = length(s)
        time = (1:s_l) / 100
        if(ylim > 0){
          plot(time, s, type = 'l', ylim = c(-ylim,ylim))
        }else{
          plot(time, s, type = 'l')
        }

        epoch_table = local_data$epoch_table
        epoch_table = epoch_table[epoch_table$Block == input$current_block, ]
        if(nrow(local_data$epoch_table)){
          abline(v = epoch_table$Onset, col = 'blue')
        }

        epoch_saved = local_data$epoch_saved
        epoch_saved = epoch_saved[epoch_saved$Block == input$current_block, ]
        if(nrow(local_data$epoch_saved)){
          abline(h = local_data$epoch_saved$Onset, col = 'red')
        }
      }
    })

    output$console_plot <- renderPlot({
      ylim = input$plot_range
      s = local_data$plot_data
      if(length(s)){

        s = s[seq(1, length(s), by = input$sample_rate / 100)]

        s_l = length(s)
        time = (1:s_l) / 100
        if(ylim > 0){
          plot(time, s, type = 'l', ylim = c(-ylim,ylim))
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
        print(e)
        local_data$thred = e$y
      }
    })

    output$epoch_table <- DT::renderDataTable({
      tbl = local_data$epoch_table
      tbl
    })


    output$epoch_tmp <- DT::renderDataTable({
      tbl = local_data$epoch_tmp
      tbl
    })

    observe({
      s = local_data$plot_data
      thred = local_data$thred
      tp = rave:::deparse_selections(which(s > thred))
      tp = as.integer(str_extract(str_split(tp, ',', simplify = T), '^[0-9]*'))
      if(length(tp) && !any(is.na(tp))){
        local_data$epoch_tmp = data.frame(
          Block = input$current_block,
          Trial = 1:length(tp),
          Onset = tp / input$sample_rate,
          stringsAsFactors = F
        )
      }
    })

  }

  return(list(body = body, server = server))
}
