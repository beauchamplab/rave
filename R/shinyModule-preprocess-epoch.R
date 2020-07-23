pre_epoch3 <- function(module_id = 'EPOCH_M', sidebar_width = 2, doc_prefix = 'ravepreprocessepoch', ...){
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  default_epoch = data.frame(
    Block = NULL,
    Trial = NULL,
    Onset = NULL,
    stringsAsFactors = FALSE,
    staged = list()
  )

  body = fluidRow(
    box(
      width = sidebar_width,
      title = 'Trial Epoch',
      box_link = sprintf(url_format, 'input', 'trialepoch'),
      uiOutput(ns('inner_ui')), # need notch to be applied
      uiOutput(ns('inner_ui2')),
      uiOutput(ns('inner_ui3')),
      uiOutput(ns('inner_ui4'))
    ),
    tabBox(
      id = ns('main'),
      width = 12 - sidebar_width,
      title = 'Inspection Panel',
      box_link = sprintf(url_format, 'output', 'inspectionpanel'),
      shiny::tabPanel(
        title = 'Select',
        fluidRow(
          column(8,
                 div(class = 'rave-border-box', plotOutput(
                   ns('console_plot'),
                   dblclick = ns('console_plot_clicked'),
                   brush = brushOpts(
                     ns('console_plot_brush'),
                     clip = T,
                     direction = 'x'
                   )
                 )))
          ,
          column(4,
                 div(class = 'rave-border-box',
                     plotOutput(ns('console_plot_sub')))),
          column(5,
                 div(class = 'rave-border-box', style = 'min-height:500px',
                     DT::dataTableOutput(ns('epoch_tmp')))),
          column(
            2,
            div(
              style = 'min-height:500px; padding-top: 100px; text-align: center;',
              actionButton(ns('toggle'), 'Toggle Selection', width = '100%'),
              actionButton(ns('clear'), 'Clear Selection', width = '100%'),
              hr(),
              actionButton(ns('stage'), 'Save Changes', width = '100%'),
              hr(),
              p('Danger:'),
              actionButton(ns('discard'), 'Discard Epoch', width = '100%')
            )
          ),
          column(5,
                 div(class = 'rave-border-box', style = 'min-height:500px',
                     DT::dataTableOutput(ns('epoch_staged'))))
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
                 hr(),
                 DT::dataTableOutput(ns('epoch_preview'))
          )
        )
      )
    )
  )

  server = function(input, output, session, user_data, utils, ...){
    local_data = reactiveValues(
      blocks = '',
      efile = NULL
    )

    local_env = new.env()

    # Step 1: init
    observe({
      user_data$reset
      # get epoch raw files (potential)
      if(utils$notch_filtered()){

        local_data$blocks = utils$get_blocks()

        dirs = utils$get_from_subject('dirs', list(), F)
        epochs = list.files(dirs$meta_dir, pattern = '^epoch_.+.[cC][sS][vV]')
        if(length(epochs)){
          epochs = stringr::str_match(epochs, '^epoch_(.+).[cC][sS][vV]')[,2]
        }
        local_data$epoch_files = c('New Epoch...', epochs)

        local_data$staged = list()
      }else{
        local_data$staged = list()
        local_data$blocks = ''
      }
    })

    observe({
      epoch_name = input$epoch_name
      if(length(epoch_name)){

        epoch_name = sprintf('epoch_%s.csv', epoch_name)
        dirs = utils$get_from_subject('dirs', list(), F)

        fpath = file.path(dirs$meta_dir, epoch_name)
        if(file.exists(fpath)){
          catgl('Loading epoch from file.')
          tbl = safe_read_csv(fpath, colClasses = c('character', 'numeric'))
          local_data$staged = sapply(utils$get_blocks(), function(b){
            sub = tbl[tbl$Block == b,]
            if(nrow(sub)){
              sub = sub[, c('Block', 'Time')]
            }
            sub
          }, simplify = F, USE.NAMES = T)
        }
      }
    })

    observe({
      block = input$block
      if(length(block) && !is.blank(block)){
        dirs = utils$get_from_subject('dirs', list(), F)
        subject_code = utils$get_from_subject('subject_code')


        raw_files = list.files(file.path(dirs$pre_subject_dir, block))
        excl_files = sprintf('%sDatafile%s_ch%s.mat', subject_code, block, utils$get_electrodes())
        selected_files = raw_files[!raw_files %in% excl_files]
      }
    })



    # step 2: UI
    output$inner_ui <- renderUI({
      blocks = local_data$blocks
      validate(need(length(blocks) && !is.blank(blocks), 'Apply notch filter first.'))
      tagList(
        selectInput(ns('epoch_name'), 'Epoch Name', choices = local_data$epoch_files),
        selectInput(ns('block'), 'Block', choices = blocks)
      )
    })

    output$inner_ui2 <- renderUI({
      block = input$block
      validate(need(length(block) && !is.blank(block), ''))
      dirs = utils$get_from_subject('dirs', list(), F)
      subject_code = utils$get_from_subject('subject_code')


      raw_files = list.files(file.path(dirs$pre_subject_dir, block))
      excl_files = sprintf('%sDatafile%s_ch%s.mat', subject_code, block, utils$get_electrodes())
      # guess selected file
      selected_files = isolate(local_data$efile)
      if(length(selected_files)){
        selected_files = stringr::str_replace(selected_files, sprintf('^%sDatafile[\\w]+_', subject_code), sprintf('%sDatafile%s_', subject_code, block))
        if(!selected_files %in% raw_files){
          selected_files = NULL
        }
      }
      if(!length(selected_files)){
        selected_files = raw_files[!raw_files %in% excl_files]
      }

      if(length(selected_files)){
        selected_files = selected_files[1]
      }else{
        selected_files = NULL
      }


      selectInput(ns('epoch_file'), 'Epoch File', choices = raw_files, selected = selected_files)
    })

    output$inner_ui3 <- renderUI({
      block = input$block
      local_data$efile = efile = input$epoch_file
      dirs = utils$get_from_subject('dirs', list(), F)



      validate(
        need(length(block) && !is.blank(block), ''),
        need(length(efile) && length(dirs) && file.exists(efile_path <- file.path(dirs$pre_subject_dir, block, efile)), '')
      )

      local_data$raw_data = raw_data = raveio::read_mat(efile_path)
      dnames = names(raw_data)
      dnames %?<-% 'Default...'
      dname_sel = ifelse('analogTraces' %in% dnames, 'analogTraces', dnames[1])
      sample_rate = get_val(isolate(local_data$sample_rate), default = 30000)
      min_trial_duration = get_val(isolate(local_data$min_trial_duration), default = 0L)
      is_symmetric = get_val(isolate(local_data$is_symmetric), default = FALSE)
      direction = get_val(isolate(local_data$direction), default = 'Above')
      lag = get_val(isolate(local_data$lag), default = FALSE)


      tagList(
        selectInput(ns('data_name'), 'Variable name', choices = dnames, selected = dname_sel),
        numericInput(ns('sample_rate'), 'Variable sample rate', min = 0, value = sample_rate),
        numericInput(ns('plot_range'), 'Plot range:', min = 0, value = 0),
        checkboxInput(ns('lag'), 'Difference plot:', value = lag),
        checkboxInput(ns('is_symmetric'), 'Use absolute value', value = is_symmetric),
        selectInput(ns('direction'), 'Threshold select', choices = c('Above', 'Below'), selected = direction),
        numericInput(ns('min_trial_duration'), 'Minimal trial duration (s):', min = 0L, value = min_trial_duration)
      )
    })

    observe({
      local_data$min_trial_duration = input$min_trial_duration
      local_data$is_symmetric = input$is_symmetric
      local_data$direction = input$direction
      local_data$lag = input$lag
    })




    observe({
      # extract data
      srate = input$sample_rate
      if(is_invalid(srate)){
        srate = 30000
      }
      local_data$sample_rate = srate
      lag = input$lag
      lag %?<-% FALSE
      is_symmetric = input$is_symmetric
      is_symmetric %?<-% FALSE

      data_name = input$data_name
      dat = NULL
      compressed_signal = NULL
      raw_compressed_singal = NULL

      if(length(data_name)){
        if(data_name == 'Default...'){
          dat = as.vector(unlist(local_data$raw_data))
        }else{
          dat = as.vector(local_data$raw_data[[data_name]])
        }
      }

      if(!is.null(dat) && is.numeric(dat) && srate > 0){

        cr = srate / 100
        ind = round(seq(1, length(dat), by = cr))

        raw_compressed_singal = dat[ind]

        if(lag){
          dat = diff.default(dat, 1)
        }

        if(is_symmetric){
          dat = abs(dat)
        }



        compressed_signal = dat[ind]

        local_data$time = seq_along(dat) / srate
        local_data$compressed_time = seq_along(compressed_signal) / 100
      }

      local_data$signal = dat

      local_data$compressed_signal = compressed_signal
      local_data$raw_compressed_singal = raw_compressed_singal
    })

    plot_volt = function(time, signal, plot_range, use_abs, vlines = NULL, hlines = NULL, vcols = 2, ...){
      if(!length(time)){
        return()
      }
      ylim = plot_range
      if(is_invalid(ylim) || ylim <= 0){
        ylim = NULL
      }else{
        if(use_abs){
          ylim = c(0, plot_range)
        }else{
          ylim = c(-plot_range, plot_range)
        }
      }

      if(use_abs){
        main = 'Absolute(Signal)'
      }else{
        main = 'Signal'
      }
      graphics::plot(time, signal, type = 'l', ylim = ylim, main = main, las = 1, xlab = 'Time (s)', ylab = 'Strength', ...)
      if(!is.null(hlines)){
        graphics::abline(h = hlines, col = 'blue')
      }
      if(!is.null(vlines)){
        graphics::abline(v = vlines, col = vcols)
      }
    }

    # Renderings
    output$console_plot <- renderPlot({
      is_symmetric = input$is_symmetric
      is_symmetric %?<-% FALSE
      plot_range = input$plot_range
      time = local_data$compressed_time
      signal = local_data$compressed_signal
      block = input$block

      validate(need(length(signal), 'No signal detected.'))

      vcol = NULL
      st = local_data$selected_time
      if(length(st)){
        si = local_data$selected_time_ind
        st = st[si]
        vcol = rep('red', length(st))
      }

      staged_tbl = get_staged_epoch(block)
      if(nrow(staged_tbl)){
        st1 = staged_tbl$Time
        st = c(st, st1)
        vcol = c(vcol, rep('green', length(st1)))
      }

      plot_volt(time, signal, plot_range = plot_range, use_abs = is_symmetric, hlines = local_data$threshold, vlines = st, vcols = vcol)
    })


    observeEvent(input$console_plot_clicked, {
      e = input$console_plot_clicked
      if(!is.null(e)){
        thred = e$y
        local_data$threshold = thred
      }
    })

    observe({
      thred = local_data$threshold
      signal = local_data$signal
      local_data$selected_time = NULL
      srate = local_data$sample_rate
      direction = input$direction
      block = input$block

      if(!zero_length(thred, signal, direction, srate)){
        if(direction == 'Above'){
          sel = signal > thred
        }else{
          sel = signal < thred
        }
        max_lag = srate * input$min_trial_duration
        if(is_invalid(max_lag)){
          max_lag = 1
        }
        max_lag = max(max_lag , 1L)
        sel = which(sel)
        sel = dipsaus::deparse_svec(sel, max_lag = max_lag, concatenate = F)
        ind = as.integer(stringr::str_extract(sel, '^[0-9]+'))
        ind = ind[!is.na(ind)]
        selected_time = ind / srate

        staged_tbl = get_staged_epoch(block)
        if(nrow(staged_tbl)){
          tmp = staged_tbl$Time
          sel = vapply(selected_time, function(t){
            sum(abs(t - tmp) < 0.001) == 0
          }, FUN.VALUE = FALSE)
          selected_time = selected_time[sel]
        }
        local_data$selected_time = selected_time
      }
    })

    output$epoch_tmp <- DT::renderDataTable({
      selected_time = local_data$selected_time
      validate(need(length(selected_time), 'Click on the plot left to threshold data.'))

      tbl = data.frame(
        Index = seq_along(selected_time),
        Time = selected_time
      )

      if(nrow(tbl)){
        brush = local_data$time_brush
        displayStart = 0
        if(!is.null(brush)){
          displayStart = min(which(tbl$Time >= brush[1])) - 1
        }

        DT::formatRound(
          DT::datatable(
            tbl, rownames = F,options = list(
              pageLength = 10,
              displayStart = displayStart
            )),
          c('Time'), 2
        )
      }

    })

    output$epoch_staged <- DT::renderDataTable({
      block = input$block
      tbl = get_staged_epoch(block)
      if(nrow(tbl)){
        tbl$Order = seq_len(nrow(tbl))
        DT::formatRound(
          DT::datatable(
            tbl, rownames = F, selection = list(mode = 'single', target = 'row'),
            options = list(
              pageLength = 10
            )),
          c('Time'), 2)
      }
    })

    observeEvent(input$epoch_staged_rows_selected, {
      s = input$epoch_staged_rows_selected
      block = input$block
      tbl = isolate(get_staged_epoch(block))
      tbl = tbl[-s,]
      local_data$staged[[block]] = tbl
    })

    observe({
      local_data$selected_time_ind = input$epoch_tmp_rows_selected
    })

    output$console_plot_sub <- renderPlot({
      time = local_data$compressed_time
      signal = local_data$raw_compressed_singal
      block = input$block

      validate(need(length(signal), 'No signal detected.'))

      vcol = NULL
      st = local_data$selected_time
      if(length(st)){
        si = local_data$selected_time_ind
        st = st[si]
        vcol = rep('red', length(st))
      }

      staged_tbl = get_staged_epoch(block)
      if(nrow(staged_tbl)){
        st1 = staged_tbl$Time
        st = c(st, st1)
        vcol = c(vcol, rep('green', length(st1)))
      }

      e = input$console_plot_brush
      if(!is.null(e)){
        xmin = e$xmin
        xmax = e$xmax

        local_data$time_brush = c(xmin, xmax)

        plot_volt(time, signal, plot_range = NULL, use_abs = FALSE, hlines = local_data$threshold, vlines = st,
                  xlim = c(xmin, xmax), vcols = vcol)
      }else{
        local_data$time_brush = NULL
      }
    })

    get_staged_epoch = function(block){
      if(length(block)){
        tbl = as.data.frame(local_data$staged[[block]], stringsAsFactors = F)
      }else{
        data.frame()
      }
    }

    observeEvent(input$discard, {
      block = input$block
      local_data$staged[[block]] = data.frame()
    })

    proxy = DT::dataTableProxy('epoch_tmp')
    observeEvent(input$toggle, {
      s = input$epoch_tmp_rows_selected
      ind = seq_along(local_data$selected_time)
      ind = ind[!ind %in% s]
      DT::selectRows(proxy, as.integer(ind))
    })
    observeEvent(input$clear, {
      DT::selectRows(proxy, as.integer(0))
    })

    observeEvent(input$stage, {
      si = local_data$selected_time_ind
      block = input$block
      if(length(si) && length(block)){
        st = local_data$selected_time
        s = st[si]
        tbl = get_staged_epoch(block)
        tbl = rbind(tbl, data.frame(
          Block = block,
          Time = s
        ))
        tbl = tbl[order(tbl$Time), ]

        local_data$staged[[block]] = tbl
      }
    })

    get_epochs = function(){
      blocks = utils$get_blocks()
      tbl = data.frame()
      for(b in blocks){
        tbl = rbind(tbl, get_staged_epoch(b))
      }
      if(nrow(tbl)){
        tbl$Trial = seq_len(nrow(tbl))
        tbl$Condition = 'NoCondition'
        tbl$Duration = NA
      }
      tbl
    }

    get_save_path = function(){
      sname = input$save_name
      if(is_invalid(sname, .invalids = c('null', 'blank'))){
        sname = 'Default'
      }
      dirs = utils$get_from_subject('dirs', list(), F)
      fpath = file.path(dirs$meta_dir, sprintf('epoch_%s.csv', sname))

      try_normalizePath(fpath)
    }

    output$epoch_preview <- DT::renderDataTable({
      get_epochs()
    })
    output$save_path <- renderText({
      get_save_path()
    })
    observeEvent(input$save, {
      save_path = get_save_path()
      tbl = get_epochs()
      safe_write_csv(tbl, save_path, row.names = F)

      showNotification(p('Epoch file exported.'), type = 'message')
    })

  }

  return(list(
    body = body,
    server = server
  ))
}
