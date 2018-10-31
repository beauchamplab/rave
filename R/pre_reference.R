#' Preprocess Module - Reference (Deprecated)
#' @param module_id internally used
#' @param sidebar_width sidebar width from 1 to 12
#' @importFrom magrittr %>%
#' @export
rave_pre_ref3 <- function(module_id = 'REF_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  CHANNEL_TYPES = c(
    'Good',
    'Epilepsy',
    'Bad',
    'Exclude'
  )

  body = fluidRow(
    shinydashboard::box(
      title = 'Reference',
      width = sidebar_width,
      uiOutput(ns('ref_inputs1')),
      hr(),
      uiOutput(ns('ref_inputs2')),
      uiOutput(ns('ref_inputs3')),
      uiOutput(ns('ref_inputs4'))
    ),
    shinydashboard::tabBox(
      width = 12-sidebar_width,
      tabPanel(
        title = 'All Channels',
        plotOutput(ns('all_channel_plot'), height = '100vh')
      ),
      tabPanel(
        title = 'Channel Table',
        DT::dataTableOutput(ns('ref_table'))
      ),
      tabPanel(
        title = 'Group Panel',
        fluidRow(
          column(width = 12,
                 plotOutput(ns('group_channel_plot'), height = '60vh',
                            click = ns('group_channel_showtime'),
                            brush = brushOpts(
                              ns('group_channel_selecttime'), clip = T, direction = 'x', resetOnNew = T
                            ),
                            dblclick = dblclickOpts(
                              ns('group_channel_plusminustime'), clip = T
                            )),
                 DT::dataTableOutput(ns('group_channel_tbl')))
        )

      )
    )
  )

  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      ref_table = data.frame(),
      current_group = NULL,
      checklevel = 0,
      all_signals = NULL,
      current_subject = '',
      current_group = '',
      new_group = NULL,

      # reset signals
      # channel_plots
      reset = Sys.time()
    )

    refresh = function(){
      local_data$ref_table = data.frame()
      local_data$current_group = NULL
      local_data$checklevel = 0
      local_data$all_signals = NULL
      local_data$current_group = ''
      if(utils$has_subject()){
        local_data$ref_table = utils$get_reference_table()
        local_data$checklevel = utils$get_check_level()

        if(isolate(local_data$current_subject) != utils$get_subject_id()){
          local_data$current_group = NULL
          local_data$current_subject = utils$get_subject_id()
          local_data$current_group = ''
        }
      }

      local_data$reset = Sys.time()
    }

    observe({
      user_data$reset
      refresh()
    })

    output$ref_table <- DT::renderDataTable({
      tbl = local_data$ref_table
      sel = rep(T, 20)
      if(nrow(tbl)){
        tbl$ChlType = CHANNEL_TYPES[tbl$ChlType]
        c_group = local_data$current_group
        sel = tbl$Group %in% c_group
        if(length(c_group) && sum(sel)){
          tbl = rbind(
            tbl[sel, ],
            tbl[!sel,]
          )
        }
      }


      DT::datatable(tbl, rownames = F, options = list(
        pageLength = max(sum(sel), 20)
      ))
    })


    gct_env = new.env()
    output$group_channel_tbl <- DT::renderDataTable({
      local_data$reset
      local_data$refresh_excluded_time
      tbl = utils$get_excluded_time()
      if(nrow(tbl)){
        tbl = tbl[, c('Block', 'Start', 'End')]
        tbl$Duration = tbl$End - tbl$Start
        tbl = tbl[tbl$Block %in% input$block,]
        gct_env$tbl = tbl
        return(
          DT::datatable(tbl, selection = list(mode = 'single', target = 'row'),
                        rownames = F, editable = T,
                        options = list(
                          pageLength = 20
                        )) %>%
            DT::formatRound(c('Start', 'End', 'Duration'), 2)
        )
      }else{
        gct_env$tbl = NULL
        return(data.frame())
      }


    })


    observeEvent(input$group_channel_tbl_cell_edit, {
      tbl = gct_env$tbl
      if(is.null(tbl) || nrow(tbl) == 0){
        return()
      }
      info = input$group_channel_tbl_cell_edit
      i = info$row
      j = info$col + 1
      if(j %in% c(2,3)){
        v = as.numeric(info$value)
        if(!is.na(v)){
          tbl[i, j] <- DT::coerceValue(v, tbl[i, j])

          utils$set_excluded_time(tbl, block = input$block, local_data = local_data)

        }
      }

    })


    observeEvent(input$group_channel_tbl_rows_selected, {
      i = input$group_channel_tbl_rows_selected
      tbl = utils$get_excluded_time()
      if(nrow(tbl)){
        tbl = tbl[tbl$Block %in% input$block, ]
        start = max(tbl[i, 'Start'] - 0.1, 0)
        updateNumericInput(session, 'start', value = start)
      }
    })

    output$ref_inputs1 <- renderUI({
      validate(need(local_data$checklevel >= 2, 'Subject must be loaded and has notch filtered.'))
      blocks = utils$get_blocks(); blocks %?<-% ""
      tagList(
        selectInput(ns('block'), 'Block', choices = blocks),
        numericInput(ns('start'), 'Time Start', min = 0, value = 0, step = 1),
        sliderInput(ns('duration'), 'Duration', min = 1, max = 50, step = 0.1, value = 25),
        numericInput(ns('space'), 'Gap Between Adjacent Channels', min = 0, value = 0),
        div(
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next')
        )
      )
    })


    observeEvent(input$nxt, {
      start = input$start
      duration = input$duration
      if(!length(start) || is.na(start)){
        start = 0
      }
      if(!length(duration) || is.na(duration)){
        duration = 0
      }
      start = start + duration * 0.8
      updateNumericInput(session, 'start', value = start)
    })

    observeEvent(input$prev, {
      start = input$start
      duration = input$duration
      if(!length(start) || is.na(start)){
        start = 0
      }
      if(!length(duration) || is.na(duration)){
        duration = 0
      }
      start = max(0, start - duration * 0.8)
      updateNumericInput(session, 'start', value = start)
    })

    output$ref_inputs2 <- renderUI({
      ref_table = local_data$ref_table
      validate(need(length(ref_table), ''))

      new_group = local_data$new_group; if(!is.null(new_group)){new_group = names(new_group)}
      groups = unique(c(ref_table$Group, new_group))
      groups = groups[groups != '']
      current_group = isolate(local_data$current_group)
      if(!length(current_group) || !current_group %in% groups || current_group == ''){
        local_data$current_group = current_group = 'New Group...'
      }

      groups = c(groups, 'New Group...')
      tagList(
        selectInput(ns('group'), 'Channel Group', choices = groups, selected = current_group)
      )

    })

    output$ref_inputs3 <- renderUI({
      ref_table = local_data$ref_table
      current_group = local_data$current_group
      validate(need(length(ref_table), ''),
               need(length(current_group) && !is.blank(current_group), ''))

      local_data$current_group_sel = FALSE
      local_data$current_group_type = NULL

      if(current_group == 'New Group...'){
        tagList(
          textInput(ns('g_name'), 'Group Name', value = '', placeholder = 'Use letters or 0-9'),
          selectInput(ns('g_type'), 'Reference Type', choices = c('Common Avg/White Matter Reference', 'Bi-polar Reference')),
          textInput(ns('g_init_channels'), 'Channels in the Group', value = '', placeholder = 'e.g. 1-3,5'),
          actionButton(ns('g_create'), 'Create!')
        )
      }else{
        sel = ref_table$Group %in% current_group
        th_chls = ref_table$Channel[sel]
        chl_type = ref_table$ChlType[sel]
        g_types = ref_table$RefType[sel]

        if(sum(sel)){
          # group exists
          gtype = g_types[1]
          local_data$current_group_sel = sel
          local_data$current_group_type = gtype

          # reference matrix
          nchls = nrow(ref_table)
          ref_mat = t(sapply(1:nchls, function(i){
            ref = parse_selections(ref_table$Reference[i])
            re = rep(0, nchls)
            if(length(ref)){
              re[ref_table$Channel %in% ref] = -1
              if(ref_table$RefType[i] == 'Common Avg/White Matter Reference'){
                re = re / (-sum(re))
              }
            }
            re[i] = re[i] + 1
            re
          }))
          local_data$ref_mat = ref_mat
        }else{
          gtype = isolate(local_data$new_group[[current_group]])
        }


        switch(
          gtype,
          'Common Avg/White Matter Reference' = {
            tagList(

              selectInput(ns('g_type'), 'Reference Type (Display Only)', choices = gtype, selected = gtype),
              textInput(ns('g_channels'), 'Channels in the Group', value = deparse_selections(th_chls), placeholder = 'e.g. 1-3,5'),
              textInput(ns('epichan'), 'Epilepsey Channels', value = deparse_selections(th_chls[chl_type == 2])),
              textInput(ns('badchan'), 'Bad Channels', value = deparse_selections(th_chls[chl_type == 3])),
              textInput(ns('exclchan'), 'Excluded Channels', value = deparse_selections(th_chls[chl_type == 4])),
              textInput(ns('g_channels_disp'), 'Channels to Display', value = deparse_selections(th_chls), placeholder = 'e.g. 1-3,5'),
              actionButton(ns('g_save_car'), 'Save CAR')
            )
          },
          'Bi-polar Reference' = {
            tagList(
              selectInput(ns('g_type'), 'Reference Type (Display Only)', choices = gtype, selected = gtype),
              textInput(ns('g_channels'), 'Channels in the Group', value = deparse_selections(th_chls), placeholder = 'e.g. 1-3,5'),
              selectInput(ns('bp_e1'), 'Current Channel', choices = th_chls, selected = isolate(local_data$last_e1)),
              selectInput(ns('bp_e2'), 'Reference Channel(s)', choices = c('', th_chls), multiple = F),
              textInput(ns('g_channels_disp'), 'Channels to Display', value = deparse_selections(th_chls), placeholder = 'e.g. 1-3,5'),
              actionButton(ns('g_save_bp'), 'Save Bipolar')
            )
          },
          {
            stop(sprintf('Unknown group type - %s', gtype))
          }
        )

      }
    })
    observeEvent(input$g_save_car, {
      saved = utils$set_reference_group(
        g_name = input$group,
        g_type = 'car',
        g_channels = parse_selections(input$g_channels),
        g_epi = parse_selections(input$epichan),
        g_bad = parse_selections(input$badchan),
        g_excl = parse_selections(input$exclchan)
      )
      if(saved){
        local_data$ref_table = utils$get_reference_table()
      }
    })

    observe({
      e1 = input$bp_e1
      ref_table = local_data$ref_table

      if(length(e1) && length(ref_table) && nrow(ref_table) && as.numeric(e1) %in% ref_table$Channel){
        e1 = as.numeric(e1)
        local_data$last_e1 = e1
        ref = ref_table[ref_table$Channel == e1, 'Reference']
        e2 = parse_selections(ref)
        updateSelectInput(session, 'bp_e2', selected = e2)
      }else{
        updateSelectInput(session, 'bp_e2', selected = '')
      }
    })

    observeEvent(input$g_save_bp, {
      e2 = as.numeric(input$bp_e2)
      if(is.na(e2)){
        e2 = NULL
      }
      saved = utils$set_reference_group(
        g_name = input$group,
        g_type = 'bipolar',
        g_channels = parse_selections(input$g_channels),
        bp_e1 = input$bp_e1,
        bp_e2 = e2
      )
      if(saved){
        local_data$ref_table = utils$get_reference_table()
      }
    })

    observeEvent(input$g_create, {
      gname = input$g_name
      gtype = input$g_type
      if(gname == '' || !str_detect(gname, '^[A-Za-z0-9\\ \\-]+$')){
        showNotification(p('Group name MUST be using letters or 0-9 and not blank.'), type = 'error')
      }else if(gname %in% isolate(local_data$ref_table$Group)){
        showNotification(p('This group already exists'), type = 'error')
      }else{
        local_data$current_group = gname
        new_group = list(gtype)
        names(new_group) = gname
        local_data$new_group = new_group
        init_channels = parse_selections(input$g_init_channels)
        if(length(init_channels)){
          # init group
          utils$set_reference_group(g_name = gname, g_type = gtype, init_channels, init = T)
          local_data$ref_table = utils$get_reference_table()
        }
      }

    })

    observe({
      local_data$current_group = input$group

    })




    # parameters for channel plots

    observe({
      duration = input$duration
      srate = utils$get_srate()
      start = input$start
      channels = utils$get_channels()
      block = input$block
      local_data$plot_params = NULL
      if(utils$has_subject() && !zero_length(srate, start, channels, block, any = T, na.rm = T)){
        space = input$space;
        if(is.null(space) || is.na(space) || space == 0){
          space = 0.995
        }
        # get color
        tbl = local_data$ref_table
        if(nrow(tbl) == length(channels)){
          col = as.factor(tbl$Group)
        }else{
          col = 1
        }

        local_data$plot_params = list(
          space = space,
          col = col,
          start = start,
          duration = duration,
          block = block
        )
      }
    })

    output$all_channel_plot <- renderPlot({
      local_data$refresh_excluded_time
      plot_params = local_data$plot_params

      validate(need(!is.null(plot_params), 'Subject not loaded? or invalid time range.'))
      col = plot_params$col
      utils$plot_channels(
        block = plot_params$block,
        from = plot_params$start,
        space = plot_params$space,
        col = plot_params$col,
        duration = plot_params$duration,
        legend = T
      )
    })

    output$group_channel_plot <- renderPlot({
      local_data$refresh_excluded_time
      plot_params = local_data$plot_params

      validate(need(!is.null(plot_params) , 'Subject not loaded? or invalid time range.'),
               need(sum(local_data$current_group_sel) > 0, 'No channel in this group'))

      ref_mat = local_data$ref_mat
      gtype = local_data$current_group_type
      sel = local_data$current_group_sel
      channels = utils$get_channels()

      disp_chls = parse_selections(input$g_channels_disp)

      show_mean = gtype %in% c('Common Avg/White Matter Reference')

      # channel colors
      cols_level = factor(CHANNEL_TYPES)
      cols = cols_level[sapply(channels, utils$get_channel_type)]

      re = utils$plot_channels(
        block = plot_params$block,
        sub_channels = channels[sel],
        from = plot_params$start,
        space = plot_params$space,
        col = 'grey60',
        duration = plot_params$duration,
        subset = disp_chls,
        show_mean = show_mean,
        legend = F
      )


      utils$plot_channels(
        block = plot_params$block,
        sub_channels = channels[sel],
        from = plot_params$start,
        space = re$space,
        space_mode = 'abs',
        col = cols,
        duration = plot_params$duration,
        subset = disp_chls,
        show_mean = show_mean,
        mat = ref_mat,
        new_plot = F,
        legend = F
      )

      legend('topright', c(CHANNEL_TYPES, 'Before Reference', 'Reference', 'Excluded Time'),
             col = c(as.numeric(cols_level)+2, 'grey60', 'red', 'black'),
             lty = 1, lwd = c(rep(1, length(CHANNEL_TYPES) + 2), 5))

    })


    # observeEvent(input$start, {
    #   local_data$wavelets = NULL
    #   if(length(input$start)){
    #     ref_mat = local_data$ref_mat
    #     sel = local_data$current_group_sel
    #     from = input$start
    #     block = input$block
    #     freq = 70
    #     vtype = 'power'
    #     # block = '008'; from = 0; sel = rep(T, 41); ref_mat = isolate(ld$ref_mat)
    #
    #     # utils$get_electrodes
    #     electrodes = read.csv('~/Downloads/electrodes.csv')[10:50,4:6]
    #
    #     # utils$gen_3d_XXX
    #     wavelet_log = utils$get_from_subject('wavelet_log', list(), T)
    #     if(length(wavelet_log)){
    #       last_wavelet = wavelet_log[[length(wavelet_log)]]
    #       fi = which.min(abs(last_wavelet$frequencies - freq))
    #       tmpdir = tempdir()
    #       dirs = utils$get_from_subject('dirs', list(), F)
    #       chls = utils$get_channels()
    #       srate = last_wavelet$target_srate
    #
    #       ind = round(c(from, from + 50) * srate) + 1
    #       sub = seq(ind[1], ind[2], by = 1)
    #       sapply(chls[sel], function(i){
    #         f = file.path(dirs$preprocess_dir, sprintf('chl_%d_wavelet.h5', i))
    #         power = load_h5(f, sprintf('/wavelet/power/%s', block))
    #         phase = load_h5(f, sprintf('/wavelet/phase/%s', block))
    #         if(dim(power)[2] < ind[2]){
    #           return(NULL)
    #         }
    #         sqrt(power[fi, sub]) * exp(1i * phase[fi, sub])
    #       }) ->
    #         w
    #
    #       if(!is.matrix(w)){
    #         return(NULL)
    #       }else{
    #         print(dim(ref_mat))
    #         print(sum(sel))
    #         print(dim(w))
    #
    #         w = ref_mat[sel, sel] %*% t(w)
    #         if(vtype == 'phase'){
    #           w = Arg(w)
    #         }else if(vtype == 'power'){
    #           w = Mod(w) ^2
    #           # remove 0.99 outlier
    #           q99 = quantile(w, 0.99)
    #           w[w > q99] = q99
    #         }
    #
    #         ntp = ncol(w)
    #         nchls = nrow(w)
    #
    #
    #
    #         local_data$wavelets = list(
    #           data = w,
    #           coord = electrodes,
    #           srate = srate,
    #           vertex = which(sel),
    #           plot_data = data.frame(
    #             x = rep(electrodes$Coord_x[sel], ntp),
    #             y = rep(electrodes$Coord_y[sel], ntp),
    #             z = rep(electrodes$Coord_z[sel], ntp),
    #             frame = rep(1:ntp, each = nchls),
    #             value = as.vector(w)
    #           )
    #         )
    #       }
    #
    #     }
    #   }
    # })



    observeEvent(input$group_channel_showtime, {
      e = input$group_channel_showtime
      if(!is.null(e)){
        showNotification(p(sprintf(
          'Time @ %.2f Seconds',
          e$x
        )), type = 'message', id = 'current_time')
      }
    })

    observeEvent(input$group_channel_selecttime, {
      local_data$time_selected = input$group_channel_selecttime
    })

    observeEvent(input$group_channel_plusminustime, {
      e = input$group_channel_plusminustime
      t_sel = isolate(local_data$time_selected)
      excl_tbl = utils$get_excluded_time(input$block)
      if(!is.null(e)){
        x = e$x; y = e$y
        if(nrow(excl_tbl) &&
           sum(sel <- excl_tbl$Start < x & excl_tbl$End > x)){
          excl_tbl = excl_tbl[!sel,]
          utils$set_excluded_time(excl_tbl, block = input$block, local_data = local_data)
        }else if(!is.null(t_sel) && t_sel$xmin < x && t_sel$xmax > x){
          utils$add_excluded_time(input$block, t_sel$xmin, t_sel$xmax, local_data = local_data)
        }

        local_data$time_selected = NULL
      }
    })



    output$ref_inputs4 <- renderUI({
      validate(need(local_data$checklevel >= 3, 'Please perform wavelet transformation.'))
      actionButton(ns('do_ref'), 'Reference Channels')
    })

    observeEvent(input$do_ref, {


      showModal(
        modalDialog(
          title = 'Confirmation',
          size = 'l',
          easyClose = F,
          footer = tagList(
            modalButton('Wait a minutes?'),
            actionButton(ns('doit'), 'Do it!')
          ),
          fluidRow(
            column(12, p('Please check the following reference table:')),
            column(12, DT::dataTableOutput(ns('confirm_tbl')))
          )
        )
      )
    })

    output$confirm_tbl <- DT::renderDataTable({
      tbl = utils$get_reference_table()
      if(nrow(tbl)){
        tbl = tbl[, c('Channel', 'Group', 'Reference')]
      }
      tbl
    })

    observeEvent(input$doit, {
      utils$apply_ref()

      utils$reset()

      removeModal()
    })

  }

  return(list(
    body = body,
    server = server
  ))
}
