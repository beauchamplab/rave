#' @export
rave_pre_ref3 <- function(module_id = 'REF_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    shinydashboard::box(
      title = 'Reference',
      width = sidebar_width,
      uiOutput(ns('ref_inputs1')),
      hr(),
      uiOutput(ns('ref_inputs2')),
      uiOutput(ns('ref_inputs3'))
    ),
    shinydashboard::tabBox(
      width = 6,
      tabPanel(
        title = 'All Channels',
        plotOutput(ns('all_channel_plot'), height = '75vh')
      )
    ),
    shinydashboard::box(
      width = 3,
      title = 'Channel Table',
      DT::dataTableOutput(ns('ref_table'))
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
      new_group = NULL
    )

    observe({
      user_data$reset
      cs = isolate(local_data$current_subject)
      if(utils$has_subject()){
        local_data$ref_table = utils$get_reference_table()
        local_data$checklevel = utils$get_from_subject('checklevel', 0, customized = T)
        if(cs != utils$get_subject_id()){
          local_data$current_group = NULL
          local_data$current_subject = utils$get_subject_id()
          local_data$current_group = ''

        }
      }else{
        local_data$ref_table = data.frame()
        local_data$current_group = NULL
        local_data$checklevel = 0
        local_data$all_signals = NULL
        local_data$current_group = ''
      }

    })

    output$ref_table <- DT::renderDataTable({
      DT::datatable(local_data$ref_table, rownames = F)
    })

    output$ref_inputs1 <- renderUI({
      validate(need(local_data$checklevel > 1, 'Subject must be loaded and has notch filtered.'))
      blocks = utils$get_blocks(); blocks %?<-% ""
      tagList(
        selectInput(ns('block'), 'Block', choices = blocks),
        numericInput(ns('start'), 'Time Start', min = 0, value = 0, step = 1),
        sliderInput(ns('duration'), 'Duration', min = 1, max = 50, step = 0.1, value = 20),
        numericInput(ns('space'), 'Gap Between Adjacent Channels', min = 0, value = 0),
        div(
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next')
        )
      )
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


      if(current_group == 'New Group...'){
        tagList(
          textInput(ns('g_name'), 'Group Name', value = '', placeholder = 'Use letters or 0-9'),
          selectInput(ns('g_type'), 'Reference Type', choices = c('Common Average Reference', 'Bi-polar Reference')),
          actionButton(ns('g_create'), 'Create!')
        )
      }else{
        sel = ref_table$Group %in% current_group
        th_chls = ref_table$Channel[sel]
        chl_type = ref_table$ChlType[sel]

        if(sum(sel)){
          # group exists
          gtype = ref_table$RefType[1]
        }else{
          gtype = isolate(local_data$new_group[[current_group]])
        }

        switch(
          gtype,
          'Common Average Reference' = {
            tagList(
              selectInput(ns('g_type'), 'Reference Type (Display Only)', choices = gtype, selected = gtype),
              textInput(ns('g_channels'), 'Channels in the Group', value = rave:::deparse_selections(th_chls), placeholder = 'e.g. 1-3,5'),
              textInput(ns('epichan'), 'Epilepsey Channels', value = rave:::deparse_selections(th_chls[chl_type == 2])),
              textInput(ns('badchan'), 'Bad Channels', value = rave:::deparse_selections(th_chls[chl_type == 3])),
              textInput(ns('exclchan'), 'Excluded Channels', value = rave:::deparse_selections(th_chls[chl_type == 4])),
              actionButton(ns('g_save_car'), 'Save')
            )
          }
        )

      }
    })
    observeEvent(input$g_save_car, {
      saved = utils$set_reference_group(
        g_name = input$group,
        g_type = 'car',
        g_channels = rave:::parse_selections(input$g_channels),
        g_epi = rave:::parse_selections(input$epichan),
        g_bad = rave:::parse_selections(input$badchan),
        g_excl = rave:::parse_selections(input$exclchan)
      )
      if(saved){
        local_data$ref_table = utils$get_reference_table()
      }
    })

    observeEvent(input$g_create, {
      gname = input$g_name
      if(gname == '' || !str_detect(gname, '^[A-Za-z0-9\\ \\-]+$')){
        showNotification(p('Group name MUST be using letters or 0-9 and not blank.'), type = 'error')
      }else if(gname %in% local_data$ref_table$Group){
        showNotification(p('This group already exists'), type = 'error')
      }else{
        local_data$current_group = gname
        new_group = list(input$g_type)
        names(new_group) = gname
        local_data$new_group = new_group
      }

    })

    observe({
      local_data$current_group = input$group
    })


    observe({
      block = input$block
      start = input$start
      user_data$reset

      local_data$all_signals = NULL
      if(!zero_length(start, block, any = T, na.rm = T)){
        local_data$all_signals = utils$load_compressed_notch_signal(block = block, from = start, to = start + 50, compress_level = 1)
      }
    })

    output$all_channel_plot <- renderPlot({
      validate(need(!is.null(local_data$all_signals) , 'Subject not loaded? or invalid time range.'))
      duration = input$duration
      srate = utils$get_srate()
      start = input$start
      channels = utils$get_channels()
      space = input$space; space %?<-% 0;
      if(space <= 1){
        space_mode = 'quantile'
      }else{
        space_mode = 'abs'
      }

      if(!zero_length(duration, start)){
        plot_signals(
          signals = local_data$all_signals,
          sample_rate = srate,
          space = space,
          space_mode = space_mode,
          col = 1,
          start_time = 0, time_shift = start,
          duration = duration, compress = TRUE,
          channel_names = channels, ylab = 'Channel',
          plot = 'base'
        )
      }
    })

  }

  return(list(
    body = body,
    server = server
  ))
}
