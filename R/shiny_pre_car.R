rave_pre_car <- function(module_id = 'CAR_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  gp = global_panel(ns)
  jscode = paste(
    "shinyjs.rave_car_sidebar = function(){",
    sprintf("$('#%s [%s]').click();", ns('sidebar'), 'data-value="Post-CAR"'),
    "};",
    "shinyjs.rave_car_main = function(){",
    sprintf("$('#%s [%s]').click();", ns('main'), 'data-value="Details (Post CAR)"'),
    "};"
  )
  # sprintf("shinyjs.customized_click = function(selector){$(selector).click()}", ns('sidebar'), 'data-value="Post-CAR"')

  body = fluidRow(
    shinydashboard::tabBox(
      id = ns('sidebar'),
      width = sidebar_width,
      shiny::tabPanel(
        title = 'CAR',
        fluidRow(
          column(
            12,
            uiOutput(ns('load_ui')),
            uiOutput(ns('inner_ui')),
            uiOutput(ns('inner_ui2')),
            uiOutput(ns('inner_ui3'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Post-CAR',
        fluidRow(
          column(
            12,
            uiOutput(ns('inner_ui4'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Global',
        fluidRow(
          gp$ui()
        )
      )
    ),
    shinydashboard::tabBox(
      id = ns('main'),
      width = 12 - sidebar_width,
      shiny::tabPanel(
        title = 'All Channels',
        fluidRow(
          column(
            12,
            plotOutput(ns('parallel_plot'), height = '95vh', click = ns('pp_clicked'))
          )
        )
      ),
      shiny::tabPanel(
        title = 'Details (Post CAR)',
        fluidRow(
          column(
            12,
            plotOutput(ns('post_car_plot'), height = '80vh')
          )
        )
      )
    ),
    shinyjs::extendShinyjs(text = jscode)
  )

  server = function(input, output, session, user_data){
    local_data = reactiveValues(
      save = Sys.Date(),
      loaded = FALSE,
      car_5 = list(),
      car_20 = list(),
      s_5 = list(),
      s_20 = list(),
      space = 0,
      click_msg = 'Click on the plot.',
      start_time = 0,
      car_plan = NULL,
      an = NULL,
      plan = NULL,
      exc = NULL,
      bad = NULL,
      epi = NULL,
      refresh_post = NULL
    )
    gp$reactive_func(input = input, user_data = user_data, local_data = local_data, session = session)

    output$load_ui <- renderUI({
      if(!local_data$loaded){
        tagList(
          actionButton(ns('car_load'), 'Load Data')
        )
      }else{
        div()
      }
    })


    observeEvent(input$main, {
      if(input$main == "Details (Post CAR)" && input$sidebar != "Post-CAR"){
        shinyjs::js$rave_car_sidebar()
      }
    })

    observeEvent(input$sidebar, {
      if(input$main != "Details (Post CAR)" && input$sidebar == "Post-CAR"){
        shinyjs::js$rave_car_main()
      }
    })



    output$inner_ui <- renderUI({
      validate(need(local_data$loaded, 'Please load data first.'))
      max_time = dim(local_data$s_20[[1]])[2] / isolate(user_data$srate) * 20
      tagList(
        selectInput(ns('current_block'), 'Block:', choices = names(local_data$s_20)),
        numericInput(ns('start_time'), 'Start Time:', value = 0, min = 0, max = max_time, step = 1L),
        numericInput(ns('duration'), 'Duration:', value = 25, min = 1, max = 40),
        numericInput(ns('space'), 'Voltage Scale', value = 0, step = 1L, min = 0),
        div(
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next')
        )
      )
    })
    observeEvent(input$nxt, {
      max_time = dim(local_data$s_20[[1]])[2] / isolate(user_data$srate) * 20
      c_t = min(max_time - input$duration, input$start_time + 0.9 * input$duration)
      updateNumericInput(session, 'start_time', value = c_t)
    })
    observeEvent(input$prev, {
      c_t = max(0, input$start_time - 0.9 * input$duration)
      updateNumericInput(session, 'start_time', value = c_t)
    })
    output$inner_ui2 <- renderUI({
      validate(need(local_data$loaded, ''))
      tagList(
        tags$small(local_data$click_msg),
        hr(),
        textInput(ns('car_epi'), 'Epilepsy Channels', value = rave:::deparse_selections(user_data$epichan)),
        textInput(ns('car_bad'), 'Bad Channels', value = rave:::deparse_selections(user_data$badchan)),
        textInput(ns('car_exc'), 'Excluded Channels', value = rave:::deparse_selections(user_data$exclchan)),
        div(
          actionButton(ns('update'), 'Update Plot'),
          actionButton(ns('calc'), 'Calculate Average')
        )
      )
    })
    output$inner_ui3 <- renderUI({
      all_choices = local_data$car_plan
      validate(need(length(all_choices) > 0, 'Please Calculate Average Signals First.'))
      tagList(
        hr(),
        selectInput(ns('car_plan_choice'), 'Select an average:', choices = all_choices, selected = local_data$new_plan),
        actionButton(ns('car_execute'), 'Apply CAR')
      )
    })
    observeEvent(user_data$reset, {
      local_data$loaded = FALSE
      local_data$s_5 = list()
      local_data$s_20 = list()
      local_data$click_msg = 'Click on the plot.'
      local_data$start_time = 0
      gc()
    })
    observe({
      if(!is.null(user_data$subject)){
        car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
        gnames = names(car_plans)
        if(length(gnames)){
          local_data$car_plan = gnames
          local_data$new_plan = gnames[1]
          return(NULL)
        }

      }
      local_data$car_plan = NULL
      local_data$new_plan = NULL
    })
    observe({
      local_data$refresh_post
      if(!is.null(user_data$subject)){
        plan = user_data$subject$logger$get_or_save('CAR_plan')
        if(length(plan)){
          plan = plan[1]
          local_data$plan = plan
          parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
          parsed = as.vector(parsed)
          name = parsed[1]
          exc = rave:::parse_selections(parsed[2])
          bad = rave:::parse_selections(parsed[3])
          epi = rave:::parse_selections(parsed[4])
          car = user_data$channels
          car = car[!car %in% c(bad, epi)]
          local_data$exc = exc
          local_data$bad = bad
          local_data$epi = epi
          local_data$car = car
          user_data$valid_channels = car
          return(NULL)
        }
      }
      local_data$plan = NULL
      local_data$exc = NULL
      local_data$bad = NULL
      local_data$epi = NULL
      local_data$car = NULL
      user_data$valid_channels = NULL
    })
    observeEvent(input$car_load, {
      nchls = length(user_data$channels)
      nblocks = length(user_data$blocks)
      progress = shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = 'Loading in progress',
                   detail = 'This may take a while...')
      # load signals
      if(!is.null(user_data$subject)){
        pre_dir = user_data$subject$dirs$preprocess_dir
        cache_file = file.path(pre_dir, 'compressed_signals.h5')
        if(file.exists(cache_file)){
          for(b in user_data$blocks){
            progress$inc(1/nblocks, detail = sprintf('Block - %s', b))
            local_data$s_5[[b]] = h5read(cache_file, sprintf('/notch/compress5/%s', b))
            local_data$s_20[[b]] = h5read(cache_file, sprintf('/notch/compress20/%s', b))
          }
        }else{
          showNotification(p('You are running CAR for the first time. Caching data will take about 3 minutes.'), type = 'message')
          for(ii in 1:nchls){
            chl = user_data$channels[ii]
            progress$inc(1/nchls, detail = sprintf('Channel - %d', chl))
            for(b in user_data$blocks){
              fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
              # original signal
              s = h5read(fname, name = sprintf('/notch/compress5/%s', b))
              if(is.null(local_data$s_5[[b]])){
                local_data$s_5[[b]] = matrix(rep(NA, nchls * length(s)), nrow = nchls)
              }
              local_data$s_5[[b]][ii, ] = s
            }
            for(b in user_data$blocks){
              fname = file.path(pre_dir, sprintf('chl_%d.h5', chl))
              # original signal
              s = h5read(fname, name = sprintf('/notch/compress20/%s', b))
              if(is.null(local_data$s_20[[b]])){
                local_data$s_20[[b]] = matrix(rep(NA, nchls * length(s)), nrow = nchls)
              }
              local_data$s_20[[b]][ii, ] = s
            }
          }
          # save
          for(b in user_data$blocks){
            rave:::save_h5(
              x = local_data$s_5[[b]],
              file = cache_file,
              name = sprintf('/notch/compress5/%s', b),
              chunk = c(1, 1024)
            )
            rave:::save_h5(
              x = local_data$s_20[[b]],
              file = cache_file,
              name = sprintf('/notch/compress20/%s', b),
              chunk = c(1, 1024)
            )
          }
        }


        local_data$loaded = TRUE

      }

    })

    update_pressed <- observeEvent(input$update, {
      save = FALSE
      epi = rave:::parse_selections(input$car_epi)
      if(!setequal(user_data$epichan, epi)){
        user_data$epichan = epi
        save = T
      }
      bad = rave:::parse_selections(input$car_bad)
      if(!setequal(user_data$badchan, bad)){
        user_data$badchan = bad
        save = T
      }
      exc = rave:::parse_selections(input$car_exc)
      if(!setequal(user_data$exclchan, exc)){
        user_data$exclchan = exc
        save = T
      }
      if(save){
        user_data$force_save = Sys.time()
      }
    })
    output$parallel_plot <- renderPlot({
      validate(need(length(local_data$s_20) > 0, ''))
      start = input$start_time
      local_data$start_time = start
      duration = input$duration
      block = input$current_block
      plan = input$car_plan_choice

      if(!is.null(plan)){
        car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
        if(plan %in% names(car_plans)){
          if(duration < 5){
            m = car_plans[[plan]][['m_5']][[block]]
          }else{
            m = car_plans[[plan]][['m_20']][[block]]
          }
        } else{
          plan = NULL
        }
      }
      if(length(duration) == 0){
        duration = 25
      }

      if(duration < 5){
        srate = isolate(user_data$srate) / 5
        ind = seq(ceiling(start * srate), floor(srate * (start + duration)))
        signals = local_data$s_5[[block]][, ind]

      }else{
        srate = isolate(user_data$srate) / 20
        ind = seq(ceiling(start * srate), floor(srate * (start + duration)))
        signals = local_data$s_20[[block]][, ind]
      }

      col = rep(1, nrow(signals))
      col[user_data$badchan] = 2
      col[user_data$epichan] = 3
      col[user_data$exclchan] = 4
      channel_names = paste(1:nrow(signals))

      if(!is.null(plan)){
        m = m[ind]
        zeros = rep(0, dim(signals)[2])
        signals = rbind(m, zeros, signals)
        col = c(5, 0, col)
        channel_names = c('Avg', '', channel_names)
      }
      if(input$space == 0){
        space = rave:::plot_signals(
          signals, srate, start_time = start, space = 0.99999, col = col, channel_names = channel_names
        )
        local_data$space = space
      }else{
        rave:::plot_signals(
          signals, srate, start_time = start, space = input$space, col = col, channel_names = channel_names
        )
        local_data$space = input$space
      }
    })
    observeEvent(input$pp_clicked, {
      if(local_data$space <= 0){
        local_data$click_msg = 'Click on the plot.'
      }else{
        e = input$pp_clicked
        ind = ceiling(e$y / local_data$space - 0.5)
        if(!is.null(input$car_plan_choice) && length(local_data$car_plan) > 0){
          ind = ind - 2
          if(ind <= 0){
            ind = 'Average Signal'
          }
        }
        time = e$x
        local_data$click_msg = HTML(sprintf(
          'You have clicked <br />Channel: %s <br />Time: %.2f sec', ind, time))
      }
    })

    observeEvent(input$calc, {
      # copied from "observeEvent(input$update, {..."
      save = FALSE
      epi = rave:::parse_selections(input$car_epi)
      if(!setequal(user_data$epichan, epi)){
        user_data$epichan = epi
        save = T
      }
      bad = rave:::parse_selections(input$car_bad)
      if(!setequal(user_data$badchan, bad)){
        user_data$badchan = bad
        save = T
      }
      exc = rave:::parse_selections(input$car_exc)
      if(!setequal(user_data$exclchan, exc)){
        user_data$exclchan = exc
        save = T
      }
      if(save){
        user_data$force_save = Sys.time()
      }
      # calculate
      pre_dir = user_data$subject$dirs$preprocess_dir
      name = sprintf('Exc[%s]Bad[%s]Epi[%s]',
                     rave:::deparse_selections(user_data$exclchan),
                     rave:::deparse_selections(user_data$badchan),
                     rave:::deparse_selections(user_data$epichan))
      invalidchans = c(user_data$badchan, user_data$exclchan, user_data$epichan)
      goodchan = user_data$channels[!user_data$channels %in% invalidchans]
      m_5 = list()
      m_20 = list()
      for(b in names(local_data$s_5)){
        m_5[[b]] = rbind(colMeans(local_data$s_5[[b]][goodchan, , drop = FALSE]))
        m_20[[b]] = rbind(colMeans(local_data$s_20[[b]][goodchan, , drop = FALSE]))
      }
      # save cache
      car_plans = user_data$subject$cacher$get_or_save('car_plans', list())
      car_plans[[name]] = list(
        m_5 = m_5,
        m_20 = m_20
      )
      user_data$subject$cacher$save(
        car_plans = car_plans
      )

      local_data$new_plan = name
      local_data$car_plan = unique(c(isolate(local_data$car_plan), name))
      # updateSelectInput(session,)
      showNotification(p('Ready.'), type = 'message')
    })

    observeEvent(input$ok, {
      plan = input$car_plan_choice
      parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
      parsed = as.vector(parsed)
      name = parsed[1]
      exc = rave:::parse_selections(parsed[2])
      bad = rave:::parse_selections(parsed[3])
      epi = rave:::parse_selections(parsed[4])

      validchans = user_data$channels
      validchans = validchans[!validchans %in% c(exc, bad, epi)]
      progress = Progress$new()
      on.exit(progress$close())
      progress$set(message = 'Common Average Reference',
                   detail = 'This may take a while...')
      rave:::pre_car2(
        project_name = user_data$project_name,
        subject_code = user_data$subject_code,
        blocks = user_data$blocks,
        validchans = validchans,
        exclchan = exc, progress = progress
      )
      showNotification(p('CAR Completed.'), type = 'message')
      shiny::removeModal()
      user_data$subject$logger$save(CAR_plan = plan)
      local_data$refresh_post = Sys.time()
    })

    observeEvent(input$car_execute, {
      # apply CAR
      error = FALSE
      modal = NULL
      # step 1: parse selection
      plan = input$car_plan_choice
      if(is.null(plan)){
        error = TRUE
        modal = shiny::modalDialog(
          title = 'Error',
          p("You must choose a strategy to apply CAR"),
          easyClose = T
        )
      }else{
        parsed = str_match(plan, '^Exc\\[([0-9,\\-]*)\\]Bad\\[([0-9,\\-]*)\\]Epi\\[([0-9,\\-]*)\\]$')
        if(sum(is.na(parsed)) > 0){
          error = TRUE
          modal = shiny::modalDialog(
            title = 'Fatal',
            p("Invalid Strategy! This shouldn't happen. Please remove the following file manually and try again. ",
              " If this error still occurs, contact author."),
            tags$blockquote(
              tools::file_path_as_absolute(file.path(user_data$subject$dirs$preprocess_dir,
                                                     'rave.RData'))),
            easyClose = T
          )
        }else{
          c_plan = user_data$subject$logger$get_or_save('CAR_plan')
          if(length(c_plan) > 0 && plan == c_plan){
            modal = shiny::modalDialog(
              title = 'Message',
              p("Your current CAR strategy is:"),
              tags$blockquote(plan),
              p("There's no need to re-run CAR"),
              easyClose = T
            )
          }else{
            parsed = as.vector(parsed)
            name = parsed[1]
            exc = rave:::parse_selections(parsed[2])
            bad = rave:::parse_selections(parsed[3])
            epi = rave:::parse_selections(parsed[4])
            modal = shiny::modalDialog(
              title = 'Confirmation',
              p('Please confirm the following information: '),
              tags$ul(
                tags$li(strong('Excluded Channels: ', parsed[2])),
                tags$li(strong('Bad Channels: ', parsed[3])),
                tags$li(strong('Epilepsy Channels: ', parsed[4]))
              ),
              p(tags$small('* Note: All channels not listed above will be included into CAR calculation. ',
                           'Excluded channels will be excluded from CAR but will be counted as "valid" channels.')),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("ok"), "Run CAR")
              )
            )
          }

        }
      }
      if(!is.null(modal)){
        shiny::showModal(modal)
      }
    })

    output$inner_ui4 <- renderUI({
      validate(need(!is.null(local_data$plan), "Please apply CAR first"))
      tagList(
        h4('Current Strategy'),
        tags$small(local_data$plan),
        selectInput(ns('current_block2'), 'Block:', choices = user_data$blocks),
        selectInput(ns('current_channel2'), 'Channel:', choices = local_data$car),
        div(
          actionButton(ns('prev2'), 'Previous'),
          actionButton(ns('nxt2'), 'Next')
        ),
        hr(),
        actionButton(ns('export'), 'Export Plots')
      )
    })
    observeEvent(input$prev2, {
      c_c = as.integer(input$current_channel2)
      a_c = as.integer(local_data$car)
      r_c = a_c[a_c < c_c]
      if(length(r_c)){
        updateSelectInput(session, 'current_channel2', selected = max(r_c))
      }
    })
    observeEvent(input$nxt2, {
      c_c = as.integer(input$current_channel2)
      a_c = as.integer(local_data$car)
      r_c = a_c[a_c > c_c]
      if(length(r_c)){
        updateSelectInput(session, 'current_channel2', selected = min(r_c))
      }
    })

    output$post_car_plot <- renderPlot({
      c_b = input$current_block2
      c_c = as.integer(input$current_channel2)

      validate(
        need(!is.null(local_data$plan), "Please apply CAR first"),
        need(length(c_b) == 1 && length(c_c) == 1, '')
      )

      if(c_c %in% local_data$exc){
        col = 'blue'
      }else{
        col = 'black'
      }
      rave:::pre_plot_car(
        user_data$project_name, user_data$subject_code, c_b, c_c, cex = 2, col = col
      )
    })

    observeEvent(input$export, {
      showNotification(p('Plots are being saved to ', user_data$subject$dirs$preprocess_dir), type = 'message')
      logger('Plots are being saved to ', user_data$subject$dirs$preprocess_dir, level = 'INFO')

      rave:::save_car_plots(
        project_name = user_data$project_name,
        subject_code = user_data$subject_code,
        blocks = user_data$blocks,
        chls = local_data$car,
        srate = user_data$srate,
        cex = 2,
        plan = local_data$plan
      )
    })

  }

  return(list(body = body, server = server))
}
