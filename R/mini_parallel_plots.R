mini_parallel_plots <- function(signals, sample_rate, channel_names, project_name, subject_code){
  result = new.env()
  result$channels = load_meta(meta_type = 'electrodes', project_name = project_name, subject_code = subject_code)
  # env = load_preprocess_module()

  if(is.null(result$channels)){
    result$channels = data.frame(
      Electrode = channel_names,
      Epichan = F,
      Badchan = F
    )
    result$channels$Epichan = result$channels$Epichan == TRUE
    result$channels$Badchan = result$channels$Badchan == TRUE
  }

  ui <- miniPage(
    gadgetTitleBar("RAVE - Signal Inspection"),
    miniContentPanel(
      # Define layout, inputs, outputs
      fillRow(
        flex = c(1, 3),
        fillCol(
          flex = c(1, 2),
          fillCol(
            selectInput('block_num', 'Block', selected = names(signals)[1],
                        choices = names(signals)),
            selectInput('bad', 'Bad Channels', multiple = T,
                        choices = result$channels$Electrode,
                        selected = result$channels$Electrode[result$channels$Badchan]),
            selectInput('epi', 'Epi Channels', multiple = T,
                        choices = result$channels$Electrode,
                        selected = result$channels$Electrode[result$channels$Epichan])
          ),
          plotOutput('block_plot', height = '100%', width = '100%', brush = "plot_brush")
        ),
        fillCol(
          plotOutput('subplot', height = '100%', width = '100%')

        )
      )
    )
  )

  server <- function(input, output, session){
    output$block_plot <- renderPlot({
      ss = signals[[input$block_num]]

      plot_signals(
        ss, sample_rate = sample_rate, space = 0.99999, plot = 'base',
        channel_names = channel_names,
        col = 1 + (result$channels$Badchan) + 2 * (result$channels$Epichan)
      )
    })

    output$subplot <- renderPlot({
      e = input$plot_brush
      validate(need(!is.null(e), 'Drag in the left figure to select time rage.'))

      if(!is.null(e)){
        ts = ceiling(e$xmin * sample_rate)
        te = floor(e$xmax * sample_rate)
        ss = signals[[input$block_num]][, ts:te]
        plot_signals(
          ss, sample_rate = sample_rate, space = 0.99999, plot = 'base',
          channel_names = channel_names,
          col = (1 + (result$channels$Electrode %in% input$bad) + 2 * (result$channels$Electrode %in% input$epi))
        )
      }
    })

    observe({
      epi = input$epi
      bad = input$bad
      bads = result$channels$Electrode %in% bad
      epis = result$channels$Electrode %in% epi

      result$channels$Epichan = epis
      result$channels$Badchan = bads
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      # save meta file
      save_meta(result$channels, meta = 'electrodes', project_name, subject_code)
      stopApp(result$channels)
    })
    observeEvent(input$cancel,{
      stopApp()
    })
  }

  runGadget(ui, server)
}
