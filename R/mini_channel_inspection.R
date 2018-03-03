#' Function to activate preprocessing-Gadgets (notch inspection)
#' @import miniUI
#' @import shiny
mini_channel_inspection <- function(process_name, project_name, subject_code, blocks, channels, srate){
  # env = load_preprocess_module()
  result = new.env()

  result$channels = load_meta(meta_type = 'electrodes', project_name = project_name, subject_code = subject_code)

  if(is.null(result$channels)){
    result$channels = data.frame(
      Channel = channels,
      Epichan = F,
      Badchan = F
    )
    result$channels$Epichan = result$channels$Epichan == TRUE
    result$channels$Badchan = result$channels$Badchan == TRUE
  }else{
    result$channels = result$channels[, 1:3]
    names(result$channels) <- c('Channel', 'Epichan', 'Badchan')
  }


  # directories
  dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = 'pre_visual_dir')
  data_dir = dirs$data_dir
  # assume save_dir exists
  subject_dir = dirs$preprocess_dir
  vis_dir = dirs$pre_visual_dir

  result$cached_imgs = list.files(vis_dir)


  title = "RAVE - Notch Filter Inspection"
  if(process_name == 'CAR'){
    title = "RAVE - CAR Inspection"
    result$channels = result$channels[!(
      result$channels$Epichan | result$channels$Badchan
    ),]

    checkboxInput = function(label, ...){
      HTML('<span><strong>', label, '</strong> is disabled in this mode.</span>')
    }
  }


  ui <- miniPage(
    gadgetTitleBar(title),
    miniContentPanel(
      # Define layout, inputs, outputs
      fillRow(
        flex = c(1, 3),
        fillCol(
          flex = c(3, 1),
          # inputs
          fillCol(
            fillRow(flex = c(1,1),
                    actionButton('prev', 'Previous'),
                    actionButton('nxt', 'Next')),
            fillRow(
              selectInput('block_num', 'Select a block',
                          selected = blocks[1], choices = blocks)
            ),
            fillRow(
              selectInput('channel', 'Select a channel',
                          selected = result$channels$Channel[1], choices = result$channels$Channel)
            ),
            fillRow(
              checkboxInput('bad', label = 'Mark as bad channel', value =
                              result$channels$Badchan[result$channels$Channel[1]])
            ),
            fillRow(
              checkboxInput('epi', label = 'Mark as epi channel', value =
                              result$channels$Epichan[result$channels$Channel[2]])
            )

          ),

          # channel info
          fillCol(
            h5(
              htmlOutput('channel_info')
            )
          )

        ),
        plotOutput('plot', height = "100%")
      )
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    if(process_name == 'notch'){
      observe({
        chl = result$channels
        current_c = isolate(as.integer(input$channel))
        chl[chl$Channel == current_c, 'Badchan'] = input$bad
        chl[chl$Channel == current_c, 'Epichan'] = input$epi
        result$channels = chl
      })
    }


    output$plot <- renderImage({
      chl = as.integer(input$channel)
      block_num = input$block_num
      # script to gen plots
      cache = sprintf('%s_%s_ch_%d.png', process_name, block_num, chl)
      if(!cache %in% result$cached_imgs){
        png(
          filename = file.path(vis_dir, cache),
          width = as.numeric(rave_options('image_width')),
          height = as.numeric(rave_options('image_height'))
        )
        pre_inspect(
          process_name = process_name,
          project_name = project_name,
          subject_code = subject_code,
          block_num = block_num,
          srate = srate,
          chls = chl,
          details = T,
          boundary = -1
        )
        dev.off()
        result$cached_imgs = c(result$cached_imgs, cache)
      }

      list(
        src = file.path(vis_dir, cache),
        width = session$clientData$output_plot_width,
        height = session$clientData$output_plot_height,
        alt = "If you see this message, that means this image is not cached properly."
      )
    }, deleteFile = F)

    observeEvent(input$nxt, {
      current_chl = as.integer(input$channel)
      available_channels = result$channels$Channel
      available_channels = available_channels[available_channels > current_chl]
      if(length(available_channels) > 0){
        chl = min(available_channels)
        updateSelectInput(session, 'channel', selected = chl)
      }
    })

    observeEvent(input$prev, {
      current_chl = as.integer(input$channel)
      available_channels = result$channels$Channel
      available_channels = available_channels[available_channels < current_chl]
      if(length(available_channels) > 0){
        chl = max(available_channels)
        updateSelectInput(session, 'channel', selected = chl)
      }
    })

    observe({
      # get info of current channel
      current_c = as.integer(input$channel)
      logger(current_c)
      chl = result$channels
      bad = chl[chl$Channel == current_c, 'Badchan']
      epi = chl[chl$Channel == current_c, 'Epichan']

      updateCheckboxInput(session, 'bad', value = bad)
      updateCheckboxInput(session, 'epi', value = epi)
    })

    output$channel_info <- renderText({
      if(is.null(input$bad)){
        bad = result$channels$Badchan[result$channels$Channel == input$channel]
      }else{
        bad = input$bad
      }
      if(is.null(input$epi)){
        epi = result$channels$Epichan[result$channels$Channel == input$channel]
      }else{
        epi = input$epi
      }

      s = c('<strong>Project:</strong> %s',
            '<strong>Subject:</strong> %s',
            '<strong>Current block:</strong> %s',
            '<strong>Current channel:</strong> %s',
            '<strong>Bad channel:</strong> %s',
            '<strong>Epi channel:</strong> %s')

      sprintf(paste0(s, collapse = '<br />'), project_name, subject_code,
              input$block_num, input$channel, bad, epi)
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      # save meta file
      if(process_name == 'notch'){
        save_meta(result$channels, meta = 'electrodes', project_name, subject_code)
      }else{
        logger("CAR visualization don't change the electrode flag", level = 'WARNING')
      }
      stopApp(result$channels)
    })
    observeEvent(input$cancel,{
      if(process_name == 'CAR'){
        stop('User canceled the preprocess. Wavelet will not start.')
      }
      stopApp()
    })
  }

  runGadget(ui, server)

}
