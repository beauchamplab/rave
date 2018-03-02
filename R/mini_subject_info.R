#' Mini UI collecting subject information
mini_subject_info <- function(){
  dirs <- get_dir()

  subject_codes = list.dirs(dirs$raw_data_dir, recursive = F, full.names = F)

  ui = miniPage(
    gadgetTitleBar("RAVE - Subject Information"),
    miniContentPanel(
      fillRow(
        flex = c(1, 2),
        fillCol(
          fillCol(textInput('project_name', 'Project Name:', value = '')),
          fillCol(selectInput('subject_code', 'Subject Code:', choices = subject_codes, selected = NULL)),
          fillCol(selectInput('blocks', 'Blocks: ', multiple = T, choices = '', selected = NULL)),
          fillCol(textInput('channels', 'Channels:', value = '')),
          fillCol(numericInput('srate', 'Sample Rate:', value = 2000, min = 100))
        ),
        fillCol(
          dataTableOutput('rawfiles')
        )
      )
    )
  )

  server <- function(input, output, session) {
    settings = reactiveValues(
      project_name = NULL,
      subject_code = NULL,
      blocks = NULL,
      channels = NULL,
      dirs = NULL,
      channel_files = NULL
    )

    observe({
      settings$project_name = input$project_name
      settings$subject_code = input$subject_code
      settings$dirs = get_dir(subject_code = input$subject_code,
                              project_name = input$project_name)
    })

    observe({
      sc = settings$dirs$pre_subject_dir
      blocks = list.files(sc)
      blocks = blocks[!str_detect(blocks, '\\.')]
      updateSelectInput(session, 'blocks', choices = blocks)
    })

    observe({
      blocks = input$blocks
      if(length(blocks) > 0 && blocks[1] != ''){
        dirs = get_dir(subject_code = input$subject_code,
                       project_name = input$project_name, block_num = blocks[1])
        settings$channel_files = list.files(dirs$block_dir)
      }
    })

    observe({
      channel_files = settings$channel_files
      nums = str_extract(channel_files, 'ch[0-9]+')
      nums = str_extract(nums, '[0-9]+')
      nums = as.numeric(nums)
      nums = nums[!is.na(nums)]
      if(length(nums) > 0){
        nums = deparse_selections(nums)
        updateTextInput(session = session, 'channels', value = nums)
      }
    })





    output$rawfiles <- renderDataTable({
      sc = settings$dirs$pre_subject_dir
      data.frame(ALL_SUBJECT_FILES = list.files(sc, recursive = T))
    })

    observeEvent(input$done, {
      re = list()
      re$project_name = input$project_name
      re$subject_code = input$subject_code
      re$blocks = input$blocks
      re$channels = parse_selections(input$channels)
      re$srate = input$srate
      stopApp(re)
    })

    observeEvent(input$cancel,{
      stop(':)))))')
      stopApp()
    })
  }

  runGadget(ui, server)

}
