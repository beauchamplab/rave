# module to load data

#' @import stringr
#' @import shiny
#' @import magrittr
#' @export
init_app <- function(modules = NULL, launch.browser = T, ...){
  tryCatch({
    rave_prepare()
  }, error = function(e){})

  test.mode = list(...)[['test.mode']]
  if(is.null(test.mode)) test.mode = rave_options('test_mode')
  if(length(modules) == 0){
    modules = load_modules()
  }

  data_selector = rave:::shiny_data_selector('DATA_SELECTOR')
  ui = rave::dashboardPage(
    title = 'R Analysis and Visualization of ECoG Data',
    header = dashboardHeader(
      title = 'RAVE',
      data_selector$header()
    ),
    sidebar = shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = 'sidebar',
        .list = lapply(modules, function(m){
          shinydashboard::menuItem(
            text = m$label_name,
            tabName = str_to_upper(m$module_id)
          )
        })
      )
    ),
    control = dashboardControl(
      data_selector$control()
    ),
    body = shinydashboard::dashboardBody(
      do.call(shinydashboard::tabItems, args = lapply(modules, function(m){
        shinydashboard::tabItem(
          tabName = str_to_upper(m$module_id),
          uiOutput(str_c(m$module_id, '_UI'))
        )
      }))
    )
  )

  server = function(input, output, session){
    #################################################################

    # Global variable, timer etc.
    async_timer = reactiveTimer(500)
    input_timer = reactiveTimer(rave_options('delay_input') / 2)
    global_reactives = reactiveValues(
      check_results = NULL,
      check_inputs = NULL,
      execute_module = '',
      has_data = FALSE
    )
    observeEvent(async_timer(), {
      global_reactives$check_results = Sys.time()
    })
    observeEvent(input_timer(), {
      global_reactives$check_inputs = Sys.time()
    })
    ##################################################################
    # Module to load data
    callModule(module = data_selector$server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)


    ##################################################################
    # load modules
    shinirized_modules = lapply(modules, rave:::shinirize, test.mode = test.mode)

    observe({
      if(global_reactives$has_data){
        global_reactives$execute_module = input$sidebar
      }
    })


    lapply(shinirized_modules, function(m){
      callModule(m$server, id = m$id, session = session, global_reactives = global_reactives)
    })

    lapply(shinirized_modules, function(m){
      output[[str_c(m$id, '_UI')]] <- renderUI(m$ui())
    })

    #################################################################
    # on session ended, clean memory
    session_id = add_to_session(session)

    if(!test.mode){
      session$onSessionEnded(function() {
        logger('Clean up environment.')
        lapply(modules, function(x){
          x$clean(session_id = session_id)
        })
        logger('Clean up data repository.')
        data_repository[[session_id]]$.clean()
      })
    }

  }
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}



# init_app()
