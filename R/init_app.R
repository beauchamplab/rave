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
      data_selector$header(),
      .list = tagList(
        tags$li(
          class = 'dropdown user user-menu',
          a(
            href = '#', class='dropdown-toggle', `data-toggle` = 'dropdown',
            `aria-expanded` = "false",
            span(class = "hidden-xs", textOutput('curr_subj_code', inline = TRUE))
          ),
          tags$ul(
            class = 'dropdown-menu',
            tags$li(
              class = 'user-body',
              fluidRow(
                column(
                  width = 12L,
                  class = 'full-width-table',
                  tableOutput('curr_subj_electrodes')
                )
              )
            ),
            tags$li(
              class = 'user-footer',
              div(
                class = 'pull-left',
                actionButton('curr_subj_details_btn', 'View Details')
              ),
              div(
                class = 'pull-right',
                actionButton('curr_subj_launch_suma', 'Launch SUMA')
              )
            )
          )
        )
      )
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
    # some navigations
    output$curr_subj_code <- renderText({
      refresh = global_reactives$force_refresh_all
      if(global_reactives$has_data && check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        return(subject$id)
      }else{
        return("")
      }
    })

    output$curr_subj_electrodes <- renderTable({
      refresh = global_reactives$force_refresh_all
      has_data = global_reactives$has_data
      if(global_reactives$has_data && check_data_repo(c('subject', 'electrodes'))){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        electrodes = data_repo[['electrodes']]
        tbl = subject$electrodes
        tbl = tbl[tbl$Channel %in% electrodes, c('Channel', 'Label')]
        rownames(tbl) = NULL
        if(nrow(tbl) > 10){
          tbl = tbl[1:10,]
          tbl[10,1] = ''
          tbl[10,2] = '...'
        }
        return(tbl)
      }else{
        return(NULL)
      }
    })

    subject_modal = function(subject, current_electrodes = NULL){
      modalDialog(
        title = subject$id,
        easyClose = T,
        size = 'l',
        tabsetPanel(
          tabPanel(
            title = '3D Visualization',
            plotly::plotlyOutput('curr_subj_elec_3d')
          ),
          tabPanel(
            title = 'Table Details',
            dataTableOutput('curr_subj_elec_table')
          )
        )
      )
    }

    observeEvent(input$curr_subj_details_btn, {
      data_repo = getDefaultDataRepository()
      subject = data_repo[['subject']]
      electrodes = data_repo[['electrodes']]
      if(!is.null(subject) && length(electrodes)){
        showModal(
          subject_modal(subject = subject, current_electrodes = electrodes)
        )
      }
    })

    output$curr_subj_elec_3d <- plotly::renderPlotly({
      btn = input$curr_subj_details_btn
      has_data = global_reactives$has_data
      data_repo = getDefaultDataRepository()
      validate(need(has_data && check_data_repo('subject'), message = 'No Subject Loaded'))

      tbl = data_repo[['subject']]$electrodes
      loaded_electrodes = data_repo[['electrodes']]
      render_3d_electrodes(tbl = tbl, loaded_electrodes = loaded_electrodes)
    })

    output$curr_subj_elec_table <- renderDataTable({
      btn = input$curr_subj_details_btn
      if(global_reactives$has_data && check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        return(subject$electrodes)
      }else{
        return(NULL)
      }
    })

    observeEvent(input$curr_subj_launch_suma, {
      # launch suma
      if(check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        suma_dir = subject$dirs$suma_dir
        launch_suma(
          root_dir = suma_dir
        )
      }
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
