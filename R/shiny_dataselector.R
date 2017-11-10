# Data selector

#' @import shiny
electrode_selector_UI = function(id, label){
  ns = shiny::NS(id)


  return(list(
    sidebar = div(
      tags$button(
        id = ns('modal_select'),
        class = 'btn btn-warning action-button',
        type = 'button',
        style = 'width: 100%; margin-left: 0px;',
        'Import Data'
      ),
      checkboxInput(ns('auto_calculate'), 'Auto-Calculation', value = TRUE)
    ),

    header = list(
      imports =
        tags$li(
          actionLink(ns('modal_select'), 'Select Data', icon = shiny::icon('tasks'), role = 'button', class = 'nav-item nav-link')
        ),
      auto_calc =
        tags$li(
          actionLink(ns('auto_calculate'), 'Auto-Calculation ON', icon = shiny::icon('calculator'), role = 'button'),
          class = 'control-sidebar-items'
        )
    )
  ))


}

#' @import shiny
electrode_selector_handler = function(input, output, session, ui_id, reactive_globals){
  ns = shiny::NS(ui_id)

  selected = reactiveValues(
    subject = NULL,
    electrode = NULL,
    last_subject = NULL,
    electrodes_text = ''
  )

  data_modal = function(){
    showModal(
      session = session,
      ui =
        modalDialog(
          size = 'l',
          title = 'Data Selection',
          easyClose = T,
          fluidRow(
            column(
              width = 4,
              selectInput(ns('subject_input'),
                          label = 'Subject ID: ',
                          choices = prophet$find_all_subjects(),
                          selected = isolate(selected$subject)),
              textInput(ns('electrode_input'), label = 'Electrodes: ', value = isolate(selected$electrodes_text),
                        placeholder = 'E.g. 1-10,15,32-66 or blank'),
              fileInput(ns('mask_selector'), 'Load Mask', multiple = F),
              hr(),
              p(tags$small(
                'If you want to use mask uploaded to select electrodes, please clear your text input first.'
              ))
            ),
            column(
              width = 8,
              tabsetPanel(
                type = 'pills',
                tabPanel(
                  'Preview',
                  div(class = 'pre-wrap',
                      verbatimTextOutput(ns('selected_print'))
                  )
                ),
                tabPanel(
                  'Full table',
                  tableOutput(ns('selected_table'))
                )
              )
              # tags$label('Selected Electrodes:'),


            )
          ),
          footer = tagList(
            actionButton(ns('do_import'), 'Import')
          )

        )
    )
  }

  observeEvent(input$subject_input, {
    tryCatch({
      subject = prophet$get_subject(subject_id = input$subject_input)
      text = sprintf('1-%s', max(subject$electrodes$Number))
      selected$electrodes_text = text
      updateTextInput(session = session, 'electrode_input', value = text)
    }, error = function(e){})
  })

  output$selected_table <- renderTable({
    electrodes <- selected$electrodes
    subject_id <- selected$subject

    subject <- prophet$get_subject(subject_id = subject_id, temp = TRUE)
    subject$electrodes %>%
      filter(Number %in% electrodes) %>%
      mutate(Subject = subject_id)
  })

  output$selected_print <- renderPrint({
    validate(
      need(length(input$subject_input) > 0, 'Select a subject first.'),
      need(stringr::str_length(stringr::str_trim(input$electrode_input)) > 0 ||
             length(input$mask_selector) > 0,
           'Select at least one electrode.'),
      need(!is.na(stringr::str_match(input$electrode_input, '^[0-9, \\-]*$')),
           'Please follow the format: "1-19, 22"...')
    )
    text_input = stringr::str_trim(input$electrode_input)
    selected$subject = input$subject_input
    electrodes = c()

    tryCatch({
      if(stringr::str_length(text_input) > 0){
        # use text input
        for(x in stringr::str_split(text_input, ',', simplify = T)){
          if(stringr::str_detect(x, '\\-')){
            parts = as.integer(stringr::str_split_fixed(x, '\\-', 2))
            electrodes = c(electrodes, seq(parts[1], parts[2]))
          }else{
            electrodes = c(electrodes, as.integer(x))
          }
        }
      }


      if(!is.null(input$mask_selector)){
        # File upload
        inFile = input$mask_selector
        mask = read.csv(inFile$datapath, header = F)[,1]
        electrodes = c(electrodes, which(as.integer(mask) != 0))
      }

      electrodes = unique(electrodes)

      # get subject
      subject = prophet$get_subject(input$subject_input)
      electrodes = subject$filter_valid_electrodes(electrodes)
      if(length(electrodes) == 0){
        electrodes = subject$valid_electrodes
      }

      elec_names = subject$electrode_label_by_index(electrodes)
      selected$electrodes = electrodes
      show_text = stringr::str_c('#', electrodes, '-', elec_names)
      cat(sprintf('Total %d electrodes.\n', length(electrodes)))
      if(length(show_text) > 70){
        show_text = c(show_text[1:70], '...')
      }
      cat(show_text, sep = ', ')
    }, error = function(e){
      cat('Oops...\n')
      print(e)
    })

  })

  observeEvent(selected$subject, {
    if(!is.null(selected$last_subject) && selected$last_subject != selected$subject){
      last_subject <- prophet$get_subject(selected$last_subject)
      last_subject$data_environment$deactivate()

      this_subject <- prophet$get_subject(selected$subject)
      this_subject$data_environment$activate()
    }
  })

  observeEvent(input$modal_select, {
    # Show modal
    data_modal()
  })


  observeEvent(input$do_import, {
    auto_calc = isolate(reactive_globals$auto_calculation)
    reactive_globals$auto_calculation = FALSE
    # import data
    # check!
    checks = c(
      need(length(input$subject_input) > 0, 'Select a subject first.'),
      need(length(selected$electrodes) > 0, 'No electrode selected.')
    )
    if(length(checks) > 0){
      for(check in checks){
        showNotification(
          check,
          duration = 3,
          type = c('error')
        )
      }
    }else{
      # import!
      shinyjs::disable(ns('do_import'))
      subject = prophet$get_subject(selected$subject)

      electrodes = selected$electrodes

      status <- subject$data_environment$load(electrodes, async = FALSE)
      if(is.null(status)){
        subject$data_environment$bind_electrodes(electrodes = electrodes)
        data_repository$set_data(subject$data_environment)

        showNotification(
          duration = 3, closeButton = T, type = 'message', session = session,
          ui = p(
            'Loaded:', br(),
            'Subject - ', subject$id, br(),
            'Electrode - Total: ', length(subject$data_environment$electrodes)
          )
        )
        shinyjs::enable(ns('do_import'))
        reactive_globals$has_data = Sys.time()

        reactive_globals$current_electrodes = subject$data_environment$electrodes
        # Notify other sessions to change data
        prophet$reload_sessions()
      }else{
        # async import is used
        showNotification(
          duration = 3, closeButton = T, type = 'message', session = session,
          ui = p('Scheduled Importing Electrodes: Total ', length(electrodes))
        )
        local({
          tmp_observer <- observeEvent(status(), {
            if(status() == 'done'){
              subject$data_environment$bind_electrodes(electrodes = electrodes)
              data_repository$set_data(subject$data_environment)

              showNotification(
                duration = 3, closeButton = T, type = 'message', session = session,
                ui = p(
                  'Loaded:', br(),
                  'Subject - ', subject$id, br(),
                  'Electrode - Total: ', length(subject$data_environment$electrodes)
                )
              )
              shinyjs::enable(ns('do_import'))
              reactive_globals$has_data = Sys.time()

              reactive_globals$current_electrodes = subject$data_environment$electrodes
              # Notify other sessions to change data
              prophet$reload_sessions()

              tmp_observer$destroy()
            }
          })
        })
      }


      removeModal()


    }


    reactive_globals$auto_calculation = auto_calc
  })

  observeEvent(input$auto_calculate, {
    is_auto = !isolate(reactive_globals$auto_calculation)
    reactive_globals$auto_calculation = is_auto
    if(is_auto){
      # shinyjs::html(id = ns('auto_calculate'), html = '<i class="fa fa-calculator"></i><span>Auto-Calculation ON</span>', add = FALSE)
      updateActionButton(session = session, inputId = 'auto_calculate', label = 'Auto-Calculation ON')
    }else{
      # shinyjs::html(id = ns('auto_calculate'), html = '<i class="fa fa-calculator"></i><span>Auto-Calculation OFF</span>', add = FALSE)
      updateActionButton(session = session, inputId = 'auto_calculate', label = 'Auto-Calculation OFF')
    }


  })

}


#' @import shiny
electrode_selector_example = function(){
  shinyApp(
    ui = rave:::dashboardPage(
      skin = 'purple',
      rave:::dashboardHeader(title = 'Electrode Selector'),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem(
            text = 'Selector',
            rave:::electrode_selector_UI('SELECTOR_UI', label = '')$sidebar,
            startExpanded = T
          )
        )
      ),
      rave:::dashboardControl(div()),
      shinydashboard::dashboardBody()
    ),
    server = function(input, output, session){
      reactive_globals = reactiveValues(
        auto_calculation = TRUE
      )
      shiny::callModule(module = rave:::electrode_selector_handler, id = 'SELECTOR_UI', ui_id = 'SELECTOR_UI', session = session, reactive_globals = reactive_globals)

      observe({
        print(reactive_globals$auto_calculation)
      })
    }
  )
}



