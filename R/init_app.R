
#' @export
init_app <- function(module_ids = NULL, suma = NULL, observables = list(), ...){



  options('ffbatchbytes' = as.numeric(rave_opts$get_options('batch_bytes')))

  if(is.null(module_ids)){
    # we need to init from a csv file

    # check data directory
    err_msg <- NULL
    if(!file.exists(rave_opts$get_options('data_dir')) && !dir.exists(rave_opts$get_options('data_dir'))){
      err_msg <- c(
        err_msg,
        '\nPlease set your data directory by assigning\n\t\t',
        'rave_opts$set_options(data_dir = [YOUR_DATA_DIR])\n\n'
      )
    }

    if(!file.exists(rave_opts$get_options('module_lookup_file'))){
      err_msg <- c(
        err_msg,
        'Cannot file module look-up file\n\t',
        'Please refer to my sample modules.csv at:\n\t\t',
        system.file('modules.csv', package = 'rave'),
        '\n\tand redirect "module_lookup_file" option to your new modules.csv:\n\t\t',
        "rave_opts$set_options(module_lookup_file = [YOUR_LOOKUP_CSV])\n\n"
      )
    }

    if(!is.null(err_msg)){
      stop(err_msg)
    }

    module_ids <- load_modules_from_file(id_only = TRUE) # This is for UI rendering
  }

  data_selector_ui = rave:::electrode_selector_UI('SELECTOR_UI', label = '')

  shinyApp(
    ui = rave:::dashboardPage(
      skin = 'purple',
      rave:::dashboardHeader(title = 'ECoG Visualization',

                             # Left aligned
                             data_selector_ui$header$imports,

                             # Right aligned
                             .list = list(
                               uiOutput(outputId = 'nav_subject_info', inline = T, container = function(...){
                                 tags$li(
                                   class="dropdown messages-menu",
                                   ...
                                 )
                               })

                             )),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = 'side_bar',
          # shinydashboard::menuItem(
          #   text = 'Selector',
          #   rave:::electrode_selector_UI('SELECTOR_UI', label = '')$sidebar,
          #   startExpanded = T
          # ),
          lapply(module_ids, function(module_id){
            module <- data_repository$get_module(module_id, session = NULL)

            shinydashboard::menuItem(
              text = module$label,
              tabName = module$id
            )
          }) %>%
            tagList()
        )
      ),
      rave:::dashboardControl(
        data_selector_ui$header$auto_calc,
        tags$li(
          actionLink('update_on_suma', 'Update With SUMA ON', icon = shiny::icon('refresh'), role = 'button'),
          class = 'control-sidebar-items'
        ),
        tags$li(
          actionLink('lauch_suma', 'Launch SUMA', icon = shiny::icon('external-link'), role = 'button'),
          class = 'control-sidebar-items'
        ),
        tags$li(
          actionLink('sync_params', 'Synchronize Sessions', icon = shiny::icon('refresh'), role = 'button'),
          class = 'control-sidebar-items'
        )
      ),
      shinydashboard::dashboardBody(
        div(
          id = 'LOADING_IMAGE',
          div(
            id = 'loading_info',
            h1('RAVE'),
            div(
              HTML(readLines(system.file('assets/helloworld.html', package = 'rave')))
              )
          )
        ),
        do.call(shinydashboard::tabItems, lapply(module_ids, function(module_id){
          module <- data_repository$get_module(module_id)
          shinydashboard::tabItem(tabName = module$id,
                  rave:::module_UI(module$id, module$label))
        }))


      )
    ),
    server = function(input, output, session){
      session$userData$session_id <- paste0(sample(
        c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = '')


      reactive_globals = reactiveValues(
        has_data = FALSE,
        auto_calculation = TRUE,
        current_module = '',
        refresh = NULL,
        update_on_suma = TRUE,
        suma_selection = data.frame(),

        current_electrodes = c()
      )

      session$userData$refresh = function(){
        reactive_globals$refresh = Sys.time()
      }

      session$onSessionEnded(function(){
        prophet$clear_session(session)
        suma$reset(session$userData$session_id)
      })
      prophet$add_session(session) #session_pool[[session$userData$session_id]] = session
      ###### Init Modules


      module_ids <- load_modules_from_file(id_only = FALSE)




      output$nav_subject_info <- renderUI({
        elec = reactive_globals$current_electrodes
        if(length(elec) > 0 && elec != "" && !is.na(elec)){

          msg = list()
          subject = prophet$get_subject(prophet$last_subject)
          valid_electrodes = length(subject$valid_electrodes)
          msg_header = sprintf('%s (1)', subject$id)

          msg[[1]] = list(
            header = 'Total Electrodes',
            content = c('Valid: ', valid_electrodes, ', Invalid: ', nrow(subject$electrodes) - valid_electrodes)
          )

          if(length(elec) > 2){
            elec = head(elec, 2)
            s_elec = paste(c(elec, '...'), collapse = ', ')
            s_label = paste(c(subject$electrode_label_by_index(elec), '...'), collapse = ', ')
          }else{
            s_elec = paste(elec, collapse = ', ')
            s_label = paste(subject$electrode_label_by_index(elec), collapse = ', ')
          }

          msg[[length(msg) + 1]] = list(
            header = reactive_globals$current_module,
            content = c('Current Electrode(s): ', s_elec)
          )

          msg_header = sprintf('%s - %s (%s)', subject$id, s_elec, s_label)

          tagList(
            a(
              class="dropdown-toggle", `data-toggle`="dropdown", href = '#',
              span(msg_header)
            ),
            tags$ul(
              class = 'dropdown-menu',
              tags$li(
                tags$ul(
                  class = 'menu',
                  lapply(msg, function(m){
                    tags$li(a(
                      h4(m$header),
                      p(paste(m$content, collapse = ''))
                    ))
                  }) %>%
                    tagList()
                )
              )
            )
          )
        }else{
          a(
            class="dropdown-toggle", `data-toggle`="dropdown", href = '#',
            span(style = 'color:#c2c2c2', 'No Data Imported')
          )
        }
      })

      # Init Observers
      observer <- Observatory$new()
      observer$start()

      if(is.null(suma) || !'SUMA' %in% class(suma)){
        suma = SUMA$new()

      }
      observer$push(suma)

      for(o in observables){
        observer$push(o)
      }

      prophet$assign_observer(observer)


      if(data_repository$has_data()){
        shinyjs::addClass(id = 'LOADING_IMAGE', 'hidden')
      }else{
        loading_observer <- observeEvent(reactive_globals$has_data, {
          if(reactive_globals$has_data != FALSE){
            shinyjs::addClass(id = 'LOADING_IMAGE', 'hidden')
            loading_observer$destroy()
          }
        })
      }


      observeEvent(input$lauch_suma, {
        suma$launch_suma(subject_id = prophet$last_subject)
      })

      observeEvent(input$sync_params, {
        # sync params
        data_repository$synchronize(session)
        prophet$reload_sessions()
      })





      observeEvent(input$update_on_suma, {
        sync_with_suma = isolate(!reactive_globals$update_on_suma)
        reactive_globals$update_on_suma = sync_with_suma
        if(sync_with_suma){
          updateActionButton(session, 'update_on_suma', label = 'Update With SUMA ON')
        }else{
          updateActionButton(session, 'update_on_suma', label = 'Update With SUMA OFF')
        }
      })


      shiny::callModule(module = electrode_selector_handler,
                        id = 'SELECTOR_UI',
                        ui_id = 'SELECTOR_UI',
                        session = session,
                        reactive_globals = reactive_globals)

      lapply(module_ids, function(module_id){
        shiny::callModule(module = rave:::module_handler, id = module_id,
                          suma = suma,
                          ui_id = module_id, session = session,
                          reactive_globals = reactive_globals,
                          module_id = module_id)
      })


      observe({
        if(reactive_globals$auto_calculation){
          logger('Auto-Calculation: ON')
        }else{
          logger('Auto-Calculation: OFF')
        }
      })



      observe({
        signals = suma$signal_from_suma()
        new_elec = data.frame()
        for(signal in signals){
          session_id = signal[['session_id']]
          new_nodes = signal[['new_nodes']]
          subject_id = signal[['subject_id']]


          if(!is.null(session_id) && session_id == session$userData$session_id && length(new_nodes) > 0){
            subject = prophet$get_subject(subject_id, temp = TRUE)

            s = div(
              h4(' - Messages From SUMA - '),
              p(
                'Electrodes selected - ', paste(new_nodes, collapse = ', '), br(),
                'Current Subject - ', subject_id, br(),
                'Label - ', paste(subject$electrode_label_by_index(new_nodes), collapse = ', '), br()
              )
            )

            new_elec = rbind(
              new_elec,
              list(
                subject_id = subject_id,
                new_node = tail(new_nodes, 1)
              ) %>%
                as.data.frame()
            )

            showNotification(s, duration = 4, type = 'warning')
          }
        }
        if(nrow(new_elec) > 0){
          reactive_globals$suma_selection = rbind(isolate(reactive_globals$suma_selection),
                                                  new_elec)
        }
      })

      observeEvent(list(
        input$side_bar
      ), {
        module_id <- input$side_bar
        if(length(module_id) > 0 && module_id %in% module_ids){
          reactive_globals$current_module <- module_id
        }
      })

    },
    ...
  )
}
