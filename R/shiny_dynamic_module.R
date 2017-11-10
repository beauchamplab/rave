
#' @import shiny
module_UI <- function(id, label, ...){
  ns <- shiny::NS(id)

  ui <- uiOutput(ns('dynamic_module_ui'))

  return(ui)
}

#' @import shiny
module_handler <- function(
  input, output, session, ui_id, reactive_globals, module_id, suma = NULL
){
  ns <- shiny::NS(ui_id)

  if(is.null(suma) || !'SUMA' %in% class(suma)){
    print(suma)
    print('asasdadasdasd')
    suma = SUMA$new()
  }
  module <- data_repository$get_module(module_id = module_id)
  input_inst <- module$runtime_env$SHINY_INPUT
  output_inst <- module$runtime_env$SHINY_OUTPUT

  triggers <- reactiveValues(
    input = Sys.time(),
    output = Sys.time(),
    suma_table = data.frame()
  )
  output$dynamic_module_ui <- renderUI({
    fluidRow(
      column(
        width = 3,
        fluidRow(
          shinydashboard::box(
            width = 12,
            collapsible = TRUE,
            title = 'Global Variables: ',
            lapply(input_inst, function(comp){
              if(!is.null(comp$global_var)){
                comp$ns <- ns
                comp$ui()
              }
            }) %>%
              tagList()
          ),
          shinydashboard::box(
            width = 12,
            collapsible = TRUE,
            title = 'Local Variables: ',
            lapply(input_inst, function(comp){
              if(is.null(comp$global_var)){
                comp$ns <- ns
                comp$ui()
              }
            }) %>%
              tagList()
          ),
          shinydashboard::box(
            width = 12,
            collapsible = TRUE,
            title = 'Description',
            get('SHINY_DESCRIPTION', envir = module$runtime_env) %>%
              HTML() %>%
              div(),
            hr(),
            actionButton(ns('suma'), 'Push to SUMA'),
            actionButton(ns('export_module'), 'Export Module'),
            actionButton(ns('reload_module'), 'Reload Module')
          )
        )
      ),
      local({
        args <- lapply(names(output_inst), function(panel_name){
          output_comp <- lapply(output_inst[[panel_name]], function(comp){
            comp$ns <- ns
            comp$ui()
          })
          shiny::tabPanel(
            title = panel_name,
            shiny::fluidRow(
              tagList(output_comp)
            )
          )
        })
        args[['width']] <- 9
        do.call(shinydashboard::tabBox, args = args)
      })
    )
  })


  # Running in reactive environment
  update_module <- function(reload = FALSE, load_packages = FALSE){
    flush_toilet$suspend()
    if(reload){
      module$reload()
    }
    try({
      module$shinirize(input, output, session, triggers)
    })

    flush_toilet$resume()
    triggers$input = Sys.time()
  }

  observeEvent(reactive_globals$current_module, {
    if(data_repository$has_data() && reactive_globals$current_module == module$id){
      logger('Switching to Module: ', module$id)
      update_module()
    }
  })

  observeEvent(reactive_globals$refresh, {
    if(data_repository$has_data()){
      update_module()
    }
  })
  observeEvent(reactive_globals$has_data, {
    if(reactive_globals$has_data != FALSE && data_repository$has_data()){
      update_module()

    }
  })

  observeEvent(reactive_globals$suma_selection, {
    sel = reactive_globals$suma_selection
    if(nrow(sel) > 0 && isolate(reactive_globals$update_on_suma)){
      new_node = tail(sel$new_node, 1)
      subject_id = tail(sel$subject_id, 1)

      if(subject_id == prophet$last_subject){
        subject = prophet$get_subject(prophet$last_subject, temp = TRUE)
        if(new_node %in% subject$valid_electrodes && new_node %in% data_repository$get_data()$electrodes){
          tryCatch({
            updateSelectInput(session, module$runtime_env$UNIVARIATE_MODE, selected = new_node)
          }, error = function(e){
            traceback(e)
            logger('Cannot set electrode, Please update manually')
          })
        }
      }
    }
  })


  flush_toilet <- observe({

    t = Sys.time()
    val <- triggers$input
    delay <- as.numeric(rave_opts$get_options('delay_input'))


    if(is.null(val) || reactive_globals$auto_calculation == FALSE || !(isolate(reactive_globals$current_module == module$id))){
      return()
    }else if(t < val + delay / 1000){
      logger('Flush Buffer Later')
      invalidateLater(delay, session = session)
    }else{
      logger('Flushing Buffers - ', module$id)
      if(as.logical(rave_opts$get_options('debug'))){
        logger('Updating Results ', toString(val))
        module$eval(input = isolate(input))

        logger('Sending render signals ', toString(val))
        triggers$output = Sys.time()
      }else{
        tryCatch({
          logger('Updating Results ', toString(val))
          module$eval(input = isolate(input), reactive_input = input)

          logger('Sending render signals ', toString(val))
          triggers$output = Sys.time()
        }, error = function(e){
          traceback(e)
          cat('\n-------------------------------------------------\n')
        })
      }

      triggers$input = NULL
    }
  }, suspended = TRUE)

  # input_signal <- shiny::debounce(reactive(triggers$input), millis = as.numeric(rave_opts$get_options('delay_input')))
  # flush_toilet <- observe({
  #   val <- input_signal()
  #   if(is.null(val)){
  #     return()
  #   }
  #   if(isolate(reactive_globals$auto_calculation) != FALSE && isolate(reactive_globals$current_module) == module$id){
  #     tryCatch({
  #       logger('Updating Results ', toString(val))
  #       module$eval(input = isolate(input))
  #
  #       logger('Sending render signals ', toString(val))
  #       triggers$output = Sys.time()
  #     }, error = function(e){
  #       traceback(e)
  #       cat('\n-------------------------------------------------\n')
  #     })
  #   }
  #
  #   val = NULL
  # })


  # open a modal asking for electrodes
  show_suma_controller <- function(){
    current_electrodes <- data_repository$get_data()$electrodes
    if(length(current_electrodes) == 0){
      current_electrodes = ''
    }else{
      current_electrodes = paste(current_electrodes, collapse = ',')
    }

    showModal(
      ui = modalDialog(
        title = 'SUMA Controller',
        size = 'l',
        easyClose = T,
        footer = tagList(
          actionButton(ns('do_push_suma'), 'Calculate')
        ),
        # body part
        fluidRow(
          shinydashboard::tabBox(
            id = ns('suma_modal'),
            title = NULL,
            width = 12,
            tabPanel(
              title = 'Mask',
              fluidRow(
                column(
                  width = 4,
                  textInput(ns('electrode_input'), label = 'Electrodes: ',
                            placeholder = 'E.g. 1-10,15,32-66 or blank', value = current_electrodes),
                  hr(),
                  p(tags$small(
                    textOutput(ns('electrode_count'), inline = T)
                  ))
                ),
                column(
                  width = 8,
                  plotly::plotlyOutput(ns('suma_3d_coordinates'))
                )
              )
            )
          )
        )
      )
    )
  }

  observeEvent(input$suma, {
    show_suma_controller()
  })

  output$suma_3d_coordinates <- plotly::renderPlotly({
    subject <- prophet$get_subject(prophet$last_subject)


    force(input$electrode_input)
    Electrodes <- subject$electrodes
    Electrodes$Mask <- FALSE
    Electrodes[get_suma_electrodes(), 'Mask'] <- TRUE
    Electrodes$SUMA <- NA

    # suma values

    if(!is.null(triggers$suma_table)){
      for(i in 1:nrow(triggers$suma_table)){
        node <- as.numeric(triggers$suma_table[i, 1])

        Electrodes[Electrodes$Number == node, 'SUMA'] <- as.numeric(triggers$suma_table[i, 2])
      }
    }




    axs <- function(title){
      list(
        title = title,
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
    }
    print((Electrodes))

    plotly::plot_ly() ->
      p_ly

    Electrodes %>% filter(Mask, !is.na(SUMA)) %>%
      nrow() ->
      calculated

    if(calculated > 0){
      p_ly %>%
        plotly::add_markers(x = ~Coord_x, y = ~Coord_y, z = ~Coord_z,
                            name = 'Mask-SUMA Value',
                            color = ~SUMA,
                            text = ~ sprintf('SUMA Value: %.2f<br />Electrode: %s (%d)', SUMA, Label, Number),
                            hoverinfo = 'text',
                            marker = list(
                              colorbar = list(title = "SUMA Value")
                            ),


                            data = Electrodes %>% filter(Mask, !is.na(SUMA))) ->
        p_ly
    }

    Electrodes %>% filter(Mask, is.na(SUMA)) %>%
      nrow() ->
      new_elec

    if(new_elec > 0){
      p_ly %>%
        plotly::add_markers(x = ~Coord_x, y = ~Coord_y, z = ~Coord_z,
                            name = 'Mask-New Electrodes',
                            hoverinfo = 'name',
                            marker = list(
                              opacity = 0.4,
                              color = 'rgb(0,0,0)'
                            ),
                            data = Electrodes %>% filter(Mask, is.na(SUMA))) ->
        p_ly
    }

    Electrodes %>% filter(!Mask) %>%
      nrow()->
      not_mask
    if(not_mask > 0){
      p_ly %>%
        plotly::add_markers(x = ~Coord_x, y = ~Coord_y, z = ~Coord_z,
                            marker = list(
                              opacity = 0.1,
                              color = 'rgb(111, 111, 111)',
                              symbol = 'cross'
                            ),
                            name = 'Not Loaded',
                            hoverinfo = 'name',
                            data = Electrodes %>% filter(!Mask)) ->
        p_ly
    }
    p_ly %>%
      plotly::layout(
        scene = list(
          xaxis = axs('X'), yaxis = axs('Y'), zaxis = axs('Z')
        )
      )
  })

  get_suma_electrodes <- function(){
    electrode_input <- isolate(stringr::str_trim(input$electrode_input))


    tryCatch({
      if(stringr::str_length(electrode_input) > 0){
        electrodes = NULL
        # use text input
        for(x in as.vector(stringr::str_split(electrode_input, ',', simplify = T))){
          if(stringr::str_detect(x, '\\-')){
            parts = as.integer(stringr::str_split_fixed(x, '\\-', 2))
            electrodes = c(electrodes, seq(parts[1], parts[2]))
          }else{
            electrodes = c(electrodes, as.integer(x))
          }
        }
        return(electrodes)
      }else{
        return(NULL)
      }
    }, error = function(e){
      return(NULL)
    }) ->
      electrodes
    subject <- prophet$get_subject(prophet$last_subject)
    if(is.null(electrodes)){
      electrodes = subject$valid_electrodes
    }else{
      electrodes = subject$filter_valid_electrodes(electrodes)
    }

    if(length(electrodes) == 0){
      electrodes = subject$valid_electrodes
    }
    return(electrodes)
  }

  output$electrode_count <- renderText({
    force(input$electrode_input)
    electrodes <- get_suma_electrodes()
    return(sprintf('Total %d electrodes selected...', length(electrodes)))
  })


  observeEvent(input$do_push_suma, {

    suma_table = module$get_SUMA(
      isolate(reactiveValuesToList(input)),
      electrodes = get_suma_electrodes()
    )
    print(suma_table)

    triggers$suma_table <- suma_table

    # Here we push to suma
    # names of suma_table: electrode, value
    subject <- prophet$get_subject(prophet$last_subject)

    filename <- suma$push_to_suma(suma_table = suma_table, module_id = module$id, subject = subject)

    showNotification(session = session, ui = sprintf('%s generated!', filename), type = 'message')
  })

  # observeEvent(input$do_push_suma, {
  #
  #   manager <- module$get_SUMA(
  #     isolate(reactiveValuesToList(input)),
  #     electrodes = get_suma_electrodes()
  #   )
  #
  #   local({
  #     tmp_obs <- observeEvent(manager$status(), {
  #       if(length(manager$status()) > 0 && manager$status() == 'resolved'){
  #         suma_table <- manager$process()
  #         triggers$suma_table <- suma_table
  #
  #         # Here we push to suma
  #         # names of suma_table: electrode, value
  #         subject <- prophet$get_subject(prophet$last_subject)
  #
  #         designed = data.frame(
  #           electrode = 1:length(subject$electrodes)
  #         )
  #
  #         suma_table <- merge(designed, suma_table, all.x = TRUE, by = 'electrode')
  #
  #         with(suma_table, {
  #           value = rep(value, each = 42)
  #           value
  #         }) ->
  #           value
  #
  #
  #
  #
  #         suma$push_to_suma(value = value)
  #         tmp_obs$destroy()
  #       }
  #     })
  #   })
  # })

  if(!module$suma_enabled){
    shinyjs::disable(ns('suma'))
  }else{
    module$runtime_env$.suma = suma
  }

  observeEvent(input$reload_module, {
    update_module(reload = TRUE)
  })

  observeEvent(input$export_module, {
    subject <- prophet$get_subject(prophet$last_subject)
    params <- sapply(module$runtime_env$SHINY_INPUT, function(comp){
      id <- comp$id
      re <- list(input[[id]])
      names(re) <- id
      re
    })
    script_path <- export_module(subject, module, params)
    showNotification(
      ui = p('Module exported to: ', br(), script_path),
      duration = 5,type = 'message'
    )
  })


  tryCatch({
    observeEvent(input[[module$runtime_env$UNIVARIATE_MODE]], {
      reactive_globals$current_electrodes = input[[module$runtime_env$UNIVARIATE_MODE]]
    })
  }, error = function(e){})

}

#
# module_example <- function(){
#   # rave_opts$set_options(data_dir = 'D:/Documents/Dropbox/Dropbox/rave/data')
#
#   shinyApp(
#     ui = rave:::dashboardPage(
#       skin = 'purple',
#       rave:::dashboardHeader(title = 'Electrode Selector'),
#       shinydashboard::dashboardSidebar(
#         shinydashboard::sidebarMenu(
#           shinydashboard::menuItem(
#             text = 'Selector',
#             rave:::electrode_selector_UI('SELECTOR_UI', label = ''),
#             startExpanded = T
#           )
#         )
#       ),
#       rave:::dashboardControl(div()),
#       shinydashboard::dashboardBody(
#         rave:::module_UI('MODULE_UI', 'Test Module')
#       )
#     ),
#     server = function(input, output, session){
#
#
#       module_id = 'cluster_within_electrode'
#       module = ModuleEnv$new(
#         module_id = 'cluster_within_electrode',
#         label = 'Spearman Correlation',
#         category = 'Trial Clusters',
#         is_univariate = TRUE,
#         source_path = system.file('modules/cluster_within electrode.R', package = 'rave')
#       )
#
#
#       reactive_globals = reactiveValues(
#         has_data = FALSE,
#         auto_calculation = TRUE
#       )
#       shiny::callModule(module = rave:::electrode_selector_handler, id = 'SELECTOR_UI', ui_id = 'SELECTOR_UI', session = session, reactive_globals = reactive_globals)
#
#       shiny::callModule(module = rave:::module_handler, id = 'MODULE_UI',
#                         ui_id = 'MODULE_UI', session = session,
#                         reactive_globals = reactive_globals,
#                         module_id = 'cluster_within_electrode')
#
#       observe({
#         if(reactive_globals$auto_calculation){
#           logger('Auto-Calculation: ON')
#         }else{
#           logger('Auto-Calculation: OFF')
#         }
#       })
#
#     }
#   )
# }
