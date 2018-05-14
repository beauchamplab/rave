# module to load data

get_people = function(){
  tryCatch({
    # get yaml file from dipterix repo
    genv = globalenv()
    if(!is.null(genv[['..all_index']])){
      img_list = genv[['..all_index']]
    }else{
      img_list = yaml::read_yaml('https://raw.githubusercontent.com/dipterix/instrave/master/mask_img/index.yaml')
      genv[['..all_index']] = img_list
    }

    if(!is.null(genv[['..last_index']])){
      img_list = img_list[-genv[['..last_index']][['index']]]
    }
    img_list = sample(img_list, 1)[[1]]
    genv[['..last_index']] = img_list
    return(img_list)
  }, error = function(e){
    return(list(
      name = 'Beauchamplab',
      src = system.file('beauchamplab.png', package = 'rave'),
      text = "Beauchamp's lab @CAMRI, BCM, 2018",
      index = 0
    ))
  })
}


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
        .list =
          # local({
          #   sel = names(modules) %in% "______"
          #   grouped = modules[sel]
          #   singles = modules[!sel]
          #
          #
          #   if(length(grouped)){
          #     lapply(names(grouped), function(group_name)){
          #       quo({
          #         shinydashboard::menuItem(
          #           text = !!group_name,
          #           expandedName = !!group_name,
          #           startExpanded = F,
          #
          #         )
          #       })
          #     }
          #   }
          # })
        tagList(
          lapply(names(modules)[!names(modules) %in% "______"], function(nm){
            m = modules[[nm]]
            if(nm != "______"){
              do.call(shinydashboard::menuItem, args = list(
                text = nm,
                expandedName = nm,
                startExpanded = F,
                lapply(m, function(smd){
                  shinydashboard::menuSubItem(
                    text = smd$label_name,
                    tabName = str_to_upper(smd$module_id)
                  )
                })
              ))
            }
          }),
          lapply(unlist(modules[["______"]]), function(smd){
            shinydashboard::menuItem(
              text = smd$label_name,
              tabName = str_to_upper(smd$module_id)
            )
          })
        )
      )
    ),
    control = dashboardControl(
      data_selector$control()
    ),
    body = shinydashboard::dashboardBody(
      do.call(
        shinydashboard::tabItems,
        args = local({
          re = lapply(unlist(modules), function(m) {
            shinydashboard::tabItem(tabName = str_to_upper(m$module_id),
                                    uiOutput(str_c(m$module_id, '_UI')))
          })
          names(re) = NULL
          re
        })
      )
    ),
    initial_mask = tagList(
      h2('R Analysis and Visualizations for Electrocorticography Data'),
      hr(),
      uiOutput('.init_mask'),
      actionLink('.init_mask_f5', "I'm Lucky Today!")
    )
  )

  server = function(input, output, session){
    output$.init_mask <- renderUI({
      input$.init_mask_f5
      img_list = get_people()
      p(
        img(src = sprintf("%s/%s", 'https://raw.githubusercontent.com/dipterix/instrave/master/mask_img', img_list$src), alt = img_list$name),
        br(),
        HTML("<span>&#8220;", img_list$text, "&#8221;</span>"),br(),
        span('- ', img_list$name)
      )
    })


    #################################################################

    # Global variable, timer etc.
    async_timer = reactiveTimer(1000)
    # input_timer = reactiveTimer(rave_options('delay_input') / 2)
    global_reactives = reactiveValues(
      check_results = NULL,
      check_inputs = NULL,
      execute_module = '',
      has_data = FALSE
    )
    observeEvent(async_timer(), {
      global_reactives$check_results = Sys.time()
    })
    # observeEvent(input_timer(), {
    #   global_reactives$check_inputs = Sys.time()
    # })
    ##################################################################
    # Module to load data
    callModule(module = data_selector$server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)


    ##################################################################
    # load modules
    shinirized_modules = lapply(unlist(modules), rave:::shinirize, test.mode = test.mode)

    observe({
      if(global_reactives$has_data){
        global_reactives$execute_module = input$sidebar
        shinyjs::hide(id = '__rave__mask__')
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
            plotly::plotlyOutput('curr_subj_elec_3d', height = '600px')
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
      validate(need(global_reactives$has_data, "Please import subject first."))
      btn = input$curr_subj_details_btn
      has_data = global_reactives$has_data
      data_repo = getDefaultDataRepository()
      validate(need(has_data && check_data_repo('subject'), message = 'No Subject Loaded'))

      tbl = data_repo[['subject']]$electrodes
      loaded_electrodes = data_repo[['electrodes']]
      # get latest value
      tryCatch({
        data_repo = getDefaultDataRepository()
        suma_out_dir = data_repo$subject$dirs$suma_out_dir

        module = modules[vapply(unlist(modules), function(x){
          global_reactives$execute_module == str_to_upper(x$module_id)
        }, FALSE)]
        pattern = module[[1]]$label_name
        pattern = sprintf('%s_([0-9_\\-]+).csv', str_replace_all(pattern, '[^a-zA-Z0-9_]', '_'))
        print(pattern)
        dat = list.files(suma_out_dir, pattern = pattern)
        fname = dat[which.max(as.numeric(strptime(str_match(dat, pattern)[,2], '%Y-%m-%d_%H_%M_%S')))]
        dat = read.csv(file.path(suma_out_dir, fname))
        dat = dat[dat[, 1] %in% loaded_electrodes, ]
        values = loaded_electrodes * NA
        values[loaded_electrodes %in% dat[,1]] = dat[,2]
        values
      }, error = function(e){
        NULL
      }) ->
        values
      rave:::render_3d_electrodes(tbl = tbl, loaded_electrodes = loaded_electrodes, values = values)
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
        lapply(unlist(modules), function(x){
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
