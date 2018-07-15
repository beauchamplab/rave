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
init_app <- function(modules = NULL, active_module = NULL, launch.browser = T, ...){
  tryCatch({
    rave_prepare()
  }, error = function(e){})

  test.mode = list(...)[['test.mode']]
  if(is.null(test.mode)) test.mode = rave_options('test_mode')
  if(length(modules) == 0){
    modules = load_modules()
  }

  if(!is.list(modules)){
    modules = list("______" = list(modules))
  }

  data_selector = rave:::shiny_data_selector('DATA_SELECTOR')
  ui = rave::dashboardPage(
    skin = "purple",
    title = 'R Analysis and Visualization of ECoG/iEEG Data',
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
          tagList(
            lapply(names(modules)[!names(modules) %in% "______"], function(nm){
              m = modules[[nm]]
              mid_up = str_to_upper(smd$module_id)

              if(nm != "______"){
                do.call(shinydashboard::menuItem, args = list(
                  text = nm,
                  expandedName = nm,
                  startExpanded = F,
                  lapply(m, function(smd){
                    shinydashboard::menuSubItem(
                      text = smd$label_name,
                      tabName = mid_up,
                      selected = mid_up %in% active_module
                    )
                  })
                ))
              }
            }),
            lapply(unlist(modules[["______"]]), function(smd){
              mid_up = str_to_upper(smd$module_id)
              shinydashboard::menuItem(
                text = smd$label_name,
                tabName = mid_up,
                selected = mid_up %in% active_module
              )
            })
          )
      )
    ),
    control = dashboardControl(
      fluidRow(
        column(
          width = 12L,
          # 3D viewer
          ''#threejsr::threejsOutput('__rave_3dviewer', width = '100%', height = '60vh')
        )
      )
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
      has_data = FALSE,
      switch_module = NULL
    )
    observeEvent(async_timer(), {
      global_reactives$check_results = Sys.time()
    })

    # unlist(modules) will flatten modules but it's still a list
    module_ids = str_to_upper(sapply(unlist(modules), function(m){m$module_id}))
    update_variable = function(module_id, variable_name = NULL, value = NULL, flush = T, ...){
      if(is.null(variable_name)){
        return()
      }
      tryCatch({
        module_id = str_to_upper(module_id)
        m = unlist(modules)[module_ids %in% module_id]
        if(length(m) == 1){
          m = m[[1]]
          e = m$get_or_new_exec_env()
          e$cache_input(inputId = variable_name, val = value, read_only = F)
          if(flush && module_id == str_to_upper(isolate(input$sidebar))){
            # When target module is not the same as current module, we need manually refresh current module
            e$input_update(input = list(), init = T)
          }
        }
      }, error = function(e){
        logger('Cannot update variable ', variable_name, ' in module ', module_id, level = 'WARNING')
        logger(e, level = 'WARNING')
      })
    }

    # Switch to module
    observe({
      module_info = global_reactives$switch_module
      if(!is.null(module_info)){
        mid = module_info$module_id = str_to_upper(module_info$module_id)
        if(mid %in% module_ids){
          logger('Switching to module - ', mid, level = 'INFO')
          do.call(update_variable, module_info)
          session$sendCustomMessage('rave_sidebar_switch', module_info)
        }
      }
    })

    # Threejs 3D viewer click callback
    # observeEvent(input[['__rave_threejsr_callback']], {
    #   dat = input[['__rave_threejsr_callback']]
    #   # dat = list(
    #   #   module_id, electrode, variable_name
    #   # )
    #   mid = dat$module_id = str_to_upper(dat$module_id)
    #   if(mid %in% module_ids){
    #     do.call(update_variable, dat)
    #     session$sendCustomMessage('rave_sidebar_switch', dat)
    #   }
    # })

    ##############
    # Control panel
    output[['__rave_3dviewer']] <- threejsr::renderThreejs({
      if(global_reactives$has_data){
        tbl = subject$electrodes
        es = subject$valid_electrodes
        sel = tbl$Electrode %in% es


        # Set2 and Paired from RColorBrewer, hard coded
        pal = c(
          "#66C2A5",  "#FC8D62",  "#8DA0CB",  "#E78AC3",  "#A6D854",  "#FFD92F",  "#E5C494",  "#B3B3B3",
          "#A6CEE3",  "#1F78B4",  "#B2DF8A",  "#33A02C",  "#FB9A99",  "#E31A1C",  "#FDBF6F",  "#FF7F00",  "#CAB2D6",  "#6A3D9A",  "#FFFF99",  "#B15928"
        )

        tbl$marker = with(tbl, {
          Label[is.na(Label)] = ''
          sprintf('Group - %s [%s]<br/>Electrode - %d %s<br/>Position - %.1f,%.1f,%.1f<br/>', Group, Type, Electrode, Label, Coord_x, Coord_y, Coord_z)
        })
        value = with(tbl[sel, ], {
          pal[as.numeric(as.factor(Group))]
        })

        # get data_env
        module_tools$plot_3d_electrodes(
          tbl = tbl,
          electrodes = es,
          values = value,
          marker = tbl$marker,
          palette = pal,
          symmetric = F,
          fps = 2,
          control_gui = F,
          loop = F,
          sidebar = tagList(
            fileInput('__rave_3dviewer_data', label = 'Upload')
          )
        )
      }
    })

    observeEvent(input[['__rave_3dviewer_data']], {
      print(input[['__rave_3dviewer_data']])
    })



    ##################################################################
    # Module to load data
    callModule(module = data_selector$server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)


    ##################################################################
    # load modules
    shinirized_modules = lapply(unlist(modules), rave:::shinirize, test.mode = test.mode)

    observe({
      if(global_reactives$has_data){
        global_reactives$execute_module = input$sidebar
        shinyjs::hide(id = '__rave__mask__', anim = T, animType = 'slide')
      }else{
        shinyjs::show(id = '__rave__mask__', anim = T, animType = 'slide')
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
        subject_id = data_repo$subject$id
        epoch_name = data_repo$preload_info$epoch_name
        reference_name = data_repo$preload_info$reference_name

        rm(data_repo)
        return(sprintf('[%s] - [%s] - [%s]', subject_id, epoch_name, reference_name))
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
        electrodes = data_repo$preload_info$electrodes
        tbl = subject$electrodes
        tbl = tbl[tbl$Electrode %in% electrodes, c('Electrode', 'Label')]
        rownames(tbl) = NULL
        if(nrow(tbl) > 10){
          tbl = tbl[1:10,]
          tbl[10,1] = ''
          tbl[10,2] = '...'
        }
        rm(data_repo)
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
        dataTableOutput('curr_subj_elec_table')
      )
    }

    observeEvent(input$curr_subj_details_btn, {
      data_repo = getDefaultDataRepository()
      subject = data_repo[['subject']]
      electrodes = data_repo$preload_info$electrodes
      if(!is.null(subject) && length(electrodes)){
        showModal(
          subject_modal(subject = subject, current_electrodes = electrodes)
        )
      }
    })

    output$curr_subj_elec_table <- renderDataTable({
      btn = input$curr_subj_details_btn
      if(global_reactives$has_data && check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        tbl = subject$electrodes
        cols = names(tbl)
        cols = cols[!cols %in% c('Coord_x', 'Coord_y', 'Coord_z')]
        return(tbl[, cols])
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
