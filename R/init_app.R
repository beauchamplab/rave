# module to load data

#' Get people who uses this app
get_people = function(){
  default_fn = function(e){
    return(list(
      name = 'Beauchamplab',
      src = system.file('beauchamplab.png', package = 'rave'),
      text = "Beauchamp's lab @CAMRI, BCM, 2018",
      index = 0
    ))
  }
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
  }, error = default_fn, warning = default_fn)
}

#' Get RAM usage
#' @param modules which module(s)
#' @param data_envir default uses getDefaultDataRepository
get_mem_usage <- function(modules, data_envir){
  if(missing(data_envir)){
    data_envir = getDefaultDataRepository()
  }
  if(missing(modules)){
    modules = NULL
  }else{
    modules = unlist(modules)
  }
  session = getDefaultReactiveDomain()
  on.exit({rm(data_envir, modules, session)})


  # get total memory used
  total_mem = pryr::mem_used()
  data_usage = pryr::object_size(data_envir)
  if(length(modules)){
    lapply(modules, function(m){

      module_ram = pryr::object_size(m)

      exec_env = m$get_or_new_exec_env(session = session)

      elem_ram = sapply(as.list(exec_env$runtime_env), function(o){
        tryCatch({
          pryr::object_size(o)
        }, error = function(e){
          0
        })
      })

      usage = 0
      if(length(elem_ram)){
        usage = sum(elem_ram[elem_ram < module_ram])
      }

      list(
        Name = m$label_name,
        usage = usage
      )
    }) ->
      module_usage
    names(module_usage) = NULL

    module_total = sum(sapply(module_usage, '[[', 'usage'))

  }else{
    module_usage = list()
    module_total = 0
  }


  misc_usage = total_mem - data_usage - module_total
  misc_usage = max(misc_usage, 0)
  list(
    total_mem = total_mem,
    data_usage = data_usage,
    module_usage = module_usage,
    other_usage = misc_usage
  )
}

#' Initialize main application
#' @param modules which modules to show. Default uses "modules.csv" (see load_modules)
#' @param active_module which module to focus at start up (use module ID)
#' @param launch.browser launch browsers, default is on
#' @param theme color theme for GUI
#' @param ... other params like test.mode for module debugging
#' @import stringr
#' @import shiny
#' @import magrittr
#' @export
init_app <- function(modules = NULL, active_module = NULL, launch.browser = T,
                     theme = "purple", disable_sidebar = FALSE, simplify_header = FALSE, ...){
  tryCatch({
    rave_prepare()
  }, error = function(e){})

  test.mode = list(...)[['test.mode']]
  if(is.null(test.mode)) {
    if(is.null(modules)){
      test.mode = rave_options('test_mode')
    }else{
      test.mode = F
    }
  }
  if(!length(modules)){
    modules = load_modules()
  }

  if(!is.list(modules)){
    modules = list("______" = list(modules))
  }

  data_selector = shiny_data_selector('DATA_SELECTOR')
  ui = rave::dashboardPage(
    skin = theme,
    title = 'R Analysis and Visualization of ECoG/iEEG Data',
    header = dashboardHeader(
      title = local({
        ver = packageVersion('rave')
        sprintf('RAVE (%s)', paste(unlist(ver), collapse = '.'))
      }),
      btn_text_right = 'RAM Usage',
      data_selector$header(),
      .list = local({
        if(!simplify_header){
          tagList(
            tags$li(
              class = 'user user-menu',
              actionLink('curr_subj_details_btn', '')
            ),
            tags$li(
              class = 'user user-menu',
              actionLink('curr_subj_launch_suma', '')
            )
          )
        }
      })
    ),
    sidebar = shinydashboard::dashboardSidebar(
      disable = disable_sidebar,
      shinydashboard::sidebarMenu(
        id = 'sidebar',
        .list =
          tagList(
            lapply(names(modules)[!names(modules) %in% "______"], function(nm){
              m = modules[[nm]]
              if(nm != "______"){
                do.call(shinydashboard::menuItem, args = list(
                  text = nm,
                  expandedName = nm,
                  startExpanded = F,
                  lapply(m, function(smd){
                    mid_up = str_to_upper(smd$module_id)
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
      uiOutput('mem_usage'),
      actionLink('control_panel_refresh', 'Click here to refresh!')
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
      h2('R Analysis and Visualizations for Electrocorticography Data')
      # hr(),
      # uiOutput('.init_mask'),
      # actionLink('.init_mask_f5', "I'm Lucky Today!")
    )
  )

  server = function(input, output, session){
    # output$.init_mask <- renderUI({
    #   input$.init_mask_f5
    #   img_list = get_people()
    #   p(
    #     img(src = sprintf("%s/%s", 'https://raw.githubusercontent.com/dipterix/instrave/master/mask_img', img_list$src), alt = img_list$name),
    #     br(),
    #     span('- ', img_list$name)
    #   )
    # })


    #################################################################

    # Global variable, timer etc.
    async_timer = reactiveTimer(5000)
    # input_timer = reactiveTimer(rave_options('delay_input') / 2)
    global_reactives = reactiveValues(
      check_results = NULL,
      check_inputs = NULL,
      execute_module = '',
      has_data = FALSE,
      switch_module = NULL,
      timer_count = 0
    )
    local_data = reactiveValues(
      mem_usage = NULL
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


    observeEvent(input[['..keyboard_event..']], {
      global_reactives$keyboard_event = input[['..keyboard_event..']]
    })



    ##################################################################
    # Module to load data
    callModule(module = data_selector$server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)


    ##################################################################
    # load modules
    # progress bar won't show title here, this is because shiny render detail information first and then
    # message
    .progress = progress(title = '', max = length(unlist(modules)))
    shinirized_modules = lapply(unlist(modules), function(m){
      .progress$inc(sprintf('Loading - %s', m$label_name))
      shinirize(m, test.mode = test.mode)
    })
    .progress$close()

    observe({
      if(global_reactives$has_data){
        global_reactives$execute_module = input$sidebar
        shinyjs::hide(id = '__rave__mask__', anim = T, animType = 'slide')
        shinyjs::removeClass(selector = 'body', class = "rave-noscroll")
      }else{
        shinyjs::show(id = '__rave__mask__', anim = T, animType = 'slide')
        shinyjs::addClass(selector = 'body', class = "rave-noscroll")
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

    observe({
      refresh = global_reactives$force_refresh_all
      if(global_reactives$has_data && check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject_id = data_repo$subject$id
        epoch_name = data_repo$preload_info$epoch_name
        reference_name = data_repo$preload_info$reference_name

        rm(data_repo)
        sub_label = (sprintf('[%s] - [%s] - [%s]', subject_id, epoch_name, reference_name))
        suma_label = 'Launch SUMA'
      }else{
        suma_label = sub_label = ("")
      }
      updateActionButton(session, 'curr_subj_details_btn', label = sub_label)
      updateActionButton(session, 'curr_subj_launch_suma', label = suma_label)
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

    observe({
      input$control_panel_refresh
      local_data$mem_usage = get_mem_usage(modules)
    })

    output$mem_usage <- renderUI({
      mem_usage = local_data$mem_usage
      if(is.null(mem_usage)){
        return()
      }

      name = c(
        'Total Usage',
        'Shared Data Usage',
        sapply(mem_usage$module_usage, '[[', 'Name')
      )

      usage = c(
        mem_usage$total_mem,
        mem_usage$data_usage + mem_usage$other_usage,
        sapply(mem_usage$module_usage, '[[', 'usage')
      )

      perc = usage / max(usage)


      tagList(
        h3(class = 'control-sidebar-heading', 'Memory Usage'),
        tags$ul(
          class="control-sidebar-menu",
          style = 'padding:15px;',
          tagList(
            lapply(seq_along(name), function(ii){
              nm = name[[ii]]
              us = usage[[ii]]
              pc = perc[[ii]]
              status = switch (nm,
                               'Total Usage' = {
                                 max_ram = rave_options('max_mem') * 1000^3
                                 pc_total = us / max_ram
                                 ind = (pc_total < 0.75) + (pc_total < 0.9) + 1
                                 c('danger', 'warning', 'success')[ind]
                               },
                               'Shared Data Usage' = 'primary',
                               'Misc & Others Sessions' = 'primary',
                               {
                                 ind = (pc < 0.5) + 1
                                 c('warning', 'primary')[ind]
                               }
              )
              txt_size = as.character(to_ram_size(us))
              txt_perc = sprintf('%d%%', as.integer(pc*100))
              if(stringr::str_length(nm) > 22){
                nm_alt = paste0(stringr::str_sub(nm, end = 19), '...')
              }else{
                nm_alt = nm
              }

              tags$li(
                h4(class = 'control-sidebar-subheading', nm_alt, span(class=sprintf('label label-%s pull-right', status), txt_size)),
                div(class = 'progress progress-xxs',
                    div(class = sprintf("progress-bar progress-bar-%s", status), style = sprintf('width: %s', txt_perc)))
              )
            })
          )
        ),
        div(style = 'margin-bottom:20px')
      )

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
        gc()
      })
    }

  }
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}



#' @export
rave_main <- init_app
