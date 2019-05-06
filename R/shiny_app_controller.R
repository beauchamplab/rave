app_controller <- function(
  modules = NULL, active_module = NULL, launch.browser = T,
  theme = "purple", disable_sidebar = FALSE, simplify_header = FALSE, ...
){

  # Register customized inputs
  register_compoundInput()

  # Detach rave data environment
  tryCatch({ rave_prepare() }, error = function(e){})

  # Get options of whether this is a test mode
  test.mode = list(...)[['test.mode']]; test.mode = isTRUE(test.mode)

  # Load all possiple modules
  controller_env = environment()
  module_table = arrange_modules(refresh = F, reset = F, quiet = T)

  module_list = new.env()
  module_list2 = new.env()

  # Function to push a module to the list
  cache_module = function(m, check = FALSE){
    ## check all module_list2
    if(check){
      nms = paste0('.future_', module_table$ID)
      sel = nms %in% names(module_list2)
      if(any(sel)){
        nms = cbind(nms[sel], module_table$ID[sel])

        apply(nms, 1, function(x){
          if(future::resolved(module_list2[[x[1]]])){
            m = module_list2[[x[2]]]
            cache_module(m, check = FALSE)
          }
        })
      }

    }


    if(!'R6' %in% class(m)){
      return()
    }
    id = m$module_id
    sel = module_table$ID == id

    if(length(sel) && sum(sel) == 1 && isTRUE(module_table$Active[sel])){
      group = module_table$Group[sel]
      m$label_name = module_table$Name[sel]
    }else{
      group = '______'
    }

    module_list[[group]] %?<-% list()

    has_cached = vapply(module_list[[group]], function(m1){
      m1$module_id == id
    }, FUN.VALUE = FALSE)
    if(!any(has_cached)){
      module_list[[group]][[length(module_list[[group]]) + 1]] = m
    }

    invisible()
  }

  cached_ids = NULL
  if(!is.null(modules)){
    lapply(unlist(list(modules)), cache_module)
    cached_ids = sapply(unlist(list(modules)), function(m){m$module_id})
  }

  local({
    tb = module_table[module_table$Active & !module_table$ID %in% cached_ids, ]
    nmodules = nrow(tb)
    if(nmodules){
      future::plan(future::multiprocess, workers = min(nmodules + 1, 20))
      lapply(seq_len(nmodules), function(ii){
        row = tb[ii, ]
        future::futureAssign(row$ID, {
          get_module(package = row$Package, module_id = row$ID, local = FALSE)
        }, globals = c('row'), assign.env = module_list2)
      })

      future::plan(future::multiprocess, workers = rave_options('max_worker'))

    }
  });

  .get_module = function(package, module_id, local = FALSE){
    # m = get0(module_id, envir = module_list2, ifnotfound = NULL)
    m = module_list2[[module_id]]
    if(is.null(m)){
      m = get_module(package = package, module_id = module_id, local = FALSE)
    }
    return(m)
  }
  load_module = function(module_id = NULL, package = NULL, check = FALSE){

    loaded_ids = sapply(unlist(as.list(module_list)), function(m){ m$module_id})

    if(is.null(module_id) && is.null(package)){
      if(!package_installed('ravebuiltins')){
        logger('Installing ravebuiltins', level = 'INFO')
        devtools::install_github(
          'beauchamplab/ravebuiltins', upgrade = 'never',
          dependencies = c("Depends", "Imports")
        )
        controller_env$module_table = arrange_modules(T,F,F)
      }
      ids = module_table$ID[module_table$Package == 'ravebuiltins']
      ids = ids[!ids %in% loaded_ids]
      for(mid in ids){
        if(module_table$Active[module_table$ID == mid]){
          m = .get_module('ravebuiltins', mid)
          cache_module(m, check = check)
        }
      }
      return(TRUE)
    }

    if(is.null(module_id)){
      if(!package_installed(package)){
        return(FALSE)
      }

      # package can be length > 1
      for(pkg in package){
        sel = module_table$Package == pkg & module_table$Active
        if(sum(sel)){
          ms = module_table$ID[sel]
        }
        ms = ms[!ms %in% loaded_ids]
        if(length(ms)){
          for(mid in ms){
            cache_module(.get_module(pkg, mid), check = check)
          }
        }
      }

      return(TRUE)

    }

    if(any(!module_id %in% loaded_ids)){
      module_id = module_id[!module_id %in% loaded_ids]
      sel = module_table$ID %in% module_id
      for(ii in which(sel)){
        pkg = module_table$Package[ii]
        mid = module_table$ID[ii]
        cache_module(.get_module(pkg, mid), check = check)
      }

    }

    return(TRUE)

  }




  # Data selector
  data_selector = shiny_data_selector('DATA_SELECTOR')

  adapter = new.env()
  adapter$data_selector_header = data_selector$header
  adapter$data_selector_server = data_selector$server
  adapter$get_option = function(opt, default = NULL){
    get0(opt, ifnotfound = default, envir = controller_env, inherits = FALSE)
  }
  adapter$get_module_ui = function(active_id = NULL){

    module_ids = module_table$ID[module_table$Package == 'ravebuiltins' & module_table$Active]
    active_id %?<-% module_ids[1]

    load_module(active_id)

    group_map = sapply(module_ids, function(mid){
      module_table[module_table$ID == mid, 'Group']
    }, USE.NAMES = F, simplify = T)
    groups = unique(group_map)

    if('______' %in% groups){
      groups = c(groups[groups != '______'], '______')
    }

    quos = list()

    for(group in groups){
      ms = as.list(unlist(module_list[[group]]))
      loaded_ids = sapply(ms, function(m){ m$module_id })
      names(ms) = loaded_ids

      sub_sel = vapply(module_ids, function(mid){
        isTRUE(module_table[module_table$ID == mid, 'Group'] == group)
      }, FALSE)

      ms = lapply(module_ids[sub_sel], function(mid){
        if(!is.null(ms[[mid]])){
          return(ms[[mid]])
        }else{
          module_table[module_table$ID == mid, ]
        }
      })
      ms = dropNulls(ms)

      if(group == '______' || length(ms) == 1){

        for(m in ms){
          if(is.data.frame(m)){
            m$label_name = m$Name
            m$module_id = m$ID
          #   quo = rlang::quo(shinydashboard::menuItem(
          #     text = !!m$Name,
          #     href = !!sprintf('?module_id=%s&nomodal=true', m$ID),
          #     newtab = FALSE
          #   ))
          # }else{
          }
            quo = rlang::quo({
              shinydashboard::menuItem(
                text = !!m$label_name,
                tabName = !!str_to_upper(m$module_id),
                selected = !!(m$module_id==active_id)
              )
            })
          # }
          quos[[length(quos) + 1]] = quo
        }

      }else{

        quo_items = lapply(ms, function(m){
          if(is.data.frame(m)){

            m$label_name = m$Name
            m$module_id = m$ID

          #   rlang::quo(shinydashboard::menuSubItem(
          #     text = !!m$Name,
          #     href = !!sprintf('?module_id=%s&nomodal=true', m$ID),
          #     newtab = FALSE
          #   ))
          # }else{
          }
            rlang::quo(shinydashboard::menuSubItem(
              text = !!m$label_name,
              tabName = !!str_to_upper(m$module_id),
              selected = !!(m$module_id==active_id)
            ))
          # }
        })
        quos[[length(quos) + 1]] = rlang::quo(shinydashboard::menuItem(
          text = !!group,
          !!!quo_items,
          startExpanded = !!(active_id %in% module_ids[sub_sel])
        ))

      }
    }
    quos

  }
  adapter$all_modules = function(){
    unlist(as.list(module_list), use.names = FALSE, recursive = TRUE)
  }
  adapter$module_ids = function(loaded_only = FALSE){
    if(loaded_only){
      sapply(unlist(as.list(module_list), use.names = FALSE, recursive = TRUE), function(m){
        m$module_id
      })
    }else{
      module_table$ID[module_table$Active]
    }
  }
  adapter$load_module = load_module

  ui_func = app_ui(adapter)

  server_func = app_server(adapter)

  shinyApp(ui = ui_func, server = server_func, options = list(launch.browser = launch.browser, ...))
  # shinyApp(ui = ui_func, server = server_func, options = list(launch.browser = T))
}

app_ui = function(adapter){

  ui_functions = list()

  # Case 1

  # ...

  # Case n (default, load ravebuiltins)

  ui_functions[['default']] = function(active_id = NULL, has_modal = TRUE){
    child_env = new.env()
    title = 'R Analysis and Visualization of ECoG/iEEG Data'
    version = local({
      ver = packageVersion('rave')
      sprintf('RAVE (%s)', paste(unlist(ver), collapse = '.'))
    })
    ui_quos = adapter$get_module_ui(active_id = active_id)

    quo = rlang::quo(dashboardPage(
      skin = adapter$get_option('theme', 'purple'), title = !!title,
      header = dashboardHeader(
        title = !!version, btn_text_right = 'RAM Usage',
        adapter$data_selector_header(),
        .list = local({
          if(!adapter$get_option('simplify_header', FALSE)){
            tagList(
              tags$li(class = 'user user-menu', actionLink('curr_subj_details_btn', '')),
              tags$li(class = 'user user-menu',actionLink('curr_subj_launch_suma', '')
              )
            )
          }
        })
      ),
      sidebar = shinydashboard::dashboardSidebar(
        disable = adapter$get_option('disable_sidebar', FALSE),
        shinydashboard::sidebarMenu(id = 'sidebar', !!!ui_quos)
      ),
      control = dashboardControl(
        uiOutput('mem_usage'),
        actionLink('control_panel_refresh', 'Click here to refresh!')
      ),
      body = shinydashboard::dashboardBody(
        do.call(
          shinydashboard::tabItems,
          args = local({
            re = lapply(adapter$module_ids(), function(module_id) {
              shinydashboard::tabItem(tabName = str_to_upper(module_id), uiOutput(str_c(module_id, '_UI')))
            })
            names(re) = NULL
            re
          })
        )
      ),
      initial_mask = !!local({
        if(has_modal){
          tagList(
            h2('R Analysis and Visualizations for Electrocorticography Data')
          )
        }else{
          NULL
        }
      })

    ))

    eval_dirty(quo, env = child_env)
  }




  function(parent_env = parent.frame()){
    # First, get query string

    qstr = parent_env$QUERY_STRING

    url_info = shiny::parseQueryString(qstr)
    print(url_info)

    # New load module?
    module_id = url_info$module_id
    if(length(module_id)){
      adapter$load_module(module_id, check = TRUE)
    }
    nomodal = url_info$nomodal
    nomodal %?<-% FALSE
    nomodal = nomodal == 'true'
    quo = ui_functions[['default']](active_id = module_id, has_modal = !nomodal)

  }


}


app_server = function(adapter){

  this_env = environment()

  test.mode = adapter$get_option('test.mode', FALSE)

  session_list = list()

  storage_keys = NULL


  #################################################################
  function(input, output, session){
    session_id = add_to_session(session)

    session$sendCustomMessage('rave_set_id', session_id);

    this_env$rave_ids = c(this_env$rave_ids, session_id)

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
    local_env = environment()
    modules = list()
    if(length(modules)){
      module_ids = str_to_upper(sapply(modules, function(m){m$module_id}))
      names(modules) = module_ids
    }

    observeEvent(async_timer(), {
      global_reactives$check_results = Sys.time()
    })

    .progress = progress(title = 'Loading Modules', max = length(unlist(modules)) + 1)
    .progress$inc('Initializing')
    shinirized_modules = sapply(unlist(modules), function(m){
      .progress$inc(m$label_name)
      shinirize(m, test.mode = test.mode)
    }, simplify = F, USE.NAMES = T)
    .progress$close()

    module_ui_functions = list()



    update_variable = function(module_id, variable_name = NULL, value = NULL, flush = T, ...){
      if(is.null(variable_name)){ return() }
      tryCatch({
        m = modules[[str_to_upper(module_id)]]
        if(is.null(m)){
          load_module(module_id)
        }
        e = m$get_or_new_exec_env()
        e$cache_input(inputId = variable_name, val = value, read_only = F)
        if(flush && module_id == str_to_upper(isolate(input$sidebar))){
          # When target module is not the same as current module, we need manually refresh current module
          e$input_update(input = list(), init = T)
        }
      }, error = function(e){
        logger('Cannot update variable ', variable_name, ' in module ', module_id, level = 'WARNING')
        logger(e, level = 'WARNING')
      })
    }

    load_module = function(module_id){
      showNotification(p('Loading module [', module_id, ']'), type = 'message', closeButton = FALSE, id = '..load_module', duration = NULL)

      loaded = FALSE
      mid = stringr::str_to_upper(module_id)
      if(!mid %in% names(local_env$modules)){
        loaded = adapter$load_module(module_id)
        ms = adapter$all_modules()
        sel = vapply(ms, function(m){ isTRUE(m$module_id == module_id) }, FALSE)

        if(any(sel)){
          local_env$modules[[mid]] = ms[[which(sel)]]

          quo = rlang::quo({
            m = shinirize(modules[[!!mid]], test.mode = test.mode)
            shinirized_modules[[!!mid]] = m
            callModule(m$server, id = m$id, session = session, global_reactives = global_reactives)

            local_env$module_ui_functions[[m$id]] = m$ui
            # output[[str_c(m$id, '_UI')]] <- renderUI(m$ui())
          })

          eval_dirty(quo, env = local_env)

          loaded = TRUE
        }


      }

      removeNotification(session = session, id = '..load_module')

      return(loaded)
    }

    # Switch to module
    observe({
      module_info = global_reactives$switch_module
      if(!is.null(module_info)){
        mid = module_info$module_id = str_to_upper(module_info$module_id)
        if(!mid %in% module_ids){
          load_module(mid)
        }
        if(mid %in% module_ids){
          logger('Switching to module - ', mid, level = 'INFO')
          do.call(update_variable, module_info)
          session$sendCustomMessage('rave_sidebar_switch', module_info)
        }
      }
    })



    observeEvent(input[['..keyboard_event..']], {
      global_reactives$keyboard_event = input[['..keyboard_event..']]
    })

    ##################################################################
    # Module to load data
    callModule(module = adapter$data_selector_server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)

    ##################################################################
    # loading modules
    # progress bar won't show title here, this is because shiny render detail information first and then
    # message


    observe({
      if(global_reactives$has_data){
        print(input$sidebar)
        global_reactives$execute_module = input$sidebar
        shinyjs::hide(id = '__rave__mask__', anim = F)
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
      local_env$module_ui_functions[[m$id]] = m$ui
      # output[[str_c(m$id, '_UI')]] <- renderUI(m$ui())
    })

    lapply(adapter$module_ids(), function(module_id){
      output[[paste0(module_id, '_UI')]] <- renderUI({

        if(!is.function(module_ui_functions[[module_id]])){
          load_module(module_id)
        }
        module_ui_functions[[module_id]]()
      })
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
        shinydashboard::tabBox(
          width = 12,
          tabPanel(
            title = '3D Viewer',
            threejsBrainOutput('curr_subj_3d_viewer')
          ),
          tabPanel(
            title = 'Electrode Table',
            dataTableOutput('curr_subj_elec_table')
          )
        )

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

    output$curr_subj_3d_viewer <- renderBrain({
      btn = input$curr_subj_details_btn
      if(global_reactives$has_data && check_data_repo('subject')){
        data_repo = getDefaultDataRepository()
        subject = data_repo[['subject']]
        brain = rave_brain2(surfaces = c('pial', 'white', 'smoothwm'), multiple_subject = F)
        brain$add_subject(subject = subject)
        brain$load_electrodes(subject)
        brain$load_surfaces(subject)
        brain$view()
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
      usage = as.numeric(unlist(usage))

      perc = usage / max(usage, na.rm = TRUE)


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
    # Browser information



    observeEvent(input[['__local_storage_inputs__']], {
      all_inputs = input[['__local_storage_inputs__']]

      if(!is.list(all_inputs)){
        return()
      }

      module_ids = names(all_inputs)
      module_ids = module_ids[stringr::str_to_upper(module_ids) %in% names(modules)]

      sig = add_to_session(session)

      sapply(module_ids, function(mid){
        changed = FALSE
        m = modules[[ stringr::str_to_upper(mid) ]]
        execenv = m$get_or_new_exec_env(session = session)
        try({
          inputs = eval(parse(text = all_inputs[[mid]]))
          print(inputs)

          for(inputId in names(inputs)){

            key = list( type = '.rave-inputs-Dipterix', inputId = inputId, sig = session_id )
            logger('Updating ', inputId, ' - ', session_id)

            global = execenv$is_global(inputId)
            new_v = inputs[[inputId]]
            old_v = execenv$cache( key, new_v, global = global, replace = FALSE, persist = FALSE)
            execenv$cache( key, new_v, global = global, replace = TRUE, persist = TRUE)

            if(!identical(new_v, old_v)){
              changed = TRUE
            }

          }
        }, silent = TRUE)

        changed
      }) ->
        changed

      if(isTRUE(all_inputs[['__changed_by']] == sig)){
        return()
      }

      if(length(changed) && any(changed)){
        logger('Sync sessions...')
        # execenv$reload
        global_reactives$force_refresh_all = Sys.time()
        global_reactives$has_data = Sys.time()
      }

    })




    #################################################################
    # on session ended, clean memory


    # test
    if(length(this_env$rave_ids) == 2){
      add_to_session(session, 'rave_linked_by', this_env$rave_ids)
    }

    if(!test.mode){
      session$onSessionEnded(function() {
        logger('Clean up environment.')

        this_env$rave_ids = this_env$rave_ids[!this_env$rave_ids == session_id]
        lapply(unlist(modules), function(x){
          x$clean(session_id = session_id)
        })
        gc()
      })
    }else{
      session$onSessionEnded(function() {

        this_env$rave_ids = this_env$rave_ids[!this_env$rave_ids == session_id]
        gc()
      })

    }


  }

}
