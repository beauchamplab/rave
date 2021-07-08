app_controller <- function(
  modules = NULL, active_module = NULL, launch.browser = TRUE,
  theme = "purple", disable_sidebar = FALSE, simplify_header = FALSE,
  token = NULL, data_repo = getDefaultDataRepository(), ...
){
  options(shiny.maxRequestSize=1024^3)

  # Register customized inputs
  # register_compoundInput()

  # Detach rave data environment
  tryCatch({ rave_prepare() }, error = function(e){})

  # Get options of whether this is a test mode
  test.mode = list(...)[['test.mode']]; test.mode = isTRUE(test.mode)

  # Get package modules
  controller_env = environment()
  loaded_modules = list()
  module_table = arrange_modules(refresh = FALSE, reset = FALSE, quiet = TRUE)
  dup = duplicated(module_table$Name)
  if(any(dup)){
    module_table$Name[dup] = sprintf('%s (%s)', module_table$Name[dup], module_table$Package[dup])
  }
  rave_pkgs = unique(module_table$Package)

  # define method to load modules
  load_module = function(module_id, notification = FALSE){
    if('ModuleEnvir' %in% class(module_id)){
      # this is a module instance, no need to load new
      controller_env$loaded_modules[[stringr::str_to_upper(module_id$module_id)]] = module_id
      return(module_id)
    }
    module_id = stringr::str_to_upper(module_id)
    # find module
    if(length(loaded_modules[[module_id]])){
      return(loaded_modules[[module_id]])
    }

    # Find package name
    sel = stringr::str_to_upper(module_table$ID) == module_id
    if(!any(sel)){
      return(NULL)
    }

    pkg = module_table$Package[sel][[1]]

    # load the entire package!
    if(notification){
      shiny::showNotification(p('Loading module - ', module_id), type = 'message', duration = NULL, id = 'load_module_notif')
      on.exit({
        shiny::removeNotification(id = 'load_module_notif')
      })
    }
    catgl('Loading module - ', module_id)
    ms = detect_modules(packages = pkg, as_module = TRUE)
    ms = unlist(ms)

    # set module
    for(m in ms){
      if(is.null(controller_env$loaded_modules[[stringr::str_to_upper(m$module_id)]])){
        controller_env$loaded_modules[[stringr::str_to_upper(m$module_id)]] = m
      }
    }

    # If loaded, then this will return module instance, otherwise, returns will be NULL
    return(loaded_modules[[module_id]])
  }


  if(length(modules)){
    if(!is.list(modules)){
      modules = unlist(list(modules))
    }
    for(mid in modules){
      load_module(mid)
    }
  }

  # generate module list (for sidebar)
  groups = unique(module_table$Group)
  groups[groups == ''] = '______'
  module_list = sapply(groups, function(group_name){
    sel = module_table$Group == group_name
    re = lapply(module_table$ID[sel], function(mid){
      idx = which(module_table$ID == mid)[1]
      active = module_table$Active[idx]
      if(!active){
        return(NULL)
      }
      list(
        module_id = mid,
        module_label = module_table$Name[idx],
        active = module_table$Active[idx],
        init = function(){
          load_module(mid)
        }
      )
    })
    dipsaus::drop_nulls(re)
  })

  # Data selector
  data_selector = shiny_data_selector('DATA_SELECTOR', data_env = data_repo)
  adapter = dipsaus::fastmap2()
  .subset2(adapter, 'mset')(
    data_selector_header = data_selector$header,
    data_selector_server = data_selector$server,
    launch_selector = data_selector$launch,
    get_option = function(opt, default = NULL){
      get0(opt, ifnotfound = default, envir = controller_env, inherits = FALSE)
    },
    get_module_ui = function(active_id = NULL){
      
      module_ids = module_table$ID[module_table$Active]
      active_id %?<-% module_ids[1]
      
      load_module(active_id)
      
      groups = names(module_list)
      
      # Generate quos
      quos = list()
      for(g in groups[groups != '______']){
        modules = module_list[[g]]
        if(length(modules)){
          quo_items = lapply(modules, function(m){
            rlang::quo(shinydashboard::menuSubItem(
              text = !!m$module_label,
              tabName = !!stringr::str_to_upper(m$module_id),
              selected = !!(m$module_id==active_id)
            ))
          })
          
          quos[[length(quos) + 1]] = rlang::quo(shinydashboard::menuItem(
            text = !!g,
            !!!quo_items,
            startExpanded = !!(active_id %in% sapply(modules, '[[', 'module_id'))
          ))
        }
      }
      
      for(m in module_list[['______']]){
        quos[[length(quos) + 1]] = rlang::quo({
          shinydashboard::menuItem(
            text = !!m$module_label,
            tabName = !!stringr::str_to_upper(m$module_id),
            selected = !!(m$module_id==active_id)
          )
        })
      }
      quos
    },
    module_ids = function(loaded_only = FALSE){
      if(loaded_only){
        sapply(loaded_modules, '[[', 'module_id')
      }else{
        module_table$ID[module_table$Active]
      }
    },
    load_module = load_module
  )
  
  ui_func = app_ui(adapter, token = token)

  instance_id = paste(sample(c(letters, LETTERS, 0:9), 22), collapse = '')
  server_func = app_server(adapter, instance_id, token = token, data_repo = data_repo)

  reg.finalizer(
    controller_env,
    function(e){
      dir_path = file.path(subject_cache_dir(), .subset2(e, 'instance_id'))
      if(dir.exists(dir_path)){
        unlink(dir_path, recursive = TRUE, FALSE)
      }
    },
    onexit = TRUE
  )

  RaveFinalizer$new(function(){
    dir_path = file.path(subject_cache_dir(), instance_id)
    if(dir.exists(dir_path)){
      unlink(dir_path, recursive = TRUE, force = FALSE)
    }
  })

  shinyApp(ui = ui_func, server = server_func, options = list(launch.browser = launch.browser, ...))
  # shinyApp(ui = ui_func, server = server_func, options = list(launch.browser = T))
}

app_ui <- function(adapter, token = NULL){

  ui_functions = dipsaus::fastmap2(
    # Case n (default, load ravebuiltins)
    missing_default = function(active_id = NULL, has_modal = TRUE){
      title = 'RAVE'
      version = utils::packageVersion('rave')
      version = sprintf('RAVE (%s)', paste(unlist(version), collapse = '.'))
      simplify_header = adapter$get_option('simplify_header', FALSE)
      ui_quos = adapter$get_module_ui(active_id = active_id)
      
      quo = as_call2(
        quote(dashboardPage),
        skin = adapter$get_option('theme', 'purple'),
        title = title,
        header = as_call2(
          quote(dashboardHeader),
          title = version,
          btn_text_right = 'RAM Usage',
          quote(adapter$data_selector_header()),
          .list = if(simplify_header) NULL else list(
            .list = quote(
              tagList(
                tags$li(class = 'user user-menu', 
                        actionLink('curr_subj_details_btn', '')),
                tags$li(class = 'user user-menu',
                        actionLink('rave_reset', 'Reset GUI'))
              )
            )
          )
        ),
        sidebar = as_call2(
          quote(shinydashboard::dashboardSidebar),
          disable = adapter$get_option('disable_sidebar', FALSE),
          as_call2(
            quote(shinydashboard::sidebarMenu),
            id = 'sidebar', 
            .list = lapply(ui_quos, rlang::quo_squash)
          )
        ),
        control = quote(dashboardControl(
          # uiOutput('mem_usage'),
          # actionLink('control_panel_refresh', 'Click here to refresh!')
        )),
        body = as_call2(
          quote(shinydashboard::dashboardBody),
          as_call2(
            quote(shinydashboard::tabItems),
            .list = unname(lapply(adapter$module_ids(), function(module_id) {
              # module_id = stringr::str_to_upper(module_id)
              as_call2(
                quote(shinydashboard::tabItem),
                tabName = stringr::str_to_upper(module_id),
                as_call2(
                  uiOutput,
                  stringr::str_c(module_id, '_UI')
                )
              )
            }))
          )
        ),
        initial_mask = if (has_modal) quote(h2('R Analysis and Visualizations for iEEG/ECoG Data')) else NULL
      )
      
      dipsaus::eval_dirty(quo)
    }
    
  )
  
  ui_functions[['404']] = function(...){
    # 404
    '404'
  }
  
  ui_functions[['3dviewer']] = function(global_id, session_id = NULL){
    shiny::fillPage(
      title = 'RAVE 3D Viewer',
      padding = 0,
      threeBrain::threejsBrainOutput(global_id, width = '100vw', height = '100vh')
    )
  }


  function(req){ #, parent_env = parent.frame()
    # First, get query string

    qstr = req$QUERY_STRING

    url_info = shiny::parseQueryString(qstr)

    # If token should be provided, check token
    # ?token=...
    if(!is.null(token)){
      if(!length(url_info$token) || !any(url_info$token %in% token)){
        # Return 404
        return(ui_functions[['404']]())
      }
    }

    # ?type=3dviewer&globalId=...&sessionId=...
    if(length(url_info$type) == 1 && url_info$type == '3dviewer'){
      return(ui_functions[['3dviewer']](url_info$globalId, url_info$sessionId))
    }

    # Default, load main app
    lapply(url_info$module_id, adapter$load_module)
    nomodal = url_info$nomodal
    nomodal %?<-% FALSE
    nomodal = nomodal == 'true'
    quo = ui_functions$default(active_id = url_info$module_id, has_modal = !nomodal)

  }


}


app_server_404 <- function(...){

}
app_server_3dviewer <- function(input, output, session, master_session, viewer_id){
  master_root = master_session$rootScope()
  
  proxy = threeBrain::brain_proxy(viewer_id, session)
  master_proxy = threeBrain::brain_proxy(viewer_id, master_root)
  
  output[[viewer_id]] <- threeBrain::renderBrain({
    f = master_session$userData$cross_session_funcs[[viewer_id]]
    if(length(formals(f))){
      f( proxy )
    }else{
      f()
    }
  })
  
  # observeEvent(master_proxy$background, {
  #   value = master_proxy$background
  #   proxy$set_background(value)
  # })
  # 
  # observeEvent(master_proxy$main_camera, {
  #   value = master_proxy$main_camera
  #   if(!length(value)){ return() }
  #   if(length(value$position) && length(value$up)){
  #     proxy$set_camera(position = as.numeric(value$position), up = as.numeric(value$up))
  #   }
  #   if(length(value$zoom)){
  #     proxy$set_zoom_level(value$zoom)
  #   }
  # })
  observeEvent(master_proxy$sync, {
    ctrl = master_proxy$get_controllers()
    main_camera = master_proxy$main_camera
    if(length(main_camera)){
      if(length(main_camera$position) && length(main_camera$up)){
        proxy$set_camera(position = as.numeric(main_camera$position), up = as.numeric(main_camera$up))
      }
      if(length(main_camera$zoom) == 1){
        proxy$set_zoom_level(main_camera$zoom)
      }
    }
    proxy$set_controllers( ctrl )
  })
  
  observeEvent(proxy$sync, {
    ctrl = proxy$get_controllers()
    main_camera = proxy$main_camera
    if(length(main_camera)){
      if(length(main_camera$position) && length(main_camera$up)){
        master_proxy$set_camera(position = as.numeric(main_camera$position), up = as.numeric(main_camera$up))
      }
      if(length(main_camera$zoom) == 1){
        master_proxy$set_zoom_level(main_camera$zoom)
      }
    }
    master_proxy$set_controllers( ctrl )
  })


  # Observe clicks
  # click_id = paste0(viewer_id, '_mouse_clicked')
  dblclick_id = paste0(viewer_id, '_mouse_dblclicked')

  observeEvent(input[[dblclick_id]], {
    mouse_event = input[[dblclick_id]]

    # Show information when double clicked
    try({
      if(mouse_event$action == 'dblclick'){
        shiny::showNotification(p('Sent back to master session.'), type = 'message',
                                id = paste0(dblclick_id, '_sent'), duration = 1)
      }
    }, silent = TRUE)

    # send whatever message from 3D viewer back to master_session
    master_session$sendCustomMessage('rave_asis', list(
      inputId = dblclick_id,
      value = mouse_event
    ))
  })
  

}


app_server <- function(adapter, instance_id, token = NULL, data_repo = getDefaultDataRepository()){

  this_env = environment()

  test.mode = adapter$get_option('test.mode', FALSE)

  session_list = list()
  rave_ids = NULL

  storage_keys = NULL


  #################################################################
  function(input, output, session){
    if( test.mode ){
      # assign('..session', session, envir = globalenv())
      
    }
    
    session_id = add_to_session(session)
    add_to_session(session, key = 'rave_instance', val = instance_id, override = TRUE)
    add_to_session(session, key = 'token', val = token, override = TRUE)

    session$sendCustomMessage('rave_set_id', session_id);
    session$userData$cross_session_funcs = list()

    # 404 Page
    query_str = shiny::isolate(getQueryString(session))

    if(!is.null(token)){
      if(!length(query_str$token) || !any(query_str$token %in% token)){
        # Return 404
        return(app_server_404(input, output, session))
      }
    }

    # 3D viewer
    if( length(query_str$type) == 1 && query_str$type == '3dviewer' ){
      ## TODO: handle exceptions
      return(app_server_3dviewer(
        input, output, session,
        this_env$session_list[[query_str$sessionId]], query_str$globalId
      ))
    }

    this_env$rave_ids = c(this_env$rave_ids, session_id)
    this_env$session_list[[session_id]] = session

    # Global variable, timer etc.
    async_timer = reactiveTimer(5000)
    # input_timer = reactiveTimer(rave_options('delay_input') / 2)
    global_reactives = reactiveValues(
      check_results = NULL,
      check_inputs = NULL,
      execute_module = '',
      has_data = FALSE,
      switch_module = NULL,
      timer_count = 0,
      launch_selector = NULL
    )

    local_data = reactiveValues(
      mem_usage = NULL
    )
    observeEvent(async_timer(), {
      global_reactives$check_results = Sys.time()
    })

    local_env = environment()
    modules = list()
    shinirized_modules = list()
    module_ui_functions = list()
    # if(length(modules)){
    #   module_ids = stringr::str_to_upper(sapply(modules, function(m){m$module_id}))
    #   names(modules) = module_ids
    # }
    # .progress = progress(title = 'Loading Modules', max = length(unlist(modules)) + 1)
    # .progress$inc('Initializing')
    # shinirized_modules = sapply(unlist(modules), function(m){
    #   .progress$inc(m$label_name)
    #   shinirize(m, test.mode = test.mode)
    # }, simplify = F, USE.NAMES = T)
    # .progress$close()





    update_variable = function(module_id, variable_name = NULL, value = NULL, flush = TRUE, ...){
      if(is.null(variable_name)){ return() }
      sidebar_id = stringr::str_to_upper(shiny::isolate(input$sidebar))
      tryCatch({
        m = modules[[stringr::str_to_upper(module_id)]]
        if(is.null(m)){
          load_module(module_id)
        }
        e = m$get_or_new_exec_env()
        
        local({
          rave_context(senv = e)
          
          cache_input(inputId = variable_name, val = value, 
                      read_only = FALSE)
          
        })
        
        if(flush && module_id == sidebar_id){
          # When target module is not the same as current module, we need manually refresh current module
          e$input_update(input = list(), init = TRUE)
        }
      }, error = function(e){
        catgl('Cannot update variable ', variable_name, ' in module ', module_id, level = 'WARNING')
        catgl(e, level = 'WARNING')
      })
    }

    load_module = function(module_id){
      module_id = stringr::str_to_upper(module_id)

      loaded = TRUE

      if(!module_id %in% names(local_env$modules)){
        loaded = FALSE
        quo = rlang::quo({
          modules[[!!module_id]] = adapter$load_module(module_id = !!module_id, notification = TRUE)
          m = shinirize(modules[[!!module_id]], test.mode = test.mode, data_env = data_repo)
          shinirized_modules[[!!module_id]] = m
          callModule(m$server, id = m$id, session = session, global_reactives = global_reactives)
          module_ui_functions[[m$id]] = m$ui
          rm(m)
        })

        # Try catch?
        safe_wrap_expr({
          dipsaus::eval_dirty(quo, env = local_env)
          loaded = TRUE
        })
        
      }

      return(loaded)
    }
    session$userData$rave_module = load_module

    # Switch to module
    observe({
      module_info = global_reactives$switch_module

      if(!is.null(module_info)){
        module_ids = adapter$module_ids()
        mid = module_info$module_id = stringr::str_to_upper(module_info$module_id)
        # print(mid)
        load_module(mid)
        catgl('Switching to module - ', mid, level = 'INFO')
        do.call(update_variable, module_info)
        session$sendCustomMessage('rave_sidebar_switch', module_info)
      }

      # if(open_selector){
      #   # Open selector!
      #   adapter$launch_selector()
      # }
    })



    observeEvent(input[['..keyboard_event..']], {
      global_reactives$keyboard_event = input[['..keyboard_event..']]
    })
    observeEvent(input[['..client_size..']], {
      global_reactives$client_size = input[['..client_size..']]
    })

    ##################################################################
    # Module to load data
    callModule(module = adapter$data_selector_server, id = 'DATA_SELECTOR', session = session, global_reactives = global_reactives)
    #            , clear_cache = function(){
    #   lapply(shinirized_modules, function(m){
    #     try({
    #       m$clean()
    #     })
    #   })
    # })

    ##################################################################
    # loading modules
    # progress bar won't show title here, this is because shiny render detail information first and then
    # message

    observeEvent(global_reactives$launch_selector, {
      adapter$launch_selector()
    })

    observe({
      if(global_reactives$has_data){
        # print(input$sidebar)
        current_module_id = input$sidebar
        global_reactives$execute_module = current_module_id
        session$userData$rave_current_module_id = current_module_id
        shinyjs::hide(id = '__rave__mask__', anim = FALSE)
        shinyjs::removeClass(selector = 'body', class = "rave-noscroll")
      }else{
        shinyjs::show(id = '__rave__mask__', anim = TRUE, animType = 'slide')
        shinyjs::addClass(selector = 'body', class = "rave-noscroll")
      }
    })

    lapply(shinirized_modules, function(m){
      callModule(m$server, id = m$id, session = session, global_reactives = global_reactives)
    })
    lapply(shinirized_modules, function(m){
      local_env$module_ui_functions[[m$id]] = m$ui
      # output[[stringr::str_c(m$id, '_UI')]] <- renderUI(m$ui())
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
        subject_id = data_repo$subject$id
        epoch_name = data_repo$preload_info$epoch_name
        reference_name = data_repo$preload_info$reference_name

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
        easyClose = TRUE,
        size = 'l',
        tags$style('.modal-lg { min-width: 80vw; }'),
        h4('Electrode Table'),
        shiny::tableOutput('curr_subj_elec_table'),
        br(),
        h4('3D Viewer'),
        threeBrain::threejsBrainOutput('curr_subj_3d_viewer')
      )
    }

    observeEvent(input$curr_subj_details_btn, {
      subject = data_repo[['subject']]
      electrodes = data_repo$preload_info$electrodes
      if(!is.null(subject) && length(electrodes)){
        showModal(
          subject_modal(subject = subject, current_electrodes = electrodes)
        )
      }
    })

    output$curr_subj_elec_table <- shiny::renderTable({
      btn = input$curr_subj_details_btn
      if(global_reactives$has_data && check_data_repo('subject')){
        subject = data_repo[['subject']]
        tbl = subject$electrodes
        cols = names(tbl)
        cols = cols[!cols %in% c('Coord_x', 'Coord_y', 'Coord_z')]
        return(tbl[, cols])
      }else{
        return(NULL)
      }
    }, striped = TRUE, spacing = 'xs', width = '100%', rownames = FALSE, digits = 2)

    output$curr_subj_3d_viewer <- threeBrain::renderBrain({
      btn = input$curr_subj_details_btn
      if( ! ( global_reactives$has_data && check_data_repo('subject') ) ){
        return(NULL)
      }
      
      subject = get0('subject', envir = data_repo, ifnotfound = NULL)
      if( is.null(subject) ){
        return(NULL)
      }
      
      brain = rave_brain2(subject = subject)
      if( is.null(brain) ){
        return(NULL)
      }
      
      theme = get_rave_theme()$themes[[1]]
      background = ifelse(theme == 'dark', '#1E1E1E', '#FFFFFF')
      
      brain$plot(control_display = TRUE, side_display = FALSE, background = background)
    })

    observeEvent(input$curr_subj_launch_suma, {
      # launch suma
      if(check_data_repo('subject')){
        subject = get0('subject', envir = data_repo)
        suma_dir = subject$dirs$suma_dir
        launch_suma(
          root_dir = suma_dir
        )
      }
    })
    observeEvent(input$rave_reset, {
      shiny::showModal(
        shiny::modalDialog(
          title = 'Reset RAVE GUI',
          p('This action will reset GUI and input parameters for all modules. (The data will NOT reset)',br(),
            'Click "Confirm" to proceed.'),
          size = 's', footer = tagList(
            tags$a(href = "#", class = "btn btn-default rave-app-btn rave-restart-btn", 
                   shiny::icon('refresh'), 'Confirm'),
            shiny::modalButton('Cancel')
          )
        )
      )
    })

    # observe({
    #   input$control_panel_refresh
    #   local_data$mem_usage = get_mem_usage(modules)
    # })

    # output$mem_usage <- renderUI({
    #   mem_usage = local_data$mem_usage
    #   if(is.null(mem_usage)){
    #     return()
    #   }
    # 
    #   name = c(
    #     'Total Usage',
    #     'Shared Data Usage',
    #     sapply(mem_usage$module_usage, '[[', 'Name')
    #   )
    # 
    #   usage = c(
    #     mem_usage$total_mem,
    #     mem_usage$data_usage + mem_usage$other_usage,
    #     sapply(mem_usage$module_usage, '[[', 'usage')
    #   )
    #   usage = as.numeric(unlist(usage))
    # 
    #   perc = usage / max(usage, na.rm = TRUE)
    # 
    # 
    #   tagList(
    #     h3(class = 'control-sidebar-heading', 'Memory Usage'),
    #     tags$ul(
    #       class="control-sidebar-menu",
    #       style = 'padding:15px;',
    #       tagList(
    #         lapply(seq_along(name), function(ii){
    #           nm = name[[ii]]
    #           us = usage[[ii]]
    #           pc = perc[[ii]]
    #           status = switch (nm,
    #                            'Total Usage' = {
    #                              max_ram = rave_options('max_mem') * 1000^3
    #                              pc_total = us / max_ram
    #                              ind = (pc_total < 0.75) + (pc_total < 0.9) + 1
    #                              c('danger', 'warning', 'success')[ind]
    #                            },
    #                            'Shared Data Usage' = 'primary',
    #                            'Misc & Others Sessions' = 'primary',
    #                            {
    #                              ind = (pc < 0.5) + 1
    #                              c('warning', 'primary')[ind]
    #                            }
    #           )
    #           txt_size = as.character(dipsaus::to_ram_size(us))
    #           txt_perc = sprintf('%d%%', as.integer(pc*100))
    #           if(stringr::str_length(nm) > 22){
    #             nm_alt = paste0(stringr::str_sub(nm, end = 19), '...')
    #           }else{
    #             nm_alt = nm
    #           }
    # 
    #           tags$li(
    #             h4(class = 'control-sidebar-subheading', nm_alt, span(class=sprintf('label label-%s pull-right', status), txt_size)),
    #             div(class = 'progress progress-xxs',
    #                 div(class = sprintf("progress-bar progress-bar-%s", status), style = sprintf('width: %s', txt_perc)))
    #           )
    #         })
    #       )
    #     ),
    #     div(style = 'margin-bottom:20px')
    #   )
    # 
    # })


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
          # print(inputs)

          for(inputId in names(inputs)){
            
            catgl('Updating ', inputId, ' - ', session_id)
            global = execenv$is_global(inputId)

            new_v = inputs[[inputId]]
            
            old_v = cache_input(inputId = inputId, val = new_v, read_only = TRUE)
            cache_input(inputId = inputId, val = new_v, read_only = FALSE)
            
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
        catgl('Sync sessions...')
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
        catgl('Clean up environment.')

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
    
    # Set theme
    set_rave_theme()


  }

}



#' @name start_rave
#' @title Start RAVE main application
#' @param modules character vector, modules modules to load before starting application.
#' @param active_module character, which module to show as default.
#' @param launch.browser logical, whether to launch browser.
#' @param theme character, color theme, default is \code{'purple'}.
#' @param disable_sidebar logical, whether to hide sidebar.
#' @param simplify_header logical, whether to show simplified header.
#' @param data_repo internally used.
#' @param token character vector, default is \code{NULL}. If specified, then
#' a \code{?token=...} is needed in url to access to the application.
#' @param ... other parameters. See details.
#'
#' @export
start_rave <- app_controller






#' @name rave-tabs
#' @title Open/Close a tab in RAVE main application
#' @param module_id character, module ID
#' @param tabname character, tab box title
#' @export
close_tab <- function(module_id, tabname){
  session = shiny::getDefaultReactiveDomain()
  if(!is.null(session)){
    session$sendCustomMessage('rave_close_tab', list(
      module_id = module_id,
      title = tabname
    ))
  }
}


#' @rdname rave-tabs
#' @export
open_tab <- function(module_id, tabname){
  session = shiny::getDefaultReactiveDomain()
  if(!is.null(session)){
    session$sendCustomMessage('rave_open_tab', list(
      module_id = module_id,
      title = tabname
    ))
  }
}




