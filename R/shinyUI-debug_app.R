#' Initialize main application for debugging purpose
#' @param modules which modules to show. See \code{\link[rave]{load_modules}}
#' @param active_module which module to focus at start up (use module ID)
#' @param launch.browser launch browsers, default is on
#' @param theme color theme for the website
#' @param disable_sidebar hide sidebar at startup?
#' @param simplify_header hide header at startup?
#' @param data_repo internally used
#' @param ... other parameters like \code{test.mode} for module debugging
#' @export
init_app <- function(modules = NULL, active_module = NULL, launch.browser = TRUE,
                     theme = "red", disable_sidebar = FALSE, simplify_header = FALSE, 
                     ..., data_repo = getDefaultDataRepository()){
  options(shiny.maxRequestSize=1024^3)
  # register_compoundInput()
  rave_setup_workers()
  
  tryCatch({
    rave_prepare()
  }, error = function(e){})
  
  test.mode = list(...)[['test.mode']]
  if(is.null(test.mode)) {
    if(is.null(modules)){
      test.mode = isTRUE(rave_options('test_mode'))
    }else{
      test.mode = FALSE
    }
  }
  if(!length(modules)){
    modules = load_modules()
  }
  
  if(!is.list(modules)){
    modules = list("______" = list(modules))
  }
  
  data_selector = shiny_data_selector('DATA_SELECTOR', data_env = data_repo)
  ui = dashboardPage(
    skin = theme,
    title = 'R Analysis and Visualization of ECoG/iEEG Data',
    header = dashboardHeader(
      title = local({
        ver = utils::packageVersion('rave')
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
            # tags$li(
            #   class = 'user user-menu',
            #   actionLink('curr_subj_launch_suma', '')
            # ),
            tags$li(
              class = 'user user-menu',
              actionLink('rave_reset', 'Reset State')
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
                    mid_up = stringr::str_to_upper(smd$module_id)
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
              mid_up = stringr::str_to_upper(smd$module_id)
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
            shinydashboard::tabItem(tabName = stringr::str_to_upper(m$module_id),
                                    uiOutput(stringr::str_c(m$module_id, '_UI')))
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
    if( test.mode ){
      # assign('..session', session, envir = globalenv())
      reg.finalizer(environment(), function(e) message("Finalizer: ", session$token))
    }
    
    
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
    
    
    # observeEvent(input[['__local_storage_message__']], {
    #   message = input[['__local_storage_message__']]
    #   if(length(message$token) && message$token == add_to_session(session)){
    #     # This instance needs to handle the event
    #
    #     # check message type
    #     # if(message$message_type == 'threeBrain')
    #
    #     # manually set value
    #     impl = .subset2( session$input, 'impl' )
    #     impl$set(message$callback_id, message$content)
    #   }
    # })
    
    
    # unlist(modules) will flatten modules but it's still a list
    module_ids = stringr::str_to_upper(sapply(unlist(modules), function(m){m$module_id}))
    session$userData$rave_module = function(module_id){
      module_id = stringr::str_to_upper(module_id)
      if(module_id %in% module_ids){
        for(m in unlist(modules)){
          if(stringr::str_to_upper(m$module_id) == module_id){
            return(m)
          }
        }
      }
      return(NULL)
    }
    update_variable = function(module_id, variable_name = NULL, value = NULL, 
                               flush = TRUE, ...){
      
      if(is.null(variable_name)){ return() }
      sidebar_id = stringr::str_to_upper(shiny::isolate(input$sidebar))
      tryCatch({
        module_id = stringr::str_to_upper(module_id)
        m = unlist(modules)[module_ids %in% module_id]
        
        if(length(m) == 1){
          
          m = m[[1]]
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
        }
      }, error = function(e){
        catgl('Cannot update variable ', variable_name, ' in module ', module_id, level = 'WARNING')
        catgl(e, level = 'WARNING')
      })
    }
    
    # Switch to module
    observe({
      module_info = global_reactives$switch_module
      if(!is.null(module_info)){
        mid = module_info$module_id = stringr::str_to_upper(module_info$module_id)
        if(mid %in% module_ids){
          catgl('Switching to module - ', mid, level = 'INFO')
          do.call(update_variable, module_info)
          session$sendCustomMessage('rave_sidebar_switch', module_info)
        }
      }
    })
    
    observeEvent(input[['..keyboard_event..']], {
      global_reactives$keyboard_event = input[['..keyboard_event..']]
    })
    observeEvent(input[['..client_size..']], {
      global_reactives$client_size = input[['..client_size..']]
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
      shinirize(m, test.mode = test.mode, data_env = data_repo)
    })
    .progress$close()
    
    observe({
      if(global_reactives$has_data){
        current_module_id = input$sidebar
        global_reactives$execute_module = current_module_id
        session$userData$rave_current_module_id = current_module_id
        shinyjs::hide(id = '__rave__mask__', anim = TRUE, animType = 'slide')
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
      output[[stringr::str_c(m$id, '_UI')]] <- renderUI(m$ui())
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
        shinydashboard::tabBox(
          width = 12,
          tabPanel(
            title = '3D Viewer',
            threeBrain::threejsBrainOutput('curr_subj_3d_viewer')
          ),
          tabPanel(
            title = 'Electrode Table',
            dataTableOutput('curr_subj_elec_table')
          )
        )
        
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
    
    output$curr_subj_elec_table <- renderDataTable({
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
    })
    
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
      
      brain$plot(control_panel = FALSE, side_canvas = FALSE, volumes = FALSE)
    })
    
    observeEvent(input$curr_subj_launch_suma, {
      # launch suma
      if(check_data_repo('subject')){
        subject = data_repo[['subject']]
        suma_dir = subject$dirs$suma_dir
        launch_suma(
          root_dir = suma_dir
        )
      }
    })
    observeEvent(input$rave_reset, {
      shiny::showModal(
        shiny::modalDialog(
          title = 'Confirmation',
          p('This action will reset all input parameters. Click "Confirm" to proceed.'),
          size = 's', footer = tagList(
            tags$a(href = "#", class = "btn btn-default rave-restart-btn", 
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
              txt_size = as.character(dipsaus::to_ram_size(us))
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
        catgl('Clean up environment.')
        
        lapply(unlist(modules), function(x){
          x$clean(session_id = session_id)
        })
        gc()
        # unlink(session_dir, recursive = T, force = T)
      })
    }else{
      session$onSessionEnded(function() {
        # unlink(session_dir, recursive = T, force = T)
      })
    }
    
    set_rave_theme()
    
  }
  shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))
}

