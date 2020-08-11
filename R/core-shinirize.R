#' Convert module to objects used in shiny
#' @param module \code{\link[rave]{ModuleEnvir}} object
#' @param session shiny session, default is current shiny session
#' @param test.mode passed by \code{\link[rave]{start_rave}} or
#' \code{\link[rave]{init_app}}
#' @param data_env internally used
shinirize <- function(module, session = getDefaultReactiveDomain(), test.mode = TRUE,
                      data_env = getDefaultDataRepository()){
  # assign variables
  MODULE_ID = module$module_id
  MODULE_LABEL = module$label_name
  
  logger = function(...){
    catgl('[', MODULE_ID, '] ', ..., strftime(Sys.time(), ' - %M:%S', usetz = FALSE))
  }

  # Runtime environment
  execenv = module$get_or_new_exec_env(session = session)
  execenv$register_context('rave_running')
  rave_context(senv = execenv)
  
  # TODO: remove debug
  # rave_context(senv = execenv, tenv = globalenv())
  
  # load script
  module$load_script(session = session)


  # ui
  list(
    id = MODULE_ID,
    ui = function(){
      title = sprintf('RAVE - %s', MODULE_LABEL)
      module$render_ui(session = session)
    },
    server = function(input, output, session, global_reactives, env = new.env()){

      ##### #####
      # This is a really frustrating process, like flushing a toilet, you don't want to wait too long before flushing, nor miss a single one of them - Zhengjia
      # So I decided to write down workflows/conditions down here. I swear that I always compile all the process in my mind when writing codes, until now.
      # To start, here's the normal workflow:
      #
      # case 1: no data imported
      # rave_inputs(),
      #
      # case 2: data imported (has_data = T), but not inited (module deactivated)
      # rave_updates(), and set as inited
      # However, some rave_updates might be annoying s.t. it loads data (Like reference module written by myself),
      # I decided to put a flag so that when module gets activated, it runs rave_updates
      # The flag would be local_data$initialized (case 4)
      #
      # case 3: data imported (has_data = T), and initialized (module deactivated)
      # No rave_updates should execute, otherwise shiny does not know whether it's due to the input changed or not, and causing inifite looping
      # In this case, do nothing
      #
      #
      # case 4: sidebar switched (module activated)
      # Check the flag local_data$initialized, if has_data = T and:
      #
      # sub1: local_data$initialized = F
      # run rave_updates and local_data$initialized <- T
      #
      # sub2: local_data$initialized = T
      # give green light to rave_execute
      #
      #
      # Now the problem starts:
      # 1. if user changes subject data, we shall remove the runtime environment and prepare for the new data. The very first thing is to set local_data$initialized = F
      #
      # 2. user close the current session and run init_app() again. This time, we already have the data in repository, has_data = T at the beginning. Therefore
      # shiny might run rave_update immediately and cause problem. Therefore it's necessary to add - like javascript, when page render is finished, set has_data = T (TODO)
      #
      # 3. Async
      # Async has two flags:
      # a) suspended: if suspended, we don't care about the async result and future should be discard at once. We shall not check the result. However, there is no way to
      # terminate async process elegantly so I choose to leave that part as todo chunk which will be implemented in the future
      # b) has_results: This flag is a shared flag telling the main application that an update is needed and outputs should be rendered right now (IMPORTANT: before
      # re-render, run shiny_execute again)
      # Async part is scheduled and activated by an observeEvent
      #
      # 4. debounce inputs - rate policy
      # We should be careful about it. The following use case is forbidden. It will cause inifite loop
      #
      # # Code Start
      # reactive({
      #   catgl('Input changed')
      #   local_data$last_input
      # }) %>%
      #   debounce(50) ->
      #   check_inputs
      # observeEvent(check_inputs(), {
      #   local_data$last_input
      # })
      # # Code End
      #
      #
      # 5 Shiny components
      # in uicomp.R, UIs use the following flags:
      # has_data (isolated) to check if data is present (should be deprecated)
      # last_input - indicating the changes of inputs
      # show_results - receive signals that output should be rendered
      #
      # Now let's init local_data (reactive) storing flags
      #### ####
      execenv$global_reactives = global_reactives

      local_data = reactiveValues(

        # Flag indicating if data is present
        has_data = FALSE,

        # Module initialized?
        initialized = FALSE,

        # Flag when any input changes
        last_input = NULL,

        # Flag when input comps should be updated
        update_input = NULL,

        # Flag when rave_execute should run
        run_script = NULL,

        # Flag when rave_execute finishes
        has_results = NULL,

        # Flag telling outputs to render
        show_results = NULL,

        # For async modules. When initialized is FALSE, this always turns to TRUE
        suspended = TRUE,

        # A flag: sidebar == this module (active module?)
        focused = FALSE,

        # current input data
        current_param = list()
      )

      execenv$local_reactives = local_data

      # TODO: remove this environment
      local_static = new.env()
      local({
        activated = FALSE
      }, envir = local_static)

      ###### 0. debounce inputs - rate policy ######
      # Note: One way to debug is to remove "debounce"
      last_input <- debounce(
        reactive({
          re = local_data$last_input
          if(check_active()){
            # catgl('Input changed')
            return(re)
          }
          return(FALSE)
        }),
        20
      )

      # last_input <- reactive({
      #   re = local_data$last_input
      #   if(check_active()){
      #     catgl('Input changed')
      #     return(re)
      #   }
      #   return(FALSE)
      # })
      
      # observe({
      #   base::print('---------------------')
      #   base::print(local_data$run_script)
      # }, priority = 1000)

      run_script <- debounce(
        reactive({
          re = local_data$run_script
          # catgl('Cheking')
          if( check_active(reactive = FALSE) && length(re) && !isFALSE(re) ){
            catgl('Ready, prepared to execute scripts.')
            return(re)
          }
          return(FALSE)
        }),
        50
      )

      # run_script <- reactive({
      #   re = local_data$run_script
      #   if(check_active()){
      #     catgl('Ready, prepared to execute scripts.')
      #     return(re)
      #   }
      #   return(FALSE)
      # })

      reactive({
        re = local_data$has_results
        if(check_active()){
          catgl('Rendering')
          return(re)
        }
        return(FALSE)
      }) ->
        has_results

      ###### 1. Utils ######

      check_active = function(reactive = F){
        if(reactive){
          I = base::I
        }else{
          I = shiny::isolate
        }
        # Has data
        has_data = get_val(I(local_data$has_data), default = FALSE)
        has_data = length(has_data) && !isFALSE(has_data)
        # initialized
        initialized = get_val(I(local_data$initialized), default = FALSE)
        initialized = isTRUE(initialized)
        # is current module focused
        focused = get_val(I(local_data$focused), default = FALSE)
        focused = isTRUE(focused)

        if(has_data && focused && initialized){
          # catgl('Pass active check', level = 'INFO')
          return(TRUE)
        }else{
          if(focused){
            catgl('Check active: has_data - [', has_data, '], initialized - [', initialized, ']', level = 'WARNING')
          }
          return(FALSE)
        }
      }

      cache_all_inputs <- function(save = TRUE){
        # params = isolate(reactiveValuesToList(input))
        lapply(execenv$input_ids, function(inputId){
          val = shiny::isolate({ input[[inputId]] })
          cache_input(inputId = inputId, val = val, read_only = !save)
        }) ->
          altered_params

        names(altered_params) = execenv$input_ids
        altered_params = dipsaus::drop_nulls(altered_params)

        if(save){
          param_str = shiny::isolate(shiny::reactiveValuesToList(input, all.names = TRUE))
          param_str = paste(deparse(param_str), collapse = '')
          execenv$set_browser(param_str)
        }
        return(altered_params)
      }

      ###### 2. Global observes: ways to tell if module is activated or not ######
      observe({
        # Module need to check if data loaded need the requirement (TODO)
        # if not, disable local_data$has_data and return NULL

        # Pass the check, local_data$has_data = T
        local_data$has_data = global_reactives$has_data
      }, priority = -1L)

      # Signal to force deactivate module
      observeEvent(global_reactives$force_refresh_all, {
        catgl('Force refresh all - reset: ', stringr::str_to_upper(MODULE_ID))
        # terminate all current running process
        local_data$suspended = TRUE
        local_data$initialized = FALSE
        local_static$activated = FALSE

        params = isolate(reactiveValuesToList(input))
        clear_cache(levels = 1)
        execenv$reset(params)
      }, priority = 999L)

      # Assign local_data$focused
      observe({
        m = global_reactives$execute_module
        if(length(m) && m == stringr::str_to_upper(MODULE_ID)){
          catgl('Sidebar switched to ', m)
          local_data$focused = T

          # Add to global_reactives current module ID
          # this is used to keep track of user view history
          global_reactives$view_history = c(
            isolate(global_reactives$view_history),
            list(list(
              module_id = m,
              activated = F
            ))
          )
        }else{
          local_data$focused = F
        }
      }, priority = 999L)

      observe({
        if(local_data$focused && local_data$has_data){
          update_input()
        }
      })

      observe({
        if(has_results() != FALSE){
          local_data$show_results = Sys.time()
        }
      })


      ###### 3. Workflow ######

      # update_input()
      # What we know:
      # Module is initializing, re-init (has_data) or focused, inputs should be updated
      # We know has_data = T, also focused = T. However, initialized is not available
      #
      # What will happen: inputs will be updated, initialized will be T and last_input() will be triggered
      update_input = function(){
        catgl('Initializing/Updating inputs')
        # step 1: get updated inputs
        params = cache_all_inputs(save = FALSE)

        # step 2: set initializa = TRUE
        local_data$initialized = TRUE

        # step 3: update UI
        err_info = execenv$input_update(input = params, session = session, init = TRUE)

        # step 4: in case no UI updated, force trigger last_input()
        # TODO: since updating UI takes time, if it exceed 0.1 sec, then
        # this statement will force running the result and yield, most of
        # time, errors. Also, it is likely that rave_execute will be called
        # twice.
        # However, if n_errors[1] > 0, which means initial update has errors, we stop the process
        # because the module might encounter fatal error (lack of data or code error)
        n_errors = err_info$n_errors
        if(n_errors[1] > 0){
          catgl('Terminate the process due to initialization failures. Data not loaded? or code error? See the following information', level = 'INFO')
          sapply(err_info$init_error_msgs, logger, level = 'ERROR')
        }else{
          local_data$last_input = Sys.time()
        }
      }

      # last_input()
      # What we know:
      #  Initialized, has data, module activated
      # since Initialized = T, rave_inputs and updates are done, we only need to run script (rave_execute)
      observe({
        last_input_updated = last_input()
        if(!isFALSE(last_input_updated)){
          # catgl('Last input updated')
          local_data$run_script = Sys.time()
        }
      })

      

      ##### Scripts #####
      exec_script <- function(async = FALSE, force = FALSE){
        if(!execenv$auto_execute && !force){
          # Do not execute
          return()
        }
        catgl('Executing Script')
        showNotification(p(MODULE_LABEL, 'is running. Please wait...'), id = '.rave_main', duration = NULL)
        safe_wrap_expr({
        # tryCatch({
          # record time
          start_time = Sys.time()
          execenv$execute(async = async, force = force)
          if(async){
            local_data$suspended = FALSE
            showNotification(p('Running in the background. Results will be shown once finished.'), type = 'message', id = 'async_msg')
          }else{
            local_data$has_results = Sys.time()
            end_time = Sys.time()
            dta = time_diff(start_time, end_time)
            catgl(MODULE_LABEL, ' - Exec time: ', sprintf('%.3f (%s)', dta$delta, dta$units), level = 'INFO')
          }

          local_data$last_executed = TRUE
          cache_all_inputs()
          cache_input(inputId = '..onced', val = TRUE, read_only = FALSE, shared = FALSE)

          # BIG TODO!!! Uncommenting the following lines triggers executing script twice
          # My guess is when module is not autoexec, we use local_data$current_param to 
          # track whether input has been changed. However, that renderUI will create a 
          # button that belongs to the input, and input$..force_execute will update 
          # input stack and cause infinite loop (actually twice)
          #
          # lapply(execenv$input_ids, function(inputId){
          #   local_data$current_param[[inputId]] = shiny::isolate({ input[[inputId]] })
          # })

        }, onFailure = function(e){
          local_data$last_executed = FALSE
        }, finally = {
          
          removeNotification(id = '.rave_main')
        })

      }
      
      # run_script()
      # What we know:
      # 1. Initialized, has data, module activated,
      # 2. Some input changed
      observe({
        run_script_signal = run_script()
        if( shiny::isTruthy(run_script_signal) ){
          exec_script()
        }
      })

      ##### Async #####
      observeEvent(input$..async_run, {
        if(!is.null(local_data$run_async)){
          showNotification(p('There is another process running in the background. ', actionLink(execenv$ns('..kill'), 'Proceed?')), type = 'warning', duration = NULL, id = 'async_msg')
        }else{
          local_data$run_async = Sys.time()
        }
      })

      observeEvent(input$..kill, {
        local_data$run_async = Sys.time()
      })

      observeEvent(local_data$run_async, {
        is_run = !is.null(local_data$run_async)
        if(is_run){
          catgl('Running the script with async')
          exec_script(async = TRUE, force = TRUE)
        }
      })


      observeEvent(global_reactives$check_results, {
        if(!isolate(local_data$suspended)){
          catgl('Checking futures')
          f = execenv$param_env[['..rave_future_obj']]
          if(!is.null(f) && inherits(f, 'Future')){
            if(future::resolved(f)){
              execenv$param_env[['..rave_future_env']] = tryCatch({
                future::value(f)
              }, error = function(e){
                catgl('[ASYNC]: ', MODULE_LABEL, ' got an error during async evaluation:', level = 'ERROR')
                catgl(paste(utils::capture.output(traceback(e)), collapse = '\n'), level = 'ERROR')
                return(NULL)
              })
              local_data$suspended = TRUE
              # Need to run script again to update async_vars
              # However, we cannot directly call run_script since users might switch to other modules
              # We leave a flag instead
              # exec_script(async = F)
              local_data$run_script = Sys.time()
              local_data$run_async = NULL
              showNotification(p('Async evaluation is finished - ', MODULE_LABEL), duration = NULL, type = 'message')
            }
          }else{
            local_data$suspended = TRUE
          }
        }
      })

      ###########################




      # observeEvent(local_data$force_render, {
      #   if(global_reactives$has_data){
      #     catgl('Force Render Results')
      #     local_data$has_results = Sys.time()
      #   }
      # })



      # observeEvent(global_reactives$execute_module, {
      #
      #   if(local_data$has_data && global_reactives$execute_module == stringr::str_to_upper(MODULE_ID)){
      #     catgl('Sidebar switched to ', global_reactives$execute_module)
      #     local_data$last_input = Sys.time()
      #   }
      # })




      observeEvent(input$..incubator, {
        input_labels = stringr::str_c(unlist(execenv$input_labels))
        export_func = ls(execenv$static_env)
        is_export_func = vapply(export_func, function(x){
          is.function(execenv$static_env[[x]]) && stringr::str_detect(x, 'export_')
        }, FUN.VALUE = logical(1))
        export_func = export_func[is_export_func]

        if(length(export_func)){

          # find all analysis names
          analysis_names = module_analysis_names(module_id = MODULE_ID)

          showModal(
            modalDialog(
              title = '',
              easyClose = T,
              footer = tagList(
                modalButton('Cancel'),
                actionButton(execenv$ns('.export_ready'), 'OK')
              ),
              fluidRow(
                column(
                  width = 12,
                  selectInput(
                    execenv$ns('.export_func'), label = 'Which function to apply?',
                    choices = export_func
                  ),
                  selectInput(
                    execenv$ns('.export_name'), 'Analysis Name',
                    choices = c('[New..]', analysis_names)
                  ),
                  conditionalPanel(
                    condition = 'input[".export_name"] == "[New..]"',
                    textInput(
                      execenv$ns('.export_name_txt'), 'Enter a New Analysis Name',
                      placeholder = 'Alphabets or digits, or `_` only'
                    ),
                    ns = execenv$ns
                  )
                )
              )
            )
          )
        }else{
          showNotification(p('No report generating function detected.'), type = 'warning')
        }

      })

      observeEvent(input$.export_ready, {
        fun_name = input$.export_func
        analysis_name = input$.export_name
        if(analysis_name == '[New..]'){
          analysis_name = input$.export_name_txt
          analysis_name = stringr::str_remove_all(analysis_name, '[^a-zA-Z0-9_]')
        }
        analysis_name = stringr::str_to_upper(analysis_name)

        tryCatch({
          f = execenv$static_env[[fun_name]]
          fm = formals(f)
          if(is.null(fm[['...']])){
            fm %?<-% list()
            fm[['...']] = rlang::sym('')
            formals(f) = fm
          }
          con = subject_tmpfile(
            module_id = MODULE_ID,
            fun_name = stringr::str_remove(fun_name, '^export_'),
            pattern = sprintf(
            '[%s]_', strftime(Sys.time(), '%Y%m%d-%H%M%S')
          ))

          res = f(con, analysis_name, dirname(con))
          # Save to group analysis
          catgl('Saving to group analysis tables...')
          module_analysis_save(module_id = MODULE_ID, analysis_name = analysis_name, file = con, meta = res)
          showNotification(p('Module ID: ', MODULE_ID, ' exported!'), type = 'message')
        }, error = function(e){
          showNotification(p('Export failed: (message)', br(), e$message, br(), 'Please check console for error messages.'), type = 'error')
        })

        removeModal()
      })

      observeEvent(input$.gen_report, {

        curr_e = dipsaus::deparse_svec(get('electrodes', envir = data_env, inherits = F))
        output_labels = stringr::str_c(unlist(execenv$output_labels))
        input_labels = stringr::str_c(unlist(execenv$input_labels))
        # guess inputs
        sel = stringr::str_detect(stringr::str_to_lower(input_labels), 'electrode')
        if(sum(sel)){
          selected = input_labels[sel][1]
        }else{
          selected = NULL
        }

        modal = shiny::modalDialog(
          title = 'Export as Report',
          easyClose = F,
          footer = tagList(
            modalButton("Cancel"),
            downloadButton(execenv$ns('.do_gen_report'))
          ),
          fluidRow(
            column(
              12,
              textInput(execenv$ns('.report_electrodes'),
                        'Electrodes:', value = curr_e),
              selectInput(
                execenv$ns('.report_inputid'),
                label = 'Which variable is for electrodes?',
                choices = input_labels,
                selected = selected
              ),
              selectInput(
                execenv$ns('.report_outputid'),
                'Output Types:',
                choices = output_labels,
                selected = output_labels,
                multiple = T
              )
            )
          )
        )

        shiny::showModal(modal)
      })

      output$.do_gen_report <- downloadHandler(
        filename = function() {
          '[RaveReport].html'
        },
        content = function(con) {
          tryCatch({
            electrodes = dipsaus::parse_svec(input$.report_electrodes)
            electrodes = data_env$valid_electrodes(electrodes)
            inputId = execenv$input_ids[unlist(execenv$input_labels) == input$.report_inputid]
            outputId = execenv$output_ids[unlist(execenv$output_labels) %in% input$.report_outputid]
            if(length(outputId) == 0){
              outputId = execenv$output_ids[1]
            }
            param = lapply(execenv$input_ids, function(nm){
              input[[nm]]
            })
            names(param) = execenv$input_ids
            args = list(
              module = module,
              inputId = inputId,
              valueList = as.list(electrodes),
              param = param,
              outputs = outputId,
              output_format = 'html_document',
              knit_root_dir = dirname(module$script_path),
              envir = new.env(parent = data_env)
            )
            print(args)
            output_fpath = do.call(export_report, args = args)

            print(output_fpath)
            file.copy(output_fpath, con, overwrite = T)
            # write.csv(data, con)
          },error = function(e){
            writeLines(utils::capture.output(traceback(e)), con)
          })
        }
      )

      # Special output - if module is not auto updated
      output[['..params_current']] <- renderUI({
        input_changed = NULL
        update_btn = NULL
        if(!execenv$auto_execute){
          # Check if any params changed
          vapply(execenv$input_ids, function(inputId){
            re = !isTRUE(all.equal(local_data$current_param[[inputId]], input[[inputId]], check.attributes = F))
            re
          }, FUN.VALUE = FALSE) ->
            input_changed
          if(length(input_changed) && any(input_changed)){
            update_btn = dipsaus::actionButtonStyled(execenv$ns('..force_execute'), 'Update Output', type = 'warning', width = '100%')
            showNotification(p('Input changed, click ', actionLink(execenv$ns('..force_execute_1'), 'here'), ' or press ', strong('Ctrl/Command+Enter'), ' to update output.'), duration = NULL, id = '..input_updated')
          }else{
            update_btn = dipsaus::actionButtonStyled(execenv$ns('..force_execute'), 'Update Output', type = 'default', disabled = TRUE, width = '100%')
            removeNotification(id = '..input_updated')
          }

          shinydashboard::box(
            width = 12L,
            update_btn
            # p(tagList(info))
          )
        }

      })

      observeEvent(input[['..force_execute_1']], {
        # catgl('Force execute script')
        exec_script(async = FALSE, force = TRUE)
      })
      observeEvent(input[['..force_execute']], {
        # catgl('Force execute script')
        exec_script(async = FALSE, force = TRUE)
      })

      observeEvent(global_reactives$keyboard_event, {
        if(local_data$focused){
          e = global_reactives$keyboard_event
          if(length(e) && length(e$enter_hit) && length(e$ctrl_hit) && e$enter_hit && e$ctrl_hit){
            # catgl('Keyboard Signal Received.')
            exec_script(async = e$shift_hit, force = TRUE)
          }
        }
      })


      # Ready, run scripts that marked once execenv is ready
      lapply(execenv$ready_functions, function(f){
        if(is.function(f)){
          try(f(execenv$runtime_env))
        }
      })


      # register outputs (rave_outputs)
      execenv$register_output_events(
        input, output, session,
        local_data = local_data
      )

      # Register inputs (rave_inputs)
      execenv$register_input_events(
        input, output, session,
        local_data = local_data
      )


    },
    clean = function(){
      clear_cache(levels = 1)
    }
  )
}


