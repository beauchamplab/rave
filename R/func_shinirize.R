#' @import magrittr
#' @import shiny
shinirize <- function(module, session = getDefaultReactiveDomain(), test.mode = TRUE){
  # assign variables
  MODULE_ID = module$module_id
  MODULE_LABEL = module$label_name

  data_env = getDefaultDataRepository()

  # load script
  module$load_script(session = session)

  # Runtime environment
  execenv = module$get_or_new_exec_env(session = session)

  # ui
  list(
    id = MODULE_ID,
    ui = function(){
      title = sprintf('RAVE - %s', MODULE_LABEL)
      module$render_ui(session = session)
    },
    server = function(input, output, session, global_reactives, env = new.env()){
      local_data = reactiveValues(
        show_results = NULL,
        input_updated = NULL,
        force_render = NULL,
        run_script = NULL,

        last_input = Sys.time(),
        last_executed = FALSE,
        initialized = FALSE,
        has_data = FALSE,
        has_results = Sys.time(),
        suspended = TRUE,
        input_queue = NULL,
        onced = FALSE, # flag: input cached?
        .rave_future = NULL
      )


      observeEvent(local_data$force_render, {
        if(global_reactives$has_data){
          logger('Force Render Results')
          local_data$has_results = Sys.time()
        }
      })

      run_script <- function(async = FALSE){
        if(isolate(!local_data$has_data)){
          return(NULL)
        }
        logger('Executing Script')
        tryCatch({
          # record time
          start_time = Sys.time()
          execenv$execute(async = async)
          if(async){
            local_data$suspended = FALSE
          }else{
            local_data$has_results = Sys.time()
            end_time = Sys.time()
            if(test.mode){
              logger(MODULE_LABEL, ' - Exec time: ', sprintf('%.3f (sec)', end_time - start_time), level = 'INFO')
            }
          }

          local_data$last_executed = T
          cache_all_inputs()
          if(!isolate(local_data$onced)){
            execenv$cache_input('..onced', TRUE, read_only = F, sig = 'special')
            local_data$onced = T
          }
        }, error = function(e){
          sapply(capture.output(traceback(e)), logger, level = 'ERROR')
          local_data$last_executed = F
        })
      }

      # debounce inputs - rate policy
      reactive({
        local_data$last_input
      }) %>%
        debounce(rave_options('delay_input')) ->
        check_inputs

      reactive({
        local_data$has_results
      }) %>%
        debounce(20) ->
        show_results

      reactive({
        local_data$run_script
      }) %>%
        debounce(20) ->
        get_results

      reactive({
        local_data$input_updated
      }) %>%
        debounce(50) ->
        input_updated


      observeEvent(check_inputs(), {
        if(!is.null(local_data$last_input)){
          local_data$input_queue = NULL
          local_data$input_updated = Sys.time()
        }
      })

      observeEvent(show_results(), {
        if(!is.null(local_data$has_results)){
          local_data$show_results = Sys.time()
        }
      })



      observeEvent(get_results(), {
        if(!is.null(local_data$run_script) && local_data$initialized){

          logger('Running scripts')
          run_script()
        }
      }, priority = -1000L)
      observeEvent(input_updated(), {
        logger = function(...){
          rave::logger('[', MODULE_ID, '] ', ...)
        }

        if(!global_reactives$has_data){
          return(NULL)
        }else{
          # Module need to check if data loaded need the requirement (TODO)
          # if not, disable local_data$has_data and return NULL

          # Pass the check, local_data$has_data = T
          local_data$has_data = T
        }
        if(test.mode){
          logger('Flushing Toilet')
        }

        # get global/local cache
        # params = isolate(reactiveValuesToList(input))

        params = cache_all_inputs(save = F)
        logger('Stand up')
        if(isolate(local_data$initialized)){
          # get current input
          execenv$input_update(input = params, session = session, init = FALSE)
          local_data$run_script = Sys.time()
          logger('Flush')
        }else if(
          local_data$has_data && global_reactives$execute_module == str_to_upper(MODULE_ID)
        ){
          logger('Close lid')
          execenv$input_update(input = params, session = session, init = TRUE)
          logger('Flush')
          local_data$initialized = TRUE
          logger('Module Initialized, Gonna Flush the Toilet again.')
          # For initialization, no need to add flag to last_input
          # However, if use switched data and the new param settings are the same
          # as the old ones. In this case, result won't be updated and we have to
          # tell it hey! you need to refresh urself.
          #
          # BUT, this will cause a problem. Because if the input need to be changed
          # the result will flush before input changed.
          local_data$last_input = Sys.time()
        }
        logger('Reload Water')
      }, priority = 500L)

      observeEvent(global_reactives$execute_module, {

        if(local_data$has_data && global_reactives$execute_module == str_to_upper(MODULE_ID)){
          logger('Sidebar switched to ', global_reactives$execute_module)
          local_data$input_updated = Sys.time()
        }
      })

      observeEvent(global_reactives$force_refresh_all, {
        logger('Force refresh all - reset: ', str_to_upper(MODULE_ID))
        # terminate all current running process
        local_data$suspended = TRUE
        params = isolate(reactiveValuesToList(input))
        execenv$reset(params)

        local_data$initialized = FALSE
        local_data$input_updated = Sys.time()
      }, priority = 999L)


      observeEvent(input$..async_run, {
        if(global_reactives$has_data){
          if(!is.null(local_data$run_async)){
            showNotification(p('There is another process running in the background. ', actionLink(execenv$ns('..kill'), 'Proceed?')), type = 'warning', duration = NULL, id = 'async_msg')
          }else{
            local_data$run_async = Sys.time()
          }

        }


      })

      observeEvent(local_data$run_async, {
        is_run = !is.null(local_data$run_async)
        if(is_run){
          logger('Running the script with async')
          run_script(async = TRUE)
          showNotification(p('Running in the background. Results will be shown once finished.'), type = 'message', id = 'async_msg')
        }
      })

      observeEvent(input$..kill, {
        local_data$run_async = Sys.time()
      })

      # observeEvent(local_data$show_results, {
      #   # TODO: check
      #   # print(ls(execenv$runtime_env))
      # })

      observeEvent(global_reactives$check_results, {
        if(!isolate(local_data$suspended)){
          logger('Checking futures')
          f = execenv$param_env[['..rave_future_obj']]
          if(!is.null(f) && is(f, 'Future')){
            if(future::resolved(f)){
              execenv$param_env[['..rave_future_env']] = tryCatch({
                future::value(f)
              }, error = function(e){
                logger('[ASYNC]: ', MODULE_LABEL, ' got an error during async evaluation:', level = 'ERROR')
                logger(paste(capture.output(traceback(e)), collapse = '\n'), level = 'ERROR')
                return(NULL)
              })
              local_data$suspended = TRUE
              # Need to run script again to update async_vars
              run_script(async = F)
              local_data$run_async = NULL
              showNotification(p('Async evaluation is finished - ', MODULE_LABEL), duration = 8, type = 'message')
            }
          }else{
            local_data$suspended = TRUE
          }
        }
      })

      observeEvent(input$..incubator, {
        input_labels = str_c(unlist(execenv$input_labels))
        export_func = ls(execenv$static_env)
        is_export_func = vapply(export_func, function(x){
          is.function(execenv$static_env[[x]]) && str_detect(x, 'export_')
        }, FUN.VALUE = logical(1))
        export_func = export_func[is_export_func]

        if(length(export_func)){
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

        tryCatch({
          # write.niml(
          #   res,
          #   electrode_numbers = data_env$electrodes,
          #   value_labels = NULL,
          #   prefix = sprintf('%s', str_replace_all(MODULE_LABEL, '[^A-Za-z0-9_]', '_')),
          #   add_electrodes_as_column = TRUE,
          #   value_file = '__vals.dat',
          #   index_file = '__ind.dat',
          #   work_dir = data_env$subject$dirs$suma_out_dir) ->
          #   cmd
          res = execenv$static_env[[fun_name]]()
          assign('..rave_exported', res, envir = globalenv())
          if(!(length(res) == 1 && is.character(res))){
            res = 'Exported!'
          }
          showNotification(p(res), type = 'message')
        }, error = function(e){
          showNotification(p('Export failed: (message)', br(), e$message, br(), 'Please check console for error messages.'), type = 'error')
        })

        removeModal()
      })

      observeEvent(input$.gen_report, {

        curr_e = rave:::deparse_selections(get('electrodes', envir = data_env, inherits = F))
        output_labels = str_c(unlist(execenv$output_labels))
        input_labels = str_c(unlist(execenv$input_labels))
        # guess inputs
        sel = str_detect(str_to_lower(input_labels), 'electrode')
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
            electrodes = rave:::parse_selections(input$.report_electrodes)
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
              envir = new.env(parent = getDefaultDataRepository())
            )
            print(args)
            output_fpath = do.call(export_report, args = args)

            print(output_fpath)
            file.copy(output_fpath, con, overwrite = T)
            # write.csv(data, con)
          },error = function(e){
            writeLines(capture.output(traceback(e)), con)
          })
        }
      )


      cache_all_inputs <- function(save = T){
        params = isolate(reactiveValuesToList(input))
        lapply(execenv$input_ids, function(inputId){
          val = params[[inputId]]
          execenv$cache_input(inputId, val, read_only = !save)
        }) ->
          altered_params
        names(altered_params) = execenv$input_ids
        altered_params = dropNulls(altered_params)
        return(altered_params)
      }

      # register outputs
      execenv$register_output_events(
        input, output, session,
        local_data = local_data
      )

      execenv$register_input_events(
        input, output, session,
        local_data = local_data
      )


    }
  )
}
