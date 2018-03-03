#' @import magrittr
#' @import shiny
shinirize <- function(module, session = shiny::getDefaultReactiveDomain(), test.mode = TRUE){
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
        if(isolate(!global_reactives$has_data)){
          return(NULL)
        }
        logger('Executing Script')
        # record time
        start_time = Sys.time()
        # local_data$last_executed = F
        q = quote({
          execenv$generate_results(env = local_data, async = async)
          if(async){
            local_data$suspended = FALSE
          }
          local_data$has_results = Sys.time()
          end_time = Sys.time()
          if(test.mode){
            logger(MODULE_LABEL, ' - Exec time: ', sprintf('%.3f (sec)', end_time - start_time), level = 'INFO')
          }
        })


        tryCatch({
          eval(q)
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

        if(!global_reactives$has_data){
          return(NULL)
        }
        if(test.mode){
          logger('Flushing Toilet')
        }

        # get global/local cache
        # params = isolate(reactiveValuesToList(input))
        params = cache_all_inputs(save = F)
        if(local_data$initialized){
          # get current input
          execenv$input_update(input = params, session = session, init = FALSE)
          local_data$run_script = Sys.time()
        }else if(
          global_reactives$has_data && global_reactives$execute_module == str_to_upper(MODULE_ID)
        ){

          execenv$input_update(input = params, session = session, init = TRUE)
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
      }, priority = 500L)

      observeEvent(global_reactives$execute_module, {

        if(global_reactives$has_data && global_reactives$execute_module == str_to_upper(MODULE_ID)){
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

      observeEvent(input$.force_run, {
        if(global_reactives$has_data){
          logger('Running the script with async')
          run_script(async = TRUE)
        }
      })

      # observeEvent(local_data$show_results, {
      #   # TODO: check
      #   # print(ls(execenv$runtime_env))
      # })

      observeEvent(global_reactives$check_results, {
        if(!global_reactives$has_data){
          return(NULL)
        }
        if(!local_data$suspended){
          logger('Checking futures')
          f = local_data$.rave_future
          if(!is.null(f) && 'Future' %in% class(f$async_future)){
            if(future::resolved(f$async_future)){
              value(f$async_future)
              res = tryCatch({
                value(f$async_future)
              }, error = function(e){
                capture.output(traceback(e))
              })
              tmp_env = new.env()
              if(is.environment(res)){
                for (nm in ls(res)) {
                  obj = get(nm, envir = res)

                  capture.output({
                    tryCatch({
                      tmp_env$obj_size = pryr::object_size(obj)
                    }, error = function(e){
                      tmp_env$obj_size = 0
                    })
                  }) -> junk
                  if(tmp_env$obj_size < rave_options('big_object_size')){
                    if(is.function(obj)){
                      environment(obj) <- execenv$runtime_env
                    }
                    assign(nm, obj, envir = execenv$runtime_env)
                  }
                }
                # remove
                em(tmp_env)
              }else{
                # async module has error
                lapply(res, logger, level = 'ERROR')
              }
              local_data$suspended = TRUE
              local_data$has_results = Sys.time()
            }
          }else{
            local_data$suspended = TRUE
          }
        }
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


      # register inputs
      lapply(execenv$input_ids, function(inputId){
        observeEvent(input[[inputId]], {
          val = input[[inputId]]
          t = Sys.time()

          if(global_reactives$has_data){
            assign(inputId, input[[inputId]], envir = execenv$runtime_env)
            local_data$last_input = t
            local_data$input_queue = c(local_data$input_queue, inputId)

          }

        })
      })

      cache_all_inputs <- function(save = T){
        params = isolate(reactiveValuesToList(input))
        lapply(execenv$input_ids, function(inputId){
          val = params[[inputId]]
          execenv$cache_input(inputId, val, read_only = !save)
        }) ->
          altered_params
        altered_params = dropNulls(altered_params)
        return(altered_params)
      }

      # register outputs
      execenv$register_output_events(
        input, output, session,
        local_data = local_data
      )
    }
  )
}
