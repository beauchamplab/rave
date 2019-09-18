rave_pre_eleclocal3 <- function(module_id = 'ELECLOCAL_M', sidebar_width = 2){
  ns = shiny::NS(module_id)
  
  body = fluidRow(
    box(
      width = sidebar_width,
      title = 'Electrodes',
      uiOutput(ns('eloc_inputs1'))
    ),
    box(
      title = 'Viewer',
      width = 12 - sidebar_width,
      uiOutput(ns('eloc_outputs1'))
    )
  )
  
  server = function(input, output, session, user_data, utils){
    
    local_data = reactiveValues()
    # Init/reset data when receive reset signal
    observeEvent(user_data$reset, {
      # Reset first
      local_data$reset = Sys.time()
      local_data$has_raw_cache = utils$has_raw_cache()
    })
    
    output$eloc_inputs1 <- renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache, message = 'Please import subject first.'))

      dirs = utils$get_from_subject('dirs')
      sub_dir = dirs$subject_dir

      if( threeBrain::check_freesurfer_path(sub_dir) ){
        re = checkboxInput(ns('use_template'), 'Use Template', value = FALSE)
      }else{
        re = checkboxInput(ns('use_template'), 'Use Template', value = TRUE)
      }
      re
    })

    output$eloc_outputs1 <- renderUI({
      div(style='margin:-10px',
          threeBrain::threejsBrainOutput(ns('eloc_viewer'), height = '80vh'))
    })
    output$eloc_viewer <- threeBrain::renderBrain({
      validate(need(local_data$has_raw_cache && length(input$use_template), message = ''))
      brain = NULL
      if( !isTRUE(input$use_template) ){
        try({
          dirs = utils$get_from_subject('dirs')
          sub_dir = dirs$subject_dir
          path = sub_dir
          scode = utils$get_from_subject('subject_code')
          brain = threeBrain::freesurfer_brain(path, scode)
        }, silent = TRUE )
        
      }
      if( is.null(brain) ){
        scode = getOption('threeBrain.template_subject', 'N27')
        path = file.path(getOption('threeBrain.template_dir', '~/rave_data/others/three_brain'), scode)
        brain = threeBrain::freesurfer_brain(path, scode)
      }
      
      brain$plot(control_presets = 'electrode_localization')
    })
  }
  
  return(list(
    body = body,
    server = server
  ))
}
