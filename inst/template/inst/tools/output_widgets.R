define_output_3d_viewer <- function(
  outputId, title, surfaces = 'pial', multiple_subject = F,
  message = 'Click here to Generate 3D viewer',
  height = '500px', width = 12, order = 0, additional_ui = NULL
){

  # Generate reactives
  output_widget_id = paste0(outputId, '_widget')
  output_btn = paste0(outputId, '_btn')
  output_fun = paste0(outputId, '_fun')

  additional_ui = substitute(additional_ui)


  quo = rlang::quo({
    assign(!!outputId, function(){
      clicked = shiny::isolate(input[[!!output_btn]])

      htmltools::tagList(
        htmltools::div(
          style = 'padding: 10px;',
          htmltools::a(
            id = ns(!!output_btn),
            href = '#',
            class = "action-button",
            !!message
          ),
          eval(!!additional_ui)
        ),
        threeBrain::threejsBrainOutput(ns(!!output_widget_id), height = !!height)
      )
    }, envir = environment())
    local({
      `%?<-%` <- rave::`%?<-%`
      input = getDefaultReactiveInput()
      output = getDefaultReactiveOutput()
      session = getDefaultReactiveDomain()
      local_data %?<-% reactiveValues(

      )

      output[[!!output_widget_id]] <- threeBrain::renderBrain({
        brain = rave::rave_brain2(surfaces = !!surfaces, multiple_subject = !!multiple_subject)
        brain$load_electrodes(subject)
        brain$load_surfaces(subject)

        set_val = function(electrode, value, time, message = ''){
          if(missing(time)){
            if(length(value) == length(preload_info$time_points)){
              time = preload_info$time_points
            }else{
              time = seq_along(value) - 1
            }
          }
          brain$set_electrode_value(subject = subject, electrode, value, time, message)
        }

        # Render function
        if(input[[!!output_btn]] > 0){
          f = get0(!!output_fun, ifnotfound = function(...){})
          tryCatch({
            f(set_val)
          }, error = function(e){
            rave::logger(e, level = 'ERROR')
          })

        }

        brain$view()
      })
    })
  })

  # generate output
  df = rlang::quo({
    define_output(
      definition = customizedUI(!!outputId),
      title = !!title,
      width = !!width,
      order = !!order
    )

    load_scripts(rlang::quo({!!quo}))
  })
  eval(rlang::quo_squash(df), envir = parent.frame())
  # evaluate

  invisible(quo)

}




