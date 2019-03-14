define_output_3d_viewer <- function(
  outputId, title, surfaces = 'pial', multiple_subject = F,
  message = 'Click here to Generate 3D viewer',
  height = '500px', width = 12, order = 0, additional_ui = NULL
){

  # Generate reactives
  output_call = paste0(outputId, '_widget')
  output_btn = paste0(outputId, '_btn')
  output_fun = paste0(outputId, '_fun')

  additional_ui = substitute(additional_ui)


  quo = rlang::quo({
    assign(!!output_call, function(){
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
        threeBrain::threejsBrainOutput(ns(!!outputId), height = !!height)
      )
    }, envir = environment())
    local({
      `%?<-%` <- rave::`%?<-%`
      input = getDefaultReactiveInput()
      output = getDefaultReactiveOutput()
      session = getDefaultReactiveDomain()
      local_data %?<-% reactiveValues(

      )

      output[[!!outputId]] <- threeBrain::renderBrain({
        brain = rave::rave_brain2(surfaces = !!surfaces, multiple_subject = !!multiple_subject)
        brain$load_electrodes(subject)
        brain$load_surfaces(subject)

        re = brain
        # Render function
        if(input[[!!output_btn]] > 0){
          f = get0(!!output_fun, envir = ..runtime_env, ifnotfound = function(...){
            rutabaga::cat2('3D Viewer', !!outputId,  'cannot find function', !!output_fun, level = 'INFO')
          })

          tryCatch({
            re = f(brain)
          }, error = function(e){
            rave::logger(e, level = 'ERROR')
          })

        }

        if('htmlwidget' %in% class(re)){
          # User called $view() with additional params, directly call the widget
          re
        }else if('rave_three_brain' %in% class(re)){
          # User just returned brain object
          re$view()
        }else{
          # User returned nothing
          brain$view()
        }


      })
    })
  })

  # generate output
  df = rlang::quo({
    define_output(
      definition = customizedUI(!!output_call),
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




