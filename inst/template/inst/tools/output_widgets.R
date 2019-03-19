define_output_3d_viewer <- function(
  outputId, title, surfaces = 'pial', multiple_subject = F,
  message = 'Generate 3D viewer. ',
  height = '500px', width = 12, order = 0, additional_ui = NULL
){

  # Generate reactives
  output_call = paste0(outputId, '_widget')
  output_btn = paste0(outputId, '_btn')
  output_new = paste0(outputId, '_new')
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
          '   ',
          htmltools::a(
            id = ns(!!output_new),
            href = '#',
            class = "action-button",
            'open viewer in a new window.'
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
      local_data %?<-% reactiveValues()
      local_env = new.env()

      observeEvent(input$output_new, {
        if(!is.null(local_env$widget)){

          tryCatch({
            widget = local_env$widget

            rave::send_to_daemon({
              widget
            }, type = 'threeBrain', outputId = ns(!!outputId),
            save = c('widget'))
          }, error = function(e){
            showNotification(p('Failed to launch the side viewer. Error message: ', e), type = 'error')
          })



        }
      })

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
          local_env$widget = re
          re
        }else if('rave_three_brain' %in% class(re)){
          # User just returned brain object
          local_env$widget = re$view()
          re$view()
        }else{
          # User returned nothing
          local_env$widget = brain$view()
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





# define_output_3d_viewer <- function(
#   outputId, title, surfaces = 'pial', multiple_subject = F,
#   message = 'Click here to Generate 3D viewer',
#   height = '500px', width = 12, order = 0, additional_ui = NULL
# ){
#
#   # Generate reactives
#   output_call = paste0(outputId, '_widget')
#   output_btn = paste0(outputId, '_btn')
#   output_fun = paste0(outputId, '_fun')
#
#   additional_ui = substitute(additional_ui)
#
#
#   quo = rlang::quo({
#     assign(!!output_call, function(){
#       clicked = shiny::isolate(input[[!!output_btn]])
#
#       htmltools::tagList(
#         htmltools::div(
#           style = 'padding: 10px;',
#           htmltools::a(
#             id = ns(!!output_btn),
#             href = '#',
#             class = "action-button",
#             !!message
#           ),
#           eval(!!additional_ui)
#         ),
#         threeBrain::threejsBrainOutput(ns(!!outputId), height = !!height)
#       )
#     }, envir = environment())
#     local({
#       `%?<-%` <- rave::`%?<-%`
#       input = getDefaultReactiveInput()
#       output = getDefaultReactiveOutput()
#       session = getDefaultReactiveDomain()
#       local_data %?<-% reactiveValues(
#
#       )
#
#       output[[!!outputId]] <- threeBrain::renderBrain({
#         brain = rave::rave_brain2(surfaces = !!surfaces, multiple_subject = !!multiple_subject)
#         brain$load_electrodes(subject)
#         brain$load_surfaces(subject)
#
#         re = brain
#         # Render function
#         if(input[[!!output_btn]] > 0){
#           f = get0(!!output_fun, envir = ..runtime_env, ifnotfound = function(...){
#             rutabaga::cat2('3D Viewer', !!outputId,  'cannot find function', !!output_fun, level = 'INFO')
#           })
#
#           tryCatch({
#             re = f(brain)
#           }, error = function(e){
#             rave::logger(e, level = 'ERROR')
#           })
#
#         }
#
#         if('htmlwidget' %in% class(re)){
#           # User called $view() with additional params, directly call the widget
#           re
#         }else if('rave_three_brain' %in% class(re)){
#           # User just returned brain object
#           re$view()
#         }else{
#           # User returned nothing
#           brain$view()
#         }
#
#
#       })
#     })
#   })
#
#   # generate output
#   df = rlang::quo({
#     define_output(
#       definition = customizedUI(!!output_call),
#       title = !!title,
#       width = !!width,
#       order = !!order
#     )
#
#     load_scripts(rlang::quo({!!quo}))
#   })
#   eval(rlang::quo_squash(df), envir = parent.frame())
#   # evaluate
#
#   invisible(quo)
#
# }
#
#
#
#
