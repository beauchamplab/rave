define_output_3d_viewer <- function(
  outputId, title, surfaces = 'pial', multiple_subject = F,
  message = 'Generate 3D Viewer ',
  height = '500px', width = 12, order = 0, additional_ui = NULL,
  hide_btn = FALSE, ...
){

  # Generate reactives
  output_call = paste0(outputId, '_widget')
  output_btn = paste0(outputId, '_btn')
  output_new = paste0(outputId, '_new')
  output_fun = paste0(outputId, '_fun')

  additional_ui = substitute(additional_ui)


  quo = rlang::quo({

    ...local_env = new.env()

    assign(!!output_call, function(){
      clicked = shiny::isolate(input[[!!output_btn]])

      # Monitor subject change. If changed, then refresh!
      monitor_subject_change()
      
      if( !!hide_btn ){
        btn = NULL
      }else{
        btn = tagList(htmltools::a(
          id = ns(!!output_btn),
          href = '#',
          class = "action-button",
          !!message
        ),
        ' | ')
      }

      htmltools::tagList(
        htmltools::div(
          btn,
          htmltools::a(
            id = ns(!!output_new),
            href = '#',
            class = "action-button",
            ' Open Viewer in a New Window '
          ),
          ' | ',
          htmltools::a(
            href = 'https://github.com/dipterix/threeBrain/blob/dev/shortcuts.md',
            target = '_blank', ' Keyboard Shortcuts ', shiny::icon('external-link')
          ),
          eval(!!additional_ui)
        ),
        htmltools::div(
          style = 'margin: 0 -10px -10px -10px',
          threeBrain::threejsBrainOutput(ns(!!outputId), height = !!height)
        )
      )
    }, envir = environment())
    local({
      `%?<-%` <- rave::`%?<-%`
      input = getDefaultReactiveInput()
      output = getDefaultReactiveOutput()
      session = getDefaultReactiveDomain()


      observeEvent(input[[!!output_new]], {

        cat2('Opening a side window...')

        if(!is.null(...local_env$widget)){

          # tryCatch({
          #   widget = ...local_env$widget
          #
          #   rave::send_to_daemon({
          #     widget
          #   }, type = 'threeBrain', outputId = ns(!!outputId),
          #   save = c('widget'))
          # }, error = function(e){
          #   showNotification(p('Failed to launch the side viewer. Error message: ', e), type = 'error')
          # })

          # generate url
          session = getDefaultReactiveDomain()
          rave_id = session$userData$rave_id
          if(is.null(rave_id)){ rave_id = '' }
          token = session$userData$token
          if(is.null(token)){ token = '' }
          globalId = ns(!!outputId)

          query_str = list(
            type = '3dviewer',
            globalId = htmltools::urlEncodePath(globalId),
            sessionId = htmltools::urlEncodePath(rave_id),
            token = token
          )
          url = paste(sprintf('%s=%s', names(query_str), as.vector(query_str)), collapse = '&')

          shinyjs::runjs(sprintf('window.open("/?%s");', url))
        }

      })

      render_func = function(){
        threeBrain::renderBrain({
          brain = rave::rave_brain2(subject = subject, surfaces = !!surfaces)
          
          shiny::validate(
            shiny::need(!is.null(brain), message = 'Cannot find surface/volume files')
          )

          re = brain
          
          local_signal = input[[!!output_btn]]
          
          # Render function
          if( length(local_signal) && local_signal > 0 ){
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
            ...local_env$widget = re
            re
          }else if('R6' %in% class(re)){
            # User just returned brain object
            ...local_env$widget = re$plot()
            re$plot(side_display = FALSE)
          }else{
            # User returned nothing
            ...local_env$widget = brain$plot()
            brain$plot(side_display = FALSE)
          }


        })
      }

      # Register render function
      output[[!!outputId]] <- render_func()

      # Register cross-session function so that other sessions can register the same output widget
      session$userData$cross_session_funcs %?<-% list()
      # ns must be defined, but in get_module(..., local=T) will raise error
      # because we are not in shiny environment
      ns %?<-% function(x){x} 
      session$userData$cross_session_funcs[[ns(!!outputId)]] = render_func
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

    # https://github.com/r-lib/rlang/issues/772
    # This seems to be an issue of rlang
    # load_scripts(rlang::quo({!!quo})) will throw error of (Error: `arg` must be a symbol)
    load_scripts(rlang::quo(!!quo))
  })
  eval(rlang::quo_squash(df), envir = parent.frame())
  # evaluate

  invisible(quo)

}



