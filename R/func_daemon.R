daemon_app <- function(rave_id = 'test', idx = '1', launch.browser = FALSE, host = '127.0.0.1', port = NULL){

  ui = shiny::fillPage(
    title = 'RAVE Side Window',
    uiOutput('container')
  )

  server = function(input, output, session){

    # Check if ~/rave_modules/statics/session/!!rave_id exists, if not, stop app
    session_dir = file.path('~/rave_modules/statics/session', rave_id)
    env = new.env()
    local_data = reactiveValues(
      update = NULL,
      stop = FALSE,
      callback = NULL
    )

    observeEvent(local_data$stop, {
      if(local_data$stop){
        logger('Shutting down side app.', level = 'INFO')
        stopApp()
      }
    })

    if(!dir.exists(session_dir)){
      logger('Session is closed. Shutting down.', level = 'WARNING')
      local_data$stop = TRUE
    }

    # Constantly write to c2s_idx.rds to communicate with server
    file_to_parent = file.path(session_dir, sprintf('c2s_%s.rds', idx))
    writeLines(session$request$HTTP_HOST, con = file.path(session_dir, sprintf('ip_%s', idx)))


    # Constantly read command from parent session, they should be R scripts
    file_from_parent = file.path(session_dir, 's2c.R')
    server_command = reactiveFileReader(
      filePath = file_from_parent, session = session,
      intervalMillis = 500, readFunc = function(file){
        if(!file.exists(file)){
          local_data$stop = TRUE
          return()
        }

        # try to read from file
        tryCatch({
          s = readLines(file)
          s = stringr::str_trim(s)
          s = s[s != '']
          if(s[length(s)] == '# RAVE EOF #'){
            # File is complete, should run s

            s = c('{', s, '}')

            s = paste(s, collapse = '\n')

            quo = rlang::parse_quo(s, env = env)

            re = rlang::eval_tidy(quo, env = env)


            if(is.function(re$render_function) && length(re$outputId)==1 && is.character(re$outputId)){
              local_data$render_function = re$render_function
              local_data$output_type = re$output_type
              local_data$outputId = re$outputId
              local_data$update = Sys.time()
            }

            return(quo)


          }else if(s[length(s)] == sprintf('# SHUTDOWN %s #', idx)){
            local_data$stop = TRUE
            return()
          }
          return(NULL)
        }, error = function(e){
          print(e)
          NULL
        })

      })

    output$container <- renderUI({
      local_data$update
      if(is.null(local_data$output_type)){
        return(div(
          style = 'padding-left:20px;',
          h1('Waiting for command from server.')
        ))
      }

      switch(
        local_data$output_type,
        'threeBrain' = { return(threejsBrainOutput('threeBrain', height = '100vh')) }
      )
    })

    observe({
      params = reactiveValuesToList(input)
      nms = names(params)
      print(nms)
      callback_prefix = isolate(local_data$output_type)
      output_id = isolate(local_data$outputId)
      if(length(callback_prefix)!=1 || length(output_id) != 1){
        return()
      }
      sel = stringr::str_detect(nms, sprintf('^%s', callback_prefix))
      if(any(sel)){
        idx = which(sel)[1]
        local_data$callback = list(
          rave_id = rave_id,
          message = params[[idx]],
          outputId = output_id,
          callbackId = stringr::str_replace(nms[[idx]], sprintf('^%s', callback_prefix), output_id)
        )
      }
    })

    observeEvent(local_data$callback, {
      if(!is.null(local_data$callback)){

        print('sending to parent session')

        dat = local_data$callback
        saveRDS(dat, file = file_to_parent)
      }
    })

    observe({
      server_command()
    })



    output$threeBrain <- renderBrain({
      local_data$update
      local_data$render_function()
    })

  }


  shinyApp(ui, server, options = list(
    launch.browser = launch.browser,
    host = host,
    port = port
  ))

}


find_available_port <- function(){
  servr::random_port(n = 1000)
}


launch_daemon <- function(rave_id = 'test', idx = '1', launch.browser = FALSE, host = '127.0.0.1', port = NULL){
  port %?<-% find_available_port()
  port = as.integer(port)

  cmd = sprintf('rave:::daemon_app("%s", "%s", %s, "%s", %d)', rave_id, idx, launch.browser, host, port)

  r = normalizePath(file.path(base::R.home(), 'Rscript'))

  system(paste0(r, " -e '", cmd, "'"), wait = FALSE)

  return(list(
    host = host,
    port = port
  ))
}


create_daemon <- function(session, idx = '1'){

  rave_id = add_to_session(session)
  session_dir = file.path('~/rave_modules/statics/session', rave_id)
  c2s_file = file.path(session_dir, sprintf('c2s_%s.rds', idx))

  tryCatch({
    dir.create(session_dir, showWarnings = F, recursive = T)
    r2c_file = file.path(session_dir, 's2c.R')
    writeLines('', r2c_file)


    # Start a daemon app
    re = launch_daemon(rave_id = rave_id, idx = idx, launch.browser = FALSE)

    re$session_dir = session_dir

    daemon_settings = add_to_session(session, key = 'daemon_settings', val = re, override = TRUE)

    c2s_reader = shiny::reactiveFileReader(
      intervalMillis = 500, session = session,
      filePath = c2s_file, readFunc = function(file){
        if(!file.exists(file)){
          return(NULL)
        }
        tryCatch({
          dat = readRDS(c2s_file)
          # list(
          #   message = input[[idx]],
          #   outputId = callback_prefix,
          #   callbackId = nms[[idx]]
          # )
          if(dat$rave_id == rave_id){
            return(dat)
          }
          NULL
        }, error = function(e){
          NULL
        })

    })

    observe({
      dat = c2s_reader()

      if(is.list(dat) && length(dat$callbackId)==1 && is.character(dat$callbackId)){
        print(dat)
        impl = .subset2( session$input, 'impl' )
        impl$set(dat$callbackId, dat$message)
      }
    })


  }, error = function(e){

  })
  return(session_dir)

}


#' @export
send_to_daemon <- function(expr, outputId, type = 'threeBrain', save = NULL){
  .env = parent.frame()

  expr = substitute(expr)

  session = getDefaultReactiveDomain()
  rave_id = add_to_session(session, key = 'rave_id')
  daemon_settings = add_to_session(session, key = 'daemon_settings')

  session_dir = daemon_settings$session_dir


  # save re
  if(length(save)){
    save(list = save, file = file.path(session_dir, 'data.RData'), envir = .env)
  }

  quo = rlang::quo({
    load(file.path(!!session_dir, 'data.RData'), envir = environment())

    list(
      render_function = function(){
        !!expr
      },
      output_type = !!type,
      outputId = !!outputId
    )
  })
  s = rlang::quo_text(quo)
  writeLines(c(s, '# RAVE EOF #'), con = file.path(session_dir, 's2c.R'))

  # read IP
  ip_file = file.path(session_dir, 'ip_1')
  if(file.exists(ip_file)){
    ip = readLines(ip_file)[1]
  }else{
    ip = sprintf('%s:%s', daemon_settings$host, daemon_settings$port)
  }

  ip = stringr::str_trim(ip)

  shinyjs::runjs(sprintf('window.open("http://%s")', ip))



}
