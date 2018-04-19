# post reference
rave_pre_postref3 <- function(module_id = 'POSTREF_M', sidebar_width = 2){
  ns = shiny::NS(module_id)

  body = fluidRow(
    shinydashboard::box(
      title = 'Post Reference',
      width = sidebar_width,
      uiOutput(ns('inputui1'))
    )
  )

  server = function(input, output, session, user_data, utils){
    local_data = reactiveValues(
      checklevel = 0,
      reset = NULL
    )

    local_env = new.env()

    #### Step 1 Check signal from main app and init ####
    observeEvent(user_data$reset, {
      local_data$checklevel = utils$get_check_level()
      rm(list = names(as.list(local_env, all.names=T)), envir = local_env)

      local_env$blocks = utils$get_blocks()
      local_env$channels = utils$get_channels()
      local_data$reset = Sys.time()
    })


    ##### step 2: util functions ####
    is_referenced = function(){
      cl = local_data$checklevel
      if(length(cl) == 0){
        cl = 1
      }
      cl >= 4
    }

    output$inputui1 <- renderUI({
      local_data$reset
      validate(need(is_referenced(), 'Please reference channels first.'))

      tagList(
        selectInput(ns('block'), 'Block', choices = local_env$blocks),
        selectInput(ns('channel'), 'Channel', choices = local_env$channels),
        div(
          actionButton(ns('prev'), 'Previous'),
          actionButton(ns('nxt'), 'Next')
        )
      )
    })
  }


  return(list(body = body, server = server))
}
