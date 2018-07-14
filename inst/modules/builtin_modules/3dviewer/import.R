# In charge of 'Data Import' section

data_picker = function(){
  # name                        size              type              datapath
  # local_sparse_function.md    2419              text/markdown     ...
  tagList(
    shiny::fileInput(
      inputId = ns('file'),
      label = 'Upload a file',
      multiple = F,
      placeholder = ''
    ),
    shiny::textInput(ns('data_name'), 'Data Name'),
    hr(),
    div(
      div(
        style = 'max-width:80px; float:right',
        actionLink(ns('import'), 'Import')
      )
    )
  )
}


observeEvent(input$file, {
  infile = input$file
  # check file
  if(!is.null(infile)){
    tryCatch({
      name = stringr::str_remove_all(infile$name, '(\\..*$)|[\\W_]')
      header = unlist(read.csv(infile$datapath, header = F, row.names = 1, stringsAsFactors = F, nrows = 1))
      body = read.csv(infile$datapath, header = F, row.names = 1, stringsAsFactors = F, skip = 1)
      names(header) = NULL

      electrodes = as.integer(row.names(body))

      sel = electrodes %in% subject$filter_all_electrodes(electrodes)
      body = body[sel, ]
      electrodes = electrodes[sel]

      if(is.numeric(header)){
        # This is animation and header is keyframes
        type = 'animation'
      }else{
        # This is static and header is variable name
        type = 'static'
      }


      # env$masks[[name]]
      local_data$to_be_imported = list(
        name = name,
        header = header,
        body = body,
        type = type,
        electrodes = electrodes,
        valid = electrodes %in% subject$valid_electrodes,
        loaded = electrodes %in% preload_info$electrodes,
        cached = FALSE
      )

      updateTextInput(session, 'data_name', value = name)

      # save mask
      # module_tools$save_subject_data(data = env$masks, name = 'file_list', path = .module_path)
      # local_data$mask_name = name
    }, error = function(e){
      print(e)
    })

  }
})




observeEvent(input$data_name, {
  old = input$data_name
  new = stringr::str_remove_all(old, '[\\W_]')
  new = stringr::str_sub(new, end = 20)
  if(new != old){
    updateTextInput(session, 'data_name', value = new)
  }
})


observeEvent(input$import, {
  name = input$data_name
  name = stringr::str_sub(stringr::str_remove_all(name, '[\\W_]'), end = 20)
  current_mask = local_data$to_be_imported
  if(is.blank(name)){
    showNotification(p('Name cannot be blank!'), id = ns(.module_id), type = 'error')
  }else if(!is.list(current_mask) || is.null(current_mask$cached) || !is.character(current_mask$name)){
    showNotification(p('Invalid mask file! Have you imported any file yet?'), id = ns(.module_id), type = 'error')
  }else if(current_mask$cached){
    showNotification(p(sprintf(
      'This mask has already been imported by [%s]', current_mask$name
    )), id = ns(.module_id), type = 'message')
    updateTextInput(session, 'data_name', value = current_mask$name)
  }else{
    # valid mask, import
    current_mask$name = name
    current_mask$cached = TRUE
    env$masks[[name]] = current_mask
    local_data$to_be_imported = current_mask
    module_tools$save_subject_data(data = env$masks, name = 'file_list', path = .module_path)
    local_data$mask_name = name
    local_data$refresh_controller = Sys.time()
    showNotification(p(sprintf(
      'Import succeed! [%s]', name
    )), id = ns(.module_id), type = 'message')
  }
})
