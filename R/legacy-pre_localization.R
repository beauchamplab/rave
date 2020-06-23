rave_pre_eleclocal3 <- function(module_id = 'ELECLOCAL_M', sidebar_width = 2, doc_prefix = 'ravepreprocesseleclocalization', ...){
  sidebar_width = max(sidebar_width, 4)
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  body = fluidRow(
    box(
      title = 'Viewer',
      width = 12 - sidebar_width,
      box_link = sprintf(url_format, 'output', 'viewer'),
      uiOutput(ns('eloc_outputs1'))
    ),
    box(
      width = sidebar_width,
      title = 'Electrode Table',
      box_link = sprintf(url_format, 'input', 'electrodetable'),
      uiOutput(ns('eloc_inputs1')),
      uiOutput(ns('eloc_inputs2'))
    )
  )
  
  server = function(input, output, session, user_data, utils, ...){
    
    local_data = reactiveValues()
    local_env = new.env(parent = emptyenv())
    # Init/reset data when receive reset signal
    observeEvent(user_data$reset, {
      # Reset first
      local_data$reset = Sys.time()
      local_data$has_raw_cache = utils$has_raw_cache()
      
      # Get window available height
      shinyjs::runjs(sprintf(paste(
        '(() => {',
        'let avail_width = screen.availWidth || window.outerWidth;',
        'let avail_height = screen.availHeight || window.outerHeight;',
        'avail_width = avail_width - window.outerWidth + window.innerWidth;',
        'avail_height = avail_height - window.outerHeight + window.innerHeight;',
        'Shiny.onInputChange("%s", {',
        'available_size : [ avail_width, avail_height ]',
        '});',
        '})();',
        collapse = '\n'
      ), ns('client_size')))
    })
    
    observeEvent(input$client_size, {
      client_size = input$client_size
      size = c(unlist(client_size$available_size), 900, 900)
      local_data$avail_height = size[[2]] - 200
    })
    
    output$eloc_inputs1 <- renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache, message = 'Please import subject first.'))

      dirs = utils$get_from_subject('dirs')
      sub_dir = dirs$subject_dir
      
      fs_dir = threeBrain::check_freesurfer_path(sub_dir, return_path = TRUE)
      
      if( is.null(fs_dir) ){
        re = checkboxInput(ns('use_template'), 'Use Template', value = TRUE)
      }else{
        re = checkboxInput(ns('use_template'), 'Use Template', value = FALSE)
      }
      re
    })
    output$eloc_inputs2 <- renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache, message = ''))
      tagList(
        downloadLink(ns('download_elec_table'), 'Download this table'),
        DT::dataTableOutput(ns('elec_table'))
      )
    })
    
    output$download_elec_table <- shiny::downloadHandler(
      filename = function(){
        'electrodes.csv'
      },
      content = function(con){
        tbl = local_env$elec_table
        if( nrow(tbl) ){
          tbl = tbl[order(tbl$Electrode),]
          if( isFALSE(tbl$Valid) ){
            tbl$Coord_x = tbl$Coord_y = tbl$Coord_z = 0
          }
          tbl = tbl[, c(
            'Electrode', 'Coord_x', 'Coord_y', 'Coord_z', 'Label',
            'MNI305_x', 'MNI305_y', 'MNI305_z', 'SurfaceElectrode', 
            'SurfaceType', 'Radius', 'VertexNumber', 'Hemisphere',
            'TemplateSubject', 'Valid'
          )]
          tbl$Coord_x[!tbl$Valid] = 0
          tbl$Coord_y[!tbl$Valid] = 0
          tbl$Coord_z[!tbl$Valid] = 0
          tbl$MNI305_x[!tbl$Valid] = 0
          tbl$MNI305_y[!tbl$Valid] = 0
          tbl$MNI305_z[!tbl$Valid] = 0
        }
        utils::write.csv(tbl, con, row.names = FALSE)
      }
    )
    
    output$elec_table <- DT::renderDataTable({
      dat = input$eloc_viewer_localization$table
      dat = lapply(dat, function(x){
        as.data.frame(x)
      })
      dat = do.call('rbind', dat)
      if( !(length(dat) && nrow(dat)) ){
        return(data.frame())
      }
      
      
      dat = dat[order(dat$Electrode, decreasing = TRUE),]
      dat$MNI305_RAS = sprintf('%.2f, %.2f, %.2f', dat$MNI305_x, dat$MNI305_y, dat$MNI305_z)
      dat$E = dat$Electrode
      local_env$elec_table = dat
      dat = dat[, c('E', 'MNI305_RAS', 'Valid', 'Label')]
      dat = DT::datatable(dat, rownames= FALSE, editable = 'cell', options = list(
        "columnDefs" = list(
          list( width = '40%', target = 1)
        ), 
        scrollX = TRUE
      ))
      dat
    })
    
    # proxy = DT::dataTableProxy('elec_table')
    observeEvent(input$elec_table_cell_edit, {
      info = input$elec_table_cell_edit
      # No rowname!
      info$col = info$col + 1
      tbl = local_env$elec_table
      e = tbl$E[[ info$row ]]
      label = tbl$Label[[ info$row ]]
      is_valid = tbl$Valid[[ info$row ]]
      
      if( info$col == 3 ){
        is_valid = stringr::str_sub(stringr::str_to_upper(info$value), end = 1L) == 'T'
        session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
          command = 'loc_set_electrode',
          electrode = e,
          label = label, 
          is_valid = is_valid
        ))
      }else if( info$col == 4 ){
        # Changing labels?
        lbl_nb = stringr::str_extract(info$value, '[0-9]*$')
        lbl_nb = as.integer(lbl_nb)
        if( is.na(lbl_nb) || lbl_nb <= 1 ){
          # only set this electrode
          session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
            command = 'loc_set_electrode',
            electrode = e,
            label = info$value, 
            is_valid = is_valid
          ))
        }else{
          cdat = list()
          for( ii in 1:lbl_nb ){
            r = info$row + ii - 1
            if( r <= 0 || r > nrow(tbl) ){ break() }
            
            label = tbl$Label[[ r ]]
            if( is.na(label) || label == 'NA' || label == '' ){
              e = tbl$E[[ r ]]
              
              cdat[[ii]] = list(
                electrode = tbl$E[[ r ]],
                is_valid = tbl$Valid[[ r ]],
                label = stringr::str_replace(info$value, '[0-9]*$', sprintf('%d', lbl_nb - ii + 1))
              )
            }else{
              break()
            }
          }
          
          session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
            command = 'loc_set_electrodes',
            data = cdat
          ))
        }
        
      }else{
        session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
          command = 'loc_electrode_info'
        ))
      }
    })
    

    output$eloc_outputs1 <- renderUI({
      height = c(local_data$avail_height, 700)[[1]]
      
      div(style='margin:-10px',
          threeBrain::threejsBrainOutput(ns('eloc_viewer'), height = sprintf('%.0fpx', height)))
    })
    output$eloc_viewer <- threeBrain::renderBrain({
      validate(need(local_data$has_raw_cache && length(input$use_template), message = ''))
      
      showNotification(p('Collecting data. Please wait...'), duration = NULL, id = ns('loc_noti'))
      on.exit({ shiny::removeNotification(ns('loc_noti')) })
      
      brain = NULL
      if( !isTRUE(input$use_template) ){
        try({
          dirs = utils$get_from_subject('dirs')
          sub_dir = dirs$subject_dir
          path = sub_dir
          scode = utils$get_from_subject('subject_code')
          brain = threeBrain::freesurfer_brain2(
            fs_subject_folder = path, 
            subject_name = scode, 
            surface_types = c('white', 'pial-outer-smoothed'))
        }, silent = TRUE )
        
      }
      if( is.null(brain) ){
        scode = getOption('threeBrain.template_subject', 'N27')
        path = file.path(getOption('threeBrain.template_dir', '~/rave_data/others/three_brain'), scode)
        brain = threeBrain::freesurfer_brain2(
          fs_subject_folder = path,
          subject_name = scode, 
          surface_types = c('white', 'pial-outer-smoothed'))
      }
      
      height = min(ceiling((c(local_data$avail_height, 700)[[1]] - 100) /3), 300)
      
      brain$plot(control_presets = 'electrode_localization', side_width = height)
    })
  }
  
  return(list(
    body = body,
    server = server
  ))
}
