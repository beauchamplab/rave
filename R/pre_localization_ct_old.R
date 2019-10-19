rave_pre_eleclocalct3_old <- function(module_id = 'ELECLOCALCT_M', sidebar_width = 2){
  sidebar_width = max(sidebar_width, 4)
  ns = shiny::NS(module_id)
  
  body = fluidRow(
    box(
      title = 'Viewer',
      width = 12 - sidebar_width,
      uiOutput(ns('eloc_outputs1'))
    ),
    box(
      width = sidebar_width,
      title = 'Electrode Table',
      uiOutput(ns('eloc_inputs1')),
      uiOutput(ns('eloc_inputs2'))
    )
  )
  
  server = function(input, output, session, user_data, utils){
    
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
    
    output$eloc_inputs1 <- shiny::renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache, message = 'Please import subject first.'))
      
      dirs = utils$get_from_subject('dirs')
      sub_dir = dirs$subject_dir
      
      fs_dir = threeBrain::check_freesurfer_path(sub_dir, return_path = TRUE)
      if( is.null(fs_dir) ){
        return(div('Cannot find `fs` directory. Please make sure FreeSurfer directory exists', 
                   class = c("shiny.silent.error", "validation")))
      }
      
      
      shinyFiles::shinyFileChoose(
        input, 'ct_path', roots = c(home = fs_dir), filetypes = c('nii', 'gz'),
        defaultRoot = 'home')
      shinyFiles::shinyFilesButton(
        id = ns('ct_path'), label = "Locate CT (aligned)", 
        title = "Choose a Nifti CT aligned to T1", multiple = FALSE)
      
    })
    
    
    
    output$eloc_inputs2 <- shiny::renderUI({
      local_data$reset
      
      if( !local_data$has_raw_cache || !(length(input$ct_path) > 1) || !length(input$ct_path$files)){
        return('')
      }
      htmltools::tagList(
        p(
          shiny::downloadLink(ns('download_elec_table'), 'Download this table'),
          ' ', shiny::textOutput(ns('total_electrodes'))
        ),
        DT::dataTableOutput(ns('elec_table'))
      )
    })
    
    output$total_electrodes <- renderText({
      tbl = local_data$elec_table
      if( !is.data.frame(tbl) ){ return('') }
      total = nrow(tbl)
      valid = sum(c(tbl$Valid, FALSE))
      sprintf('%d (valid) of %d (total)', valid, total)
    })
    
    output$download_elec_table <- shiny::downloadHandler(
      filename = function(){
        'electrodes.csv'
      },
      content = function(con){
        tbl = shiny::isolate(local_data$elec_table)
        if( nrow(tbl) ){
          tbl = tbl[order(tbl$Electrode),]
          if( isFALSE(tbl$Valid) ){
            tbl$Coord_x = tbl$Coord_y = tbl$Coord_z = 0
          }
          tbl = tbl[, c(
            'Electrode', 'Coord_x', 'Coord_y', 'Coord_z', 'Label',
            'MNI305_x', 'MNI305_y', 'MNI305_z', 'SurfaceElectrode', 
            'SurfaceType', 'Radius', 'VertexNumber', 'Hemisphere',
            'TemplateSubject', 'Valid', 'DistanceToSurface'
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
    
    
    observeEvent(input$eloc_viewer_ct_threshold, {
      signal = input$eloc_viewer_ct_threshold
      brain = local_env$brain
      current_subject = signal$current_subject #'YAB'
      thred = signal$threshold
      n_elec = length(utils$get_from_subject('channels'))
      if( 'rave-brain' %in% class(brain) && !is.null(brain$volumes$ct.aligned.t1) ){
        progress = progress(title = 'Guessing electrodes', max = 4)
        on.exit({ progress$close() })
        progress$inc('Fetching volume data...')
        ct_data = brain$volumes$ct.aligned.t1$object$get_data(
          sprintf('datacube_value_ct.aligned.t1 (%s)', current_subject))
        size = brain$volumes$ct.aligned.t1$object$get_data(
          sprintf('datacube_dim_ct.aligned.t1 (%s)', current_subject))
        dim(ct_data) = size
        
        progress$inc('Thresholding...')
        xyz = t(which(ct_data >= thred, arr.ind = TRUE)) - size/2
        xyz = t(xyz)
        progress$inc('Clustering')
        hclust = stats::hclust( dist(xyz), method = 'single')
        k = ceiling(n_elec * 1.2)
        cuts = stats::cutree(hclust, k = k)
        
        progress$inc('Generating results...')
        centers = sapply(sort(unique(cuts)), function(ii){
          pos = colMeans(xyz[cuts == ii,, drop = FALSE])
          names(pos) = NULL
          pos
        })
        
        # sort centers by RAS
        centers = centers[, order(centers[1, ] + centers[2, ] + centers[3, ]), drop = FALSE]
        centers = lapply(seq_len(ncol(centers)), function(ii){
          pos = centers[,ii]
          list(
            electrode = ii,
            label = 'NA',
            position = pos,
            is_surface = TRUE,
            surface_type = 'pial'
          )
        })
        
        
        session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
          command = 'loc_add_electrodes',
          data = centers
        ))
        
      }
    })
    
    observe({
      dat = input$eloc_viewer_localization$table
      
      dat = lapply(dat, function(x){
        x = as.data.frame(x, stringsAsFactors = FALSE)
        if( is.null(x$DistanceToSurface) ){
          x$DistanceToSurface = 999
        }
        if( !length(x$Label) || is.na(x$Label) || x$Label %in% c('', 'na', 'NA')){ x$Label = '' }
        x
      })
      
      dat = do.call('rbind', dat)
      
      
      if( !(length(dat) && nrow(dat)) ){
        return(data.frame())
      }
      
      
      
      dat$MNI305_RAS = sprintf('%.2f, %.2f, %.2f', dat$MNI305_x, dat$MNI305_y, dat$MNI305_z)
      dat$E = dat$Electrode
      dat$Dist = dat$DistanceToSurface
      dat = dat[order(dat$Valid, dat$Dist, dat$Electrode, decreasing = TRUE), ]
      local_data$elec_table = dat
      
      
    })
    
    output$elec_table <- DT::renderDataTable({
      dat = local_data$elec_table
      if( !is.data.frame(dat) || !nrow(dat) ){
        return()
      }
      dat = dat[, c('E', 'MNI305_RAS', 'Valid', 'Label', 'Dist')]
      
      dat = DT::datatable(dat, rownames= FALSE, editable = 'cell', options = list(
        "columnDefs" = list(
          list( width = '40%', target = 1)
        ), 
        # pageLength = max(5, nrow(dat)),
        scrollX = TRUE
      ))
      
      DT::formatRound(dat, columns=c('Dist'), digits=1)
    })
    
    # proxy = DT::dataTableProxy(ns('elec_table'))
    observeEvent(input$eloc_viewer_mouse_clicked, {
      signal = input$eloc_viewer_mouse_clicked
      dat = shiny::isolate(local_data$elec_table)
      
      if( is.data.frame(dat) && nrow(dat) && is.list(signal) && isTRUE(signal$is_electrode)) {
        e = signal$electrode_number
        sel = dat$Electrode %in% e
        dat = rbind(dat[sel,], dat[!sel,])
        local_data$elec_table = dat
      }
    })
    
    observeEvent(input$elec_table_cell_edit, {
      info = input$elec_table_cell_edit
      # No rowname!
      info$col = info$col + 1
      tbl = shiny::isolate(local_data$elec_table)
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
        # only set this electrode
        session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
          command = 'loc_set_electrode',
          electrode = e,
          label = info$value, 
          is_valid = is_valid
        ))
        
      }else{
        session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
          command = 'loc_electrode_info'
        ))
      }
    })
    
    
    output$eloc_outputs1 <- shiny::renderUI({
      height = c(local_data$avail_height, 700)[[1]]
      
      div(style='margin:-10px',
          threeBrain::threejsBrainOutput(ns('eloc_viewer'), height = sprintf('%.0fpx', height)))
    })
    output$eloc_viewer <- threeBrain::renderBrain({
      validate(need(local_data$has_raw_cache, message = ''),
               need((length(input$ct_path) > 1) && length(input$ct_path$files), message = ''))
      local_env$brain = NULL
      
      sub_dir = utils$get_from_subject('dirs')$subject_dir
      scode = utils$get_from_subject('subject_code')
      fs_dir = threeBrain::check_freesurfer_path(sub_dir, return_path = TRUE)
      ct_path = do.call(file.path, c(list(fs_dir), input$ct_path$files[[1]]))
      
      cache = file.path(fs_dir, 'RAVE', sprintf('%s_ct_aligned_t1.json', scode))
      if( length(cache) == 1 && file.exists(cache) ){ unlink(cache) }
      
      showNotification(p('Collecting data. Please wait...'), duration = NULL, id = ns('loc_noti'))
      on.exit({ shiny::removeNotification(ns('loc_noti')) })
      
      brain = threeBrain::freesurfer_brain(
        sub_dir, scode, additional_surfaces = 'pial-outer-smoothed', aligned_ct = ct_path)
      
      height = min(ceiling((c(local_data$avail_height, 700)[[1]] - 100) /3), 300)
      
      local_env$brain = brain
      
      
      brain$plot(control_presets = c('electrode_localization', 'ct_visibility'), side_width = height)
      
    })
  }
  
  return(list(
    body = body,
    server = server
  ))
}
