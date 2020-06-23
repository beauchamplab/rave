rave_pre_eleclocalct3 <- function(module_id = 'ELECLOCALCT_M', sidebar_width = 2, doc_prefix = 'ravepreprocesseleclocalizationct', ...){
  sidebar_width = max(sidebar_width, 4)
  ns = shiny::NS(module_id)
  REGEX_POS = '^[ ]*([-]{0,1}[0-9]+[.]{0,1}[0-9]*)[ ]*,[ ]*([-]{0,1}[0-9]+[.]{0,1}[0-9]*)[ ]*,[ ]*([-]{0,1}[0-9]+[.]{0,1}[0-9]*)[ ]*$'
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  body = fluidRow(
    box(
      title = 'Viewer',
      width = 12 - sidebar_width,
      box_link = sprintf(url_format, 'output', 'viewer'),
      uiOutput(ns('eloc_outputs1'))
    ),
    tabBox(
      width = sidebar_width,
      title = 'Controls',
      box_link = sprintf(url_format, 'input', 'controls'),
      tabPanel(
        title = 'Localization Method',
        div(
          style = 'max-height:85vh; overflow-y:scroll; padding-bottom: 80px',
          shiny::uiOutput(ns('eloc_inputs1')),
          shiny::uiOutput(ns('eloc_inputs2')),
          shiny::uiOutput(ns('eloc_inputs3'))
        )
      ),
      tabPanel(
        title = 'Electrode Table',
        uiOutput(ns('eloc_outputs2')),
        uiOutput(ns('eloc_outputs3'))
      )
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
      local_data$ct_path = NULL
      local_data$has_brain = NULL
      local_data$trajectories = list()
      local_data$traj_size = 0
      local_data$traj_changed = Sys.time()
      local_data$ct_data = NULL
      local_data$electrode_locations = list()
      
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
      
      shinyFiles::shinyFileChoose(
        input, 'ct_path', roots = c(home = sub_dir), filetypes = c('nii', 'gz'),
        defaultRoot = 'home')
      
      # give options ti choose from:
      # 1. via template
      # 2. via local subject
      # 2 a) overlay CT
      # 2 b) add trajectory (rosa...)
      
      div(
        class = 'rave-grid-inputs',
        div(
          style = 'flex-basis:70%;',
          selectInput(ns('mri_subject'), 'Choose MRI source', choices = c(
            'Local subject', 'Local subject + CT Overlay', 'Template brain'
          ), selected = 'Local subject')
        ),
        div(
          style = 'flex-basis:30%; min-height: 55px;',
          conditionalPanel(
            'input.mri_subject === "Local subject + CT Overlay"', ns = ns, 
            shinyFiles::shinyFilesButton(
              id = ns('ct_path'), label = "CT (aligned)", 
              title = "Choose a Nifti CT aligned to T1", multiple = FALSE, style = 'width:100%')
          )
        ),
        div(
          style = 'flex-basis:100%;',
          dipsaus::actionButtonStyled(ns('mri_load'), 'Load Data', type = 'primary')
        )
      )
    })
    
    # Source of CT location
    observeEvent(input$ct_path, {
      ct_path_info = input$ct_path
      if( local_data$has_raw_cache && is.list(ct_path_info) && length(ct_path_info$files) ){
        dirs = utils$get_from_subject('dirs')
        path = do.call(file.path, c(list(dirs$subject_dir), ct_path_info$files[[1]]))
        local_data$ct_path = normalizePath(path, mustWork = TRUE)
      }
    })
    
    
    
    # Load MRI data with CT
    observeEvent(input$mri_load, {
      local_data$reset
      mri_subject = input$mri_subject
      switch (mri_subject,
        'Template brain' = {
          scode = getOption('threeBrain.template_subject', 'N27')
          path = file.path(getOption('threeBrain.template_dir', '~/rave_data/others/three_brain'), scode)
          local_env$brain = threeBrain::freesurfer_brain2(
            fs_subject_folder = path, subject_name = scode, 
            surface_types = c('white', 'pial-outer-smoothed'))
        },
        'Local subject' = {
          dirs = utils$get_from_subject('dirs')
          sub_dir = dirs$subject_dir
          scode = utils$get_from_subject('subject_code')
          local_env$brain = threeBrain::freesurfer_brain2(
            fs_subject_folder = sub_dir, subject_name = scode,
            surface_types = c('white', 'pial-outer-smoothed'), aligned_ct = NULL)
        }, {
          dirs = utils$get_from_subject('dirs')
          sub_dir = dirs$subject_dir
          scode = utils$get_from_subject('subject_code')
          fs_dir = threeBrain::check_freesurfer_path(sub_dir, check_volume = TRUE, return_path = TRUE)
          if(!is.null(fs_dir)){
            ct_cache = file.path(fs_dir, 'RAVE', sprintf('%s_ct_aligned_t1.json', scode))
            if( file.exists(ct_cache) ){
              unlink(ct_cache)
            }
          }
          showNotification(p('Generating brain... It will take a while'), 
                           duration = NULL, id = ns('ct_id'))
          local_env$brain = threeBrain::freesurfer_brain2(
            fs_subject_folder = sub_dir, subject_name = scode,
            additional_surfaces = c('white', 'pial-outer-smoothed'), 
            ct_path = local_data$ct_path)
          removeNotification(id = 'ct_id')
          # Load into memory
          tryCatch({
            group = local_env$brain$volumes$ct.aligned.t1$group
            cube = group$get_data(sprintf('datacube_value_ct.aligned.t1 (%s)', scode))  
            dim = group$get_data(sprintf('datacube_dim_ct.aligned.t1 (%s)', scode))
            dim(cube) = dim
            local_data$ct_data = cube
          }, error = function(e){
            local_data$ct_data = NULL
          })
          
          
        }
      )
      local_data$has_brain = Sys.time()
    })

    output$eloc_inputs2 <- shiny::renderUI({
      local_data$reset
      validate(need(local_data$has_raw_cache && !is.null(local_data$has_brain), message = ''))
      
      div(
        class = 'rave-grid-inputs',
        div(
          style = 'flex-basis: 60%; min-height: 100px;',
          selectInput(ns('traj_method'), 'Add trajectory method', c(
            'Straight Line', 'Spline', 'Grid', 'ROSA File'
          ))
        ),
        div(
          style = 'flex-basis: 40%; min-height: 100px;', 
          conditionalPanel(
            'input.traj_method === "ROSA File"', ns = ns,
            fileInput(ns('add_rosa'), 'Upload', width = '100%')
          ),
          conditionalPanel(
            'input.traj_method !== "ROSA File"', ns = ns,
            div(
              style = 'display: inline-grid',
              tags$label('Create new'),
              actionButton(ns('add_manual'), 'Add', width = '100%')
            )
          )
        )
      )
    })
    
    observeEvent(input$add_rosa, {
      path = input$add_rosa$datapath
      brain = local_env$brain
      if( is.null(brain) ){
        showNotification(p('Please load 3D viewer first'), type = 'error')
        return()
      }
      scanner2ras = matrix(c(1,0,0,0,0,1,0,0,0,0,1,-6,0,0,0,1), nrow = 4, byrow = TRUE)#brain$Torig %*% solve( brain$Norig )
      
      tryCatch({
        rosa = read.rosa(path)
        traj_names = names(rosa$trajectory)
        c_l = shiny::isolate(local_data$traj_size)
        lapply(seq_along(traj_names), function(ii){
          meta = traj_names[[ii]]
          anc = rosa$trajectory[[ii]]
          dir = anc$end - anc$start; n_elec = max(1, norm(dir, '2') / 3.5)
          
          # Transform: scanner - tksurfer: ScannerRAS = Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
          # ROSA is using LPS instead of RAS
          start = (scanner2ras %*% c(anc$start * c(-1,-1,1), 1))[1:3] 
          end = (scanner2ras %*% c(anc$end * c(-1,-1,1), 1))[1:3] 
          start_anchor = sprintf('%.3f,%.3f,%.3f', start[1], start[2], start[3])
          end_anchor = sprintf('%.3f,%.3f,%.3f', end[1], end[2], end[3])
          
          local_data$trajectories[[c_l + ii]] = list(
            type = 'Straight Line',
            size = c(floor(n_elec),1),
            space = 3.5,
            anchor = c(start_anchor, end_anchor, '0,0,0', '0,0,0'),
            meta = meta,
            color = c_l + ii
          )
          local_data$traj_size = c_l + ii
          make_trajectory( c_l + ii )
          update_trajectory( c_l + ii )
        })
      }, error = function(e){
        print(traceback(e))
      })
      local_data$traj_changed = Sys.time()
    })
    
    observeEvent(input$add_manual, {
      traj_method = input$traj_method
      traj = shiny::isolate(local_data$trajectories)
      c_l = shiny::isolate(local_data$traj_size) + 1
      traj[[ c_l ]] = list(
        type = traj_method,
        size = c(1,1),
        space = 10,
        end_points = NULL,
        anchor = c('0,0,0', '0,0,0', '0,0,0', '0,0,0'),
        color = c_l
      )
      local_data$trajectories = traj
      local_data$traj_size = c_l
      local({
        make_trajectory( c_l )
      })
      local_data$traj_changed = Sys.time()
    })
    
    gen_warn = function(msg){
      tags$small( style = 'color: red', msg )
    }
    update_trajectory = function(ii){
      traj = local_data$trajectories[[ii]]
      if( !is.list(traj) ){ return() }
      anchors = unlist(traj$anchor)
      anchors = as.numeric(stringr::str_match(anchors, REGEX_POS)[,2:4])
      space = traj$space
      width = traj$size[[1]]
      
      
      if( !length(anchors) || any(is.na(anchors)) ){
        showNotification(p('Anchors are invalid. Please make sure they are numeric.'), type = 'error')
        return()
      }
      dim(anchors) = c(length(anchors) / 3, 3)
      
      has_ct = isTRUE(input$mri_subject == "Local subject + CT Overlay") && !is.null(local_data$ct_data)
      
      if( traj$type == 'Straight Line' ){
        # Get anchor 1 and 2, and space
        start = anchors[1,]
        end = anchors[2,]
        dir = end - start; dir = dir/ norm(dir, '2')
        
        if( width > length(space) + 1 ){
          space = c(space, rep(space[length(space)], width - length(space)-1))
        }
        space = cumsum(c(0, space))
        
        local_data$electrode_locations[[ii]] = lapply(space, function(s){ start + dir * s })
        # If CT exists, try to attach to CT
      }else{
        print('TODO: update traj ii')
      }
    }
    make_trajectory = function(ii){
      # Remove trajectory
      observeEvent(input[[sprintf('traj_remove_%d', ii)]], {
        v = input[[sprintf('traj_remove_%d', ii)]]
        if( !is.null(v) && v > 0 ){
          local_data$trajectories[[ii]] = ''
          local_data$electrode_locations[[ii]] = list()
          local_data$traj_changed = Sys.time()
        }
      })
      
      # Show error message
      output[[sprintf('traj_msg_%d', ii)]] <- renderUI({
        
        if( local_data$traj_size >= ii ){
          traj = shiny::isolate({local_data$trajectories[[ii]]})
          if( !is.list(traj) ){ return() }
        }else{
          return()
        }
        
        changed = FALSE
        traj_type = traj$type
        # type = traj_method,
        # size = c(1,1),
        # space = 10,
        # end_points = NULL,
        # anchor = c('0,0,0', '0,0,0', '0,0,0', '0,0,0')
        # Step 1: check anchor
        anc = sapply(1:4, function(jj){
          input[[sprintf('traj_anchor%s_%s', jj, ii)]]
        })
        anc = unlist(anc)
        if( !length(anc) ){
          # Higly impossible, but just in case
          return(gen_warn('No anchor found'))
        }
        # Input exist
        pass = stringr::str_detect(anc, REGEX_POS)
        notpass = which(!pass)
        if(length(notpass)){
          return(gen_warn(sprintf('Wrong position (%s)', paste(notpass, collapse = ','))))
        }else{
          changed = TRUE
          traj$anchor[seq_along(anc)] = anc
        }
        # Step 2: check space (numeric)
        space = input[[sprintf('traj_space_%d', ii)]]
        space = as.numeric(stringr::str_split(space, '[^0-9.]+')[[1]])
        if( traj_type %in% c('Straight Line', 'Spline') ){
          if( !length(space) || any(is.na(space)) || !is.numeric(space) || any(space <= 0) ){
            return(gen_warn('Invalid spacing'))
          }else{
            changed = TRUE
            traj$space = space
          }
        }
        
        # Step 3: check width & height
        width = input[[sprintf('traj_width_%d', ii)]]
        height = input[[sprintf('traj_height_%d', ii)]]
        if( !length(width) || is.na(width) || !is.numeric(width) || width <= 0 ){
          return(gen_warn('Invalid size'))
        }else{
          changed = TRUE
          traj$size[[1]] = ceiling(width)
        }
        if( traj_type %in% c('Grid') ){
          if( !length(height) || is.na(height) || !is.numeric(height) || height <= 0 ){
            return(gen_warn('Invalid size'))
          }else{
            changed = TRUE
            traj$size[[2]] = ceiling(height)
          }
        }
        
        if( changed ){
          local_data$trajectories[[ii]] = traj
        }
        return(actionLink(ns(sprintf('traj_update_%d', ii)), 'Update'))
      })
      
      observeEvent(input[[sprintf('traj_update_%d', ii)]], {
        
        update_trajectory(ii)
        
      })
    }
    
    
    observeEvent(local_data$electrode_locations, {
      locs = local_data$electrode_locations
      centers = lapply(seq_along(locs), function(ii){
        traj = local_data$trajectories[[ ii ]]
        if(!is.list(traj)){ return(NULL) }
        is_surface = traj$type %in% c('Spline', 'Grid')
        col = dipsaus::col2hexStr(traj$color + 1, prefix = '#')
        loc = locs[[ ii ]]
        
        lapply(loc, function(center){
          list(
            position = center,
            label = 'NA',
            is_surface = is_surface,
            surface_type = 'pial',
            color = col
          )
        })
        
      })
      
      centers = unlist(centers, recursive = F)
      centers = lapply(seq_along(centers), function(ii){
        x = centers[[ii]]
        x$electrode = ii
        x
      })
        
        
      session$sendCustomMessage(ns('eloc_viewer__shiny'), list(
        command = 'loc_add_electrodes',
        data = centers,
        reset = TRUE
      ))
    })
    
    
    output$eloc_inputs3 <- renderUI({
      local_data$traj_changed
      # type, end-points, size, space, meta
      traj = shiny::isolate(local_data$trajectories)
      lapply(rev(seq_along(traj)), function(ii){
        el = traj[[ii]]
        if(!is.list(el)){
          return(NULL)
        }
        ui1s = list(
          tagList(
            div(
              style = 'flex-basis: 27%; min-height: 75px;',
              numericInput(ns(sprintf('traj_width_%d', ii)), 'Size: Width', step = 1, min = 1, value = el$size[1], width = '100%')
            ), 
            div(
              style = 'flex-basis: 27%; min-height: 75px;',
              numericInput(ns(sprintf('traj_height_%d', ii)), 'x Height', step = 1, min = 1, value = el$size[2], width = '100%')
            )
          ),
          tagList(
            div(
              style = 'flex-basis: 27%; min-height: 75px;',
              textInput(ns(sprintf('traj_space_%d', ii)), 'Space (mm)', value = paste(el$space, collapse = ','), width = '100%', placeholder = 'e.g. 2,3')
            ), 
            div(
              style = 'flex-basis: 27%; min-height: 75px;',
              numericInput(ns(sprintf('traj_width_%d', ii)), 'Size', min = 1, value = el$size[1], width = '100%')
            )
          )
          
        )
        ui2s = list(
          tagList(lapply(1:4, function(jj){
            div(
              style = 'flex-basis: 25%',
              textInput(ns(sprintf('traj_anchor%d_%d', jj, ii)), sprintf('Anchor %d', jj), width = '100%', placeholder = '0,0,0', value = el$anchor[[jj]])
            )
          })),
          tagList(lapply(1:2, function(jj){
            div(
              style = 'flex-basis: 50%',
              textInput(ns(sprintf('traj_anchor%d_%d', jj, ii)), sprintf('Anchor %d', jj), width = '100%', placeholder = '0,0,0', value = el$anchor[[jj]])
            )
          }))
        )
        switch (el$type,
          'Grid' = {
            ui1 = ui1s[[1]]
            ui2 = ui2s[[1]]
          }, 
          'Spline' = {
            ui1 = ui1s[[2]]
            ui2 = ui2s[[1]]
          }, {
            ui1 = ui1s[[2]]
            ui2 = ui2s[[2]]
          }
        )
        
        col = dipsaus::col2hexStr(el$color + 1)
        
        div(
          class = 'rave-grid-inputs no-border-inputs',
          div(
            style = 'flex-basis: 45%; min-height: 75px;',
            h4(style = sprintf('color:%s', col), sprintf('Trajectory %d', ii)),
            tags$small(sprintf('[%s]%s', el$type, ifelse(is.null(el$meta), '', paste0(' (', el$meta, ')')))),
            div(
              style = 'display: inline-flex;',
              actionLink(ns(sprintf('traj_remove_%d', ii)), 'Remove'),
              span(' | ', style = 'display:block;padding:0 5px;'),
              uiOutput(ns(sprintf('traj_msg_%d', ii)))
            )
          ),
          ui1,
          hr(),
          ui2
        )
      })
    })
    
    output$eloc_outputs1 <- shiny::renderUI({
      height = c(local_data$avail_height, 700)[[1]]
      
      div(style='margin:-10px',
          threeBrain::threejsBrainOutput(ns('eloc_viewer'), height = sprintf('%.0fpx', height)))
    })
    
    output$eloc_viewer <- threeBrain::renderBrain({
      validate(need(local_data$has_raw_cache, message = ''),
               need(length(local_data$has_brain), message = ''))
      brain = local_env$brain
      
      if( is.null(brain) ){
        return(NULL)
      }
      
      height = min(ceiling((c(local_data$avail_height, 700)[[1]] - 100) /3), 300)
      
      brain$plot(control_presets = c('electrode_localization', 'ct_visibility'), 
                 side_width = height, side_display = FALSE)
    })
    
    # 
    # observeEvent(input$ct_path, {
    #   # load CT files
    #   validate(need(local_data$has_raw_cache, message = ''),
    #            need((length(input$ct_path) > 1) && length(input$ct_path$files), message = ''))
    #   local_env$brain = NULL
    #   
    #   sub_dir = utils$get_from_subject('dirs')$subject_dir
    #   scode = utils$get_from_subject('subject_code')
    #   fs_dir = threeBrain::check_freesurfer_path(sub_dir, return_path = TRUE)
    #   ct_path = do.call(file.path, c(list(fs_dir), input$ct_path$files[[1]]))
    #   
    #   cache = file.path(fs_dir, 'RAVE', sprintf('%s_ct_aligned_t1.json', scode))
    #   if( length(cache) == 1 && file.exists(cache) ){ unlink(cache) }
    #   
    #   showNotification(p('Collecting data. Please wait...'), duration = NULL, id = ns('loc_noti'))
    #   on.exit({ shiny::removeNotification(ns('loc_noti')) })
    #   
    #   brain = threeBrain::freesurfer_brain(
    #     sub_dir, scode, additional_surfaces = 'pial-outer-smoothed', aligned_ct = ct_path)
    # })
    # 
    # output$eloc_inputs3 <- shiny::renderUI({
    #   
    # })
    # 
    
    # output$eloc_outputs2 <- shiny::renderUI({
    #   local_data$reset
    # 
    #   validate(need(local_data$has_raw_cache, message = 'Please import subject first.'))
    #   dirs = utils$get_from_subject('dirs')
    #   sub_dir = dirs$subject_dir
    # 
    #   fs_dir = threeBrain::check_freesurfer_path(sub_dir, return_path = TRUE)
    #   if( is.null(fs_dir) || isFALSE(fs_dir) ){
    #     return(div('Cannot find `fs` directory. Please make sure FreeSurfer directory exists',
    #                class = c("shiny.silent.error", "validation")))
    #   }
    # 
    #   print(fs_dir)
    # 
    #   shinyFiles::shinyFileChoose(
    #     input, 'ct_path', roots = c(home = fs_dir), filetypes = c('nii', 'gz'),
    #     defaultRoot = 'home')
    #   shinyFiles::shinyFilesButton(
    #     id = ns('ct_path'), label = "Locate CT (aligned)",
    #     title = "Choose a Nifti CT aligned to T1", multiple = FALSE)
    # 
    # })
    # 
    # 
    # 
    output$eloc_outputs3 <- shiny::renderUI({
      local_data$reset

      if( !(local_data$has_raw_cache) || is.null(local_data$has_brain) ){
        return('')
      }
      htmltools::tagList(
        p(
          shiny::downloadLink(ns('download_elec_table'), 'Download this table'),
          hr(), shiny::textOutput(ns('total_electrodes'))
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
        if( is.data.frame(tbl) && nrow(tbl) ){
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
      n_elec = signal$n_electrodes
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
        hclust = stats::hclust( stats::dist(xyz), method = 'single')
        k = ceiling(n_elec)
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
          data = centers,
          reset = TRUE
        ))
        
      }
    })
    
    observe({
      dat = input$eloc_viewer_localization$table
      
      dat = lapply(dat, function(x){
        x = as.data.frame(x, stringsAsFactors = FALSE)
        if( is.null(x$DistanceToSurface) ){
          x$DistanceToSurface = -1
        }
        if( !length(x$Label) || is.na(x$Label) || x$Label %in% c('', 'na', 'NA')){ x$Label = '' }
        x
      })
      
      dat = do.call('rbind', dat)
      
      
      if( !(length(dat) && nrow(dat)) ){
        return(data.frame())
      }
      
      
      
      dat$RAS = sprintf('%.2f, %.2f, %.2f', dat$Coord_x, dat$Coord_y, dat$Coord_z)
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
      dat = dat[, c('E', 'RAS', 'Valid', 'Label', 'Dist')]
      
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
    
    
    
    
  }
  
  return(list(
    body = body,
    server = server
  ))
}
