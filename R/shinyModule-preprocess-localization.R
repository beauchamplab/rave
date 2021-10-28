pre_localization <- function(module_id = 'LOCALIZATION_M', sidebar_width = 2, doc_prefix = 'ravepreprocesseleclocalization', ...){
  ns = shiny::NS(module_id)
  
  url_format = sprintf('https://openwetware.org/wiki/RAVE:ravepreprocess:%s:%%s_%%s', doc_prefix)
  
  # module <- threeBrain::localization_module('N27', file.path(threeBrain::default_template_directory(), 'N27'))
  fslut_json <-
    system.file("palettes",
                "datacube2",
                "FreeSurferColorLUT.json",
                package = "threeBrain")
  cmap <- threeBrain::load_colormap(fslut_json)
  cmap <- do.call("rbind",
                  lapply(cmap$map, as.data.frame,
                         stringAsFactors = FALSE))
  
  body = fluidRow(
    box(
      width = 12L,
      title = '3D Viewer',
      box_link = sprintf(url_format, 'output', '3dviewer'),
      threeBrain::threejsBrainOutput(ns("viewer"), width = "100%", height = "70vh")
    ),
    box(
      width = 12L,
      title = 'Electrode Table',
      box_link = sprintf(url_format, 'output', 'electrodetable'),
      
      dipsaus::flex_div(
        shiny::fileInput(ns("load"), dipsaus::html_asis(" "), buttonLabel = "Import & add", width = "100%"), 
        shiny::actionButton(ns("refresh"), "Validate & Update table", width = "100%"), 
        shiny::actionButton(ns("clear"), "Clear table", width = "100%"),
        shiny::downloadButton(ns("save"), "Export", style = "width: 100%;"),
        
        shiny::selectizeInput(ns("fslut"), "FreeSurfer look-up", choices = NULL, multiple = FALSE, selected = character(0), width = "100%"),
        shiny::textInput(ns("fslutid"), "Index code", value = "", placeholder = "FreeSurfer index code will be displayed here...", width = "100%"),
        ncols = 6L
      ),
      DT::DTOutput(ns("table"), width = "100%")
    )
  )
  
  server = function(input, output, session, user_data, utils, ...){
    shiny::updateSelectizeInput(
      session = session,
      inputId = "fslut",
      choices = cmap$Label,
      server = TRUE
    )
    
    proxy_brain <- threeBrain::brain_proxy(outputId = "viewer", session = session)
      
    local_reactive <- shiny::reactiveValues(
      table = NULL,
      subject_id = ''
    )
    local_env = dipsaus::fastmap2()
    
    localize <- function(){
      if(!isTRUE(local_reactive$subject_id != "")){ return() }
      sid <- local_reactive$subject_id
      subject <- raveio::as_rave_subject(sid, strict = FALSE)
      
      local_env$brain <- NULL
      if(!is.na(subject$freesurfer_path)){
        local_env$brain <- 
          threeBrain::freesurfer_brain2(
            fs_subject_folder = subject$freesurfer_path,
            subject_name = subject$subject_code,
            surface_types = c("pial", 'white')
          )
      }
      
      if (is.null(local_env$brain)) {
        local_env$brain <- threeBrain::merge_brain(template_surface_types = c("pial", 'white'))
        local_env$brain <- local_env$brain$template_object
        return(local_env$brain$localize(
          side_display = FALSE,
          control_presets = "localization",
          show_modal = FALSE
        ))
      }
      # get ct_path
      ct_paths <- c(
        file.path(subject$freesurfer_path, "RAVE", "ct_in_t1.nii"),
        file.path(subject$freesurfer_path, "RAVE", "ct_in_t1.nii.gz"),
        file.path(subject$freesurfer_path, "ct_in_t1.nii"),
        file.path(subject$freesurfer_path, "ct_in_t1.nii.gz")
      )
      ct_paths <- ct_paths[file.exists(ct_paths)]
      if(!length(ct_paths)){
        return(local_env$brain$localize(
          side_display = FALSE,
          control_presets = "localization",
          show_modal = FALSE
        ))
      }
      ct_path <- normalizePath(ct_paths[[1]], mustWork = TRUE)
      ns <- asNamespace('threeBrain')
      ct <- ns$read_nii2(ct_path)
      cube <- ns$reorient_volume(ct$get_data(), local_env$brain$Torig)
      threeBrain::add_voxel_cube(local_env$brain, "CT", cube)
      
      key <- seq(0, max(cube))
      cmap <-
        threeBrain::create_colormap(
          gtype = "volume",
          dtype = "continuous",
          key = key,
          value = key,
          color = c("white",
                    "green", "darkgreen")
        )
      controllers <- list()
      controllers[["Left Opacity"]] <- 0.4
      controllers[["Right Opacity"]] <- 0.4
      controllers[["Voxel Type"]] <- "CT"
      controllers[["Voxel Min"]] <- 3000
      controllers[["Edit Mode"]] <- "CT/volume"
      
      local_env$brain$plot(
        control_presets = "localization",
        voxel_colormap = cmap,
        controllers = controllers,
        side_display = FALSE,
        custom_javascript = "canvas.controls.noPan=true;",
        show_modal = FALSE
      )
    }
    
    output$viewer <- threeBrain::renderBrain({
      shiny::validate(
        shiny::need(isTRUE(local_reactive$subject_id != ""),
                    message = "Please check 'Overview' module to load a subject first.")
      )
      shiny::showNotification(
        shiny::p("Loading... Please wait"),
        type = "message",
        closeButton = FALSE,
        duration = NULL,
        id = "notif"
      )
      on.exit({
        shiny::removeNotification("notif")
      })
      localize()
    })
    
    
    # Step 1: init, whenever subject has changed
    observe({
      user_data$reset
      # utils$load_subject(subject_code = 'YAB', project_name = 'demo')
      list2env(list(utils = utils), envir=globalenv())
      subject_id <- utils$get_subject_id()
      
      if(length(subject_id) && subject_id != "" && nchar(subject_id) >= 3){
        local_reactive$subject_id <- subject_id
      } else {
        local_reactive$subject_id <- ''
      }
      
    })
    
    shiny::observeEvent(proxy_brain$localization_table, {
      tbl <- proxy_brain$localization_table
      tbl$Electrode[duplicated(tbl$Electrode)] <- ""
      local_reactive$table <- tbl
    })
    
    shiny::observeEvent(input$load, {
      infile <- input$load
      tryCatch({
        dat <- utils::read.csv(infile$datapath)
        for (i in seq_len(nrow(dat))) {
          proxy_brain$add_localization_electrode(dat[i,], i == nrow(dat))
        }
      }, error = function(e) {
        shiny::showNotification(shiny::p(
          "Error while trying to load from ",
          infile$name,
          ": ",
          e$message
        ),
        type = "error")
      })
    })
    
    shiny::observeEvent(input$refresh, {
      proxy_brain$set_localization_electrode(-1, list(),
                                             update_shiny = TRUE)
    })
    
    shiny::observeEvent(input$fslut, {
      nm <- input$fslut
      if (length(nm) == 1) {
        cid <- cmap$ColorID[cmap$Label == nm]
        if (length(cid) == 1) {
          shiny::updateTextInput(session = session,
                                 inputId = "fslutid",
                                 value = cid)
        }
      }
    })
    
    
    shiny::observeEvent(input$table_cell_edit, {
      edit <- input$table_cell_edit
      tbl <- local_reactive$table
      nms <- names(tbl)
      mode <- sapply(nms, function(nm) {
        storage.mode(tbl[[nm]])
      })
      params <- structure(lapply(seq_along(nms), function(ii) {
        re <- edit$value[edit$col == ii]
        storage.mode(re) <- mode[[ii]]
        re
      }), names = nms)
      proxy_brain$set_localization_electrode(params$LocalizationOrder,
                                             params, update_shiny = FALSE)
    })
    shiny::observeEvent(input$clear, {
      proxy_brain$clear_localization()
    })
    output$table <- DT::renderDT({
      tbl <- local_reactive$table
      shiny::validate(
        shiny::need(is.data.frame(tbl) &&
                      length(tbl) &&
                      nrow(tbl), message = "Please click on the CT to localize electrodes first.")
      )
      rownames(tbl) <- tbl$LocalizationOrder
      DT::datatable(
        tbl,
        class = "compact cell-border stripe",
        editable = list(target = "row", disable = list(columns = c(
          1,
          3:5, 9, 10:15
        ))),
        selection = "single",
        rownames = TRUE,
        caption = "Please add the electrode/channel number before saving to RAVE",
        filter = "none",
        options = list(
          dom = "rtip",
          scrollX = FALSE,
          scrollY = TRUE,
          pageLength = nrow(tbl),
          columnDefs = list(list(
            visible = FALSE,
            targets = c(1,
                        3:5, 7, 10:15, 19, 20)
          ))
        )
      )
    })
    output$save <-
      shiny::downloadHandler(
        filename = "electrode.csv",
        content = function(conn) {
          shiny::showNotification(
            shiny::p("Generating electrode.csv ..."),
            type = "message",
            duration = NULL,
            id = "save_notif",
            closeButton = FALSE
          )
          tbl <-
            local_reactive$table
          on.exit({
            local_env$brain$electrodes$raw_table <- NULL
            local_env$brain$electrodes$objects <- NULL
            shiny::removeNotification("save_notif", session = session)
          })
          tbl$Hemisphere <- ""
          tbl$Subject <- local_env$brain$subject_code
          tbl$Electrode <- as.integer(tbl$Electrode)
          sel <- is.na(tbl$Electrode)
          if (length(tbl$Electrode) &&
              any(sel)) {
            if (all(sel)) {
              tbl$Electrode <- seq_along(tbl$Electrode)
            }
            else {
              start <- max(tbl$Electrode, na.rm = TRUE)
              tbl$Electrode[sel] <-
                seq_len(sum(sel)) +
                start
            }
          }
          tbl$Hemisphere[grepl("(lh)|(Left)", tbl$FSLabel)] <- "left"
          tbl$Hemisphere[grepl("(rh)|(Right)", tbl$FSLabel)] <- "right"
          tbl$VertexNumber <- -1
          local_env$brain$set_electrodes(tbl)
          try({
            tbl <- local_env$brain$calculate_template_coordinates(hemisphere = FALSE)
          })
          
          utils::write.csv(tbl, conn, row.names = FALSE)
        }
      )
    
  }
  
  return(list(
    body = body,
    server = server
  ))
}
