#' Tools to load and view brain in 3D viewer
#' @param surfaces one or more from `pial`, `white`, `smoothwm`, brain surface types
#' @param multiple_subject is this a template brain?
#' @param prefix internally used, prefix to `FreeSurfer` asc files
#' @export
rave_brain2 <- function(surfaces = 'pial', multiple_subject = FALSE, prefix = 'std.141.'){
  env = environment()
  Brain = get_from_package(f = 'Brain', pkg = 'threeBrain', check = F)
  brain = Brain$new(multiple_subject = multiple_subject, default_surfaces = surfaces)
  current_subject_id = ''
  default_surfaces = surfaces

  afni_tool = afni_tools()
  logger = function(..., sep = ''){
    cat2('[', current_subject_id, '] ', ..., sep = sep)
  }

  # Check if subject exists
  has_subject = function(subject){
    if(!is.character(subject)){
      subject = subject$id
    }
    exists(subject, envir = brain$subjects, inherits = FALSE)
  }

  # Function to load subject, no data is loaded!
  add_subject = function(subject){
    subject = as_subject(subject)
    env$current_subject_id = subject$id
    pretty_id = stringr::str_remove_all(subject$id, '[^a-zA-Z0-9]')
    if(!has_subject(subject$id)){
      brain$add_subject(subject_name = subject$id)
    }
    viewer_dir = file.path(subject$dirs$rave_dir, 'viewer')

    return(list(
      subject = subject,
      pretty_id = pretty_id,
      viewer_dir = viewer_dir
    ))
  }

  # load electrodes
  # We only assume electrodes.csv is provided
  load_electrodes = function(subject){
    re = add_subject(subject)
    subject = re$subject
    pretty_id = re$pretty_id
    viewer_dir = re$viewer_dir

    # --------------------- Import Electrodes ---------------------
    electrode_table = load_meta('electrodes', subject_id = subject$id)
    assert_that(is.data.frame(electrode_table), msg = 'Cannot read meta/electrodes.csv. Make sure it exists and not broken!')

    # get x, y, x, subcortical, which surface attached, vertex node
    # Optional columns:
    #  Subcortical, Hemisphere, SurfaceType, VertexNumber, IsMini

    for(ii in seq_len(nrow(electrode_table))){
      row = electrode_table[ii, ]

      if(all(c(row$Coord_x, row$Coord_y, row$Coord_z) == 0)){
        next();
      }

      row$Subcortical %?<-% FALSE
      row$SurfaceType %?<-% 'pial'
      row$VertexNumber %?<-% -1
      row$IsMini %?<-% FALSE

      brain$add_electrode(
        subject_name = subject$id,
        name = sprintf('Electrode %s (%s, %s)', row$Electrode, row$Label, subject$id),
        x = row$Coord_x,
        y = row$Coord_y,
        z = row$Coord_z,
        sub_cortical = row$Subcortical,
        surface_type = row$SurfaceType,
        hemisphere = row$Hemisphere,
        vertex_number = row$VertexNumber,
        radius = 2 - row$IsMini
      )
    }
  }

  align_electrodes = function(subject){
    re = add_subject(subject)
    subject = re$subject
    pretty_id = re$pretty_id
    viewer_dir = re$viewer_dir
    suma_dir = subject$dirs$suma_dir
    # --------------------- Transform electrodes ---------------------
    # Need to load transformation for T1 to freesurfer
    trans_mat_file = file.path(suma_dir, 'T1_to_freesurf.txt')
    afni_file = file.path(suma_dir, 'fs_SurfVol_Alnd_Exp+orig.HEAD')
    trans_mat = NULL

    if(file.exists(trans_mat_file)){
      try({
        mm = as.matrix(read.table(trans_mat_file))
        if(is.numeric(mm) && length(mm) == 16 && nrow(mm) == 4){
          trans_mat = mm
          colnames(trans_mat) = rownames(trans_mat) = NULL
        }
      })
    }

    if(is.null(trans_mat)){
      if(file.exists(afni_file)){

        logger('suma/T1_to_freesurf.txt is missing, try to load from AFNI file')
        dat = afni_tool$read.AFNI(afni_file)

        if('VOLREG_MATVEC_000000' %in% names(dat$header)){

          mat = matrix(c(dat$header$VOLREG_MATVEC_000000, 0,0,0,1), nrow = 4, ncol = 4, byrow = T)
          mm = diag(c(-1,-1,1, 0)) %*% mat; mm[,4] = -mm[,4]; mm[4,4] = 1; mm = solve(mm)
          trans_mat = mm
          write.table(mm, file = trans_mat_file, sep = '\t', col.names = FALSE, row.names = FALSE)

        }else{

          logger('Cannot find VOLREG_MATVEC_000000 in afni file. ',
                 'Please make sure it contains transform matrix from T1 to FreeSurfer orientation. ',
                 'No transform will be made', level = 'WARNING')

        }
      }else{
        trans_mat = diag(1, 4)
        logger('No transform matrix found, I assume that these electrodes have already been aligned to FreeSurfer orientation.', level = 'INFO')
      }
    }

    brain$set_tranform(subject_name = subject$id, mat = trans_mat)
  }

  load_surfaces = function(subject, surfaces, quiet = FALSE){
    re = add_subject(subject)
    subject = re$subject
    pretty_id = re$pretty_id
    viewer_dir = re$viewer_dir

    if(missing(surfaces)){
      surfaces = default_surfaces
    }

    progress = progress('Loading Surface', max = length(surfaces) * 2 + 1, quiet = quiet)
    on.exit({progress$close()})

    # --------------------- Import Surface ---------------------
    suma_dir = subject$dirs$suma_dir
    fs = list.files(suma_dir)
    pattern = sprintf('^%s[rl]h\\.([\\w]+)\\.asc$', prefix)
    fs = fs[stringr::str_detect(fs, pattern)]
    fs = stringr::str_match(fs, pattern)

    if(!isTRUE(surfaces)){
      fs = fs[fs[,2] %in% surfaces, , drop = FALSE]
    }

    if(nrow(fs)){
      v = sapply(fs[, 2], function(nm){
        lh = paste0(prefix, 'lh.', nm, '.asc')
        rh = paste0(prefix, 'rh.', nm, '.asc')
        sum(lh %in% fs[,1], rh %in% fs[,1])
      })
      fs = fs[v >= 2, , drop = FALSE]


    }

    if(nrow(fs)){
      # Add surfaces
      lapply(unique(fs[,2]), function(nm){
        lh = paste0(prefix, 'lh.', nm, '.asc')
        lh = file.path(suma_dir, lh)

        rh = paste0(prefix, 'rh.', nm, '.asc')
        rh = file.path(suma_dir, rh)

        lh_cache = paste0(pretty_id, '_lh_', nm, '.json')
        lh_cache = file.path(viewer_dir, lh_cache)

        rh_cache = paste0(pretty_id, '_rh_', nm, '.json')
        rh_cache = file.path(viewer_dir, rh_cache)

        if(!file.exists(rh_cache) && file.exists(rh)){
          progress$inc(paste('Creating cache for', nm, '(This may take a while)'))
        }else if(file.exists(rh_cache)){
          progress$inc(paste('Loading ', nm))
        }

        brain$add_surface(
          subject_name = subject$id, surface_name = nm,
          lh_surface = lh,
          rh_surface = rh,
          lh_surface_cache = lh_cache,
          rh_surface_cache = rh_cache
        )
      })
    }

    progress$inc('Transform electrodes from T1 to FreeSurfer...')
    align_electrodes(subject)

  }


  cache = function(subject){
    subject = as_subject(subject)

    electrode_table = load_meta('electrodes', subject_id = subject$id)
    assert_that(is.data.frame(electrode_table), msg = 'Cannot read meta/electrodes.csv. Make sure it exists and not broken!')

    # Make sure all surfaces are loaded and all electrodes are loaded
    load_surfaces(subject, surfaces = TRUE)
    # load all the electrodes
    load_electrodes(subject)

    electrodes = brain$subjects[[subject$id]]$electrodes
    tran_mat = electrodes[[1]]$group$trans_mat
    surfaces = brain$subjects[[subject$id]]$surface

    # --------------------- Mapping ------------------------
    tbl_names = names(electrode_table)
    new_cols = c("Subcortical", "SurfaceType", "Hemisphere", "VertexNumber")
    tbl_names = c(tbl_names[!tbl_names %in% c(new_cols)], new_cols)
    idx = which(tbl_names == 'Electrode')
    tbl_names = tbl_names[idx:length(tbl_names)]

    new_table = lapply(seq_len(nrow(electrode_table)), function(ii){
      row = electrode_table[ii, ]
      name = sprintf('Electrode %s (%s, %s)', row$Electrode, row$Label, subject$id)

      pos = tran_mat %*% c(row$Coord_x, row$Coord_y, row$Coord_z, 1)
      pos = pos[1:3]

      row$Subcortical %?<-% FALSE

      if(!row$Subcortical){

        row$SurfaceType %?<-% 'pial'

        # Match to SurfaceType
        sf = surfaces[[row$SurfaceType]]

        vert_left = sf$left$get_data(sprintf('free_vertices_lh - %s (%s)', row$SurfaceType, subject$id))
        vert_right = sf$right$get_data(sprintf('free_vertices_rh - %s (%s)', row$SurfaceType, subject$id))

        dist_left = colSums((t(vert_left) - pos)^2)
        dist_right = colSums((t(vert_right) - pos)^2)

        idx_left = which.min(dist_left)
        idx_right = which.min(dist_right)


        if(dist_left[idx_left] < dist_right[idx_right]){
          # use lh
          row$VertexNumber = idx_left
          row$Hemisphere = 'left'
        }else{
          row$VertexNumber = idx_right
          row$Hemisphere = 'right'
        }

      }else{
        row$SurfaceType %?<-% 'pial'
        row$VertexNumber = -1
        row$Hemisphere = ''
      }

      row[, tbl_names]

    })

    new_table = do.call(rbind, new_table)

    save_meta(new_table, 'electrodes', project_name = subject$project_name, subject_code = subject$subject_code)

    #  Subcortical, Hemisphere, SurfaceType, VertexNumber, IsMini
  }


  set_electrode_value = function(subject, electrode, value, time, message = ''){
    time %?<-% 0
    assert_that(is.numeric(value), msg = 'value must be numeric')
    assert_that(length(time) == length(value), msg = 'value must be a single value, otherwise time value must have the same length.')

    subject = as_subject(subject)

    l = brain$subjects[[subject$id]]
    if(is.list(l)){
      if(length(l$electrodes) == 0){
        load_electrodes(subject = subject$id)
      }
      tbl = subject$electrodes
      sel = tbl$Electrode == electrode
      if(any(sel)){
        row = tbl[sel, ]
        name = sprintf('Electrode %s (%s, %s)', row$Electrode, row$Label, subject$id)
        if(name %in% names(l$electrodes)){
          l$electrodes[[name]]$set_value(value = value, time_stamp = time)
          l$electrodes[[name]]$custom_info = message
        }

      }

    }
  }



  set_multiple_subject = function(is_mult){
    is_mult = isTRUE(is_mult)
    # if is_mult

    if(is_mult){
      ## Remove gui elements
      # brain$groups[["Left Hemisphere"]]$group_data$.gui_params = brain$groups[["Left Hemisphere"]]$group_data$.gui_params['N27']
      # brain$groups[["Right Hemisphere"]]$group_data$.gui_params = brain$groups[["Right Hemisphere"]]$group_data$.gui_params['N27']

      ## remove surfaces
      sf = names(brain$groups[["Left Hemisphere"]]$group_data)
      nm = sf[!stringr::str_detect(sf, '^free_(vertices|faces)_[lr]h - .+ \\((?!Template N27).*\\)$')]
      brain$groups[["Left Hemisphere"]]$group_data = brain$groups[["Left Hemisphere"]]$group_data[nm]

      sf = names(brain$groups[["Right Hemisphere"]]$group_data)
      nm = sf[!stringr::str_detect(sf, '^free_(vertices|faces)_[lr]h - .+ \\((?!Template N27).*\\)$')]
      brain$groups[["Right Hemisphere"]]$group_data = brain$groups[["Right Hemisphere"]]$group_data[nm]
    }

    brain$set_multiple_subject(is_mult)

  }



  re = list(
    has_subject = has_subject,
    add_subject = add_subject,
    load_electrodes = load_electrodes,
    align_electrodes = align_electrodes,
    set_electrode_value = set_electrode_value,
    load_surfaces = load_surfaces,
    calculate_template_brain_location = cache,

    set_multiple_subject = set_multiple_subject,
    view = function(...){
      if(brain$multiple_subject){
        set_multiple_subject(brain$multiple_subject)
      }
      env$last_viewer_args = list(...)
      brain$view(...)
    },

    save_brain = function(directory, filename = 'index.html', title = '3D Viewer', as_zip = FALSE, ...){
      args = list(...)
      if(!length(args)){
        args = as.list(env$last_viewer_args)
      }
      wd = do.call(brain$view, args)
      re = save_threebrain(widget = wd, directory = directory,
                             filename = filename, title = title, as_zip = as_zip)
      invisible(re)
    },

    get_object = function(){
      brain
    }
  )

  .env = list2env(re, parent = baseenv())

  class(.env) = c('rave_three_brain', 'environment')

  .env
}


#' Show RAVE brain via package `threeBrain`
#' @param x generated from rave_brain2
#' @param ... passed to \code{threeBrain::threejs_brain}
#' @export
print.rave_three_brain <- function(x, ...){
  print(x$view(...))
}
