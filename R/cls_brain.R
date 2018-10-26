#' Class for Brain Mesh/Electrodes
#' @export
RaveBrain <- R6::R6Class(
  classname = 'Brain',
  active = list(
    mesh_count = function(){
      length(private$three_pial)
    }
  ),
  private = list(

    data = list(
      # Electrode   Coord_x    Coord_y    Coord_z      Label
      electrode_table = list()
    ),

    # Subject object
    subject = NULL,
    electrodes = list(),  # # position label

    # ThreeJSR objects
    three_pial = list(),  # 1 for left, 2 for right
    three_electrodes = list(),

    # Animation/color objects
    ani_pial = list(),
    ani_electrodes = list(),

    # Sapces
    # 1: MRI space - Experiment space
    # 2: FreeSurfer space
    sp_pial = 2,
    sp_electrodes = 1,

    # Transforms: All mat are: from space n to MRI space
    tf_2 = diag(c(-1,-1,1,1))
  ),
  public = list(
    initialize = function(subject){
      if(!missing(subject)){
        self$load_subject(subject)
      }
    },
    `_set_threejs` = function(key, val){
      private[[key]] = val
    },
    copy = function(){
      re = self$clone(deep = TRUE)
      re[['_set_threejs']]('three_electrodes', lapply(private$three_electrodes, function(x){
        if(is.null(x)){
          return(NULL)
        }
        x$clone()
      }))
      re[['_set_threejs']]('three_pial', lapply(private$three_pial, function(x){
        if(is.null(x)){
          return(NULL)
        }
        x$clone()
      }))
      re
    },
    load_electrodes = function(tbl){
      lapply(seq_len(nrow(tbl)), function(ii){
        row = tbl[ii,]
        if(is_invalid(row$Label, .invalids = c('null','na','blank'))){
          row$Label = sprintf('Electrode %d', row$Electrode)
        }else{
          row$Label = sprintf('Electrode %d - %s', row$Electrode, row$Label)
        }
        self$set_electrode(which = row$Electrode, position = c(row$Coord_x, row$Coord_y, row$Coord_z), label = row$Label, show_warning = FALSE)
      })
      invisible()
    },
    load_subject = function(subject){
      if(!is(subject, 'Subject')){
        subject = stringr::str_split_fixed(subject, '/', 2)
        subject = Subject$new(project_name = subject[1], subject_code = subject[2])
      }
      # try to import electrodes from subject
      private$subject = subject

      tbl = subject$electrodes
      self$load_electrodes(tbl)
    },
    set_electrode = function(which, position, label = sprintf('Electrode %d', which), show_warning = T){
      if(show_warning && (length(label) != 1 || is_invalid(label, .invalids = c('null', 'na')))){
        logger('label is invalid', level = 'WARNING')
        label = ''
      }
      assertthat::assert_that(length(position) == 3 && is.numeric(position), msg = 'invalid position')
      assertthat::assert_that(length(which) == 1 && is.integer(which) && which >=1, msg = 'invalid which')
      private$electrodes[[which]] = list(
        idx = which,
        position = position,
        label = label
      )

      private$three_electrodes[[which]] = threejsr::GeomSphere$new(
        position = position,
        mesh_name = label,
        radius = 2,
        layer = 2, # Layer 2 is for electrodes
        hover_enabled = T,
        is_clipper = FALSE,
        clippers = NULL
      )

      # Add click callback
      private$three_electrodes[[which]]$add_custom_control(
        type = 'hidden',
        label = 'hidden',
        index = 1,
        l = list(hidden = TRUE, name = 'mouse_callback', mouse_event = TRUE, callback = "function(value, mesh){var pos = mesh.position; var plane_x = canvas.scene.getObjectByName('plane_x'), plane_y = canvas.scene.getObjectByName('plane_y'), plane_z = canvas.scene.getObjectByName('plane_z'), label, any_update = false;if(plane_x.isMesh || false){label = plane_x.userData.__params.position_x.label;plane_x.userData.__params.position_x.__values[label] = pos.x;plane_x.userData.__funs['position_x'](pos.x); any_update = true;}if(plane_y.isMesh || false){label = plane_y.userData.__params.position_y.label;plane_y.userData.__params.position_y.__values[label] = pos.y;plane_y.userData.__funs['position_y'](pos.y);any_update = true;}if(plane_z.isMesh || false){label = plane_z.userData.__params.position_z.label;plane_z.userData.__params.position_z.__values[label] = pos.z;plane_z.userData.__funs['position_z'](pos.z);any_update = true;}  if(any_update){(gui.__folders['Position'].__controllers.map(k => k.updateDisplay()));}}")
      )
      invisible()
    },

    set_pial = function(which, vertices, faces, position = c(0,0,0)){
      if(is.character(which)){
        switch (which,
                'left' = { which = 1 },
                'right' = { which = 2 },
                {
                  stop('Unsupported which argument, we only support "left"/1, or "right"/2')
                }
        )
      }else if(!which %in% c(1,2)){
        stop('Unsupported which argument, we only support "left"/1, or "right"/2')
      }
      private$three_pial[[which]] = threejsr::GeomFreeMesh$new(
        position = position,
        mesh_name = c('Left Pial', 'Right Pial')[which],
        vertices = vertices,
        faces = faces,
        hover_enabled = F,
        is_clipper = F
      )
    },
    reset_transform = function(which){
      if(!which %in% c(1, 2)){
        stop('which must be:\n\t1: Ignored\n\t2: Freesurfer to MRI Space')
      }
      if(which == 2){
        private$tf_2 = daig(c(-1,-1,1,1))
      }
    },
    set_transform = function(which, mat){
      if(!which %in% c(1, 2)){
        stop('which must be:\n\t1: Ignored\n\t2: Freesurfer to MRI Space')
      }
      assertthat::assert_that(nrow(mat) == 4 && ncol(mat) == 4, msg = '4x4 matrix is needed')
      if(which == 2){
        private$tf_2 = mat
      }
    },
    find_file = function(path, default = NULL, alt_dir = '.'){
      tmp = path
      if(!file.exists(path)){
        file_name = tail(unlist(str_split(path, '/|\\\\')), 1)

        tmp = file.path(alt_dir, file_name)

        if(!file.exists(tmp)){
          tmp = file.path(alt_dir, path)

          if(!file.exists(tmp)){
            logger('Cannot find file', level = 'WARNING')
            tmp = default
          }
        }


      }
      tmp
    },
    import_spec = function(spec_file, include_electrodes = FALSE, nearest_face = TRUE, ...){
      # shiny_mode
      if(is.null(getDefaultReactiveDomain())){
        shiny_mode = F
      }else{
        shiny_mode = T
      }

      if(missing(spec_file)){
        if(is.null(private$subject)){
          s = 'Please provide spec_file'
          if(shiny_mode){ return(s) }else{ stop(s) }
        }
        # try to load from private subject
        suma_dir = private$subject$dirs$suma_dir

        # get spec file
        spec_file = file.path(suma_dir, rave_options('suma_spec_file'))
      }
      if(!file.exists(spec_file)){
        s = 'spec_file not exists'
        if(shiny_mode){ return(s) }else{ stop(s) }
      }

      # This will be a long process (maybe?)
      progress = rave::progress('Loading from suma spec file...', max = 5 + include_electrodes + nearest_face)
      on.exit({progress$close()})

      progress$inc('Parsing spec file')
      spec_info = rave::suma_spec_parse(spec_file = spec_file)

      # Load volume
      progress$inc('Loading surface volume')
      volume_file = unique(unlist(
        lapply(spec_info, '[[', 'SurfaceVolume')
      ))

      #
      if(length(volume_file)){
        # right now we only support one volume file
        volume_file = volume_file[1]
        tmp = sprintf('%s.head', volume_file)

        if(!file.exists(tmp)){
          # path might be relative, hence switch to subject sumar dir, which is parent dir of spec_file
          # get file name only
          file_name = tail(unlist(str_split(volume_file, '/|\\\\')), 1)

          tmp = file.path(dirname(spec_file), sprintf('%s.head', file_name))

          if(!file.exists(tmp)){
            tmp = file.path(dirname(spec_file), sprintf('%s.head', volume_file))

            if(!file.exists(tmp)){
              logger('Cannot find surface volume file', level = 'WARNING')
              tmp = NULL
            }
          }


        }

        if(length(tmp)){
          # load surface volume
          volume_info = rave:::suma_surface_volume_parse(tmp)

          # save mat volume_info[['VOLREG_MATVEC_000000']]
          mat = volume_info[['VOLREG_MATVEC_000000']]
          if(length(mat) && length(mat$value) >= 12){
            mat = c(mat$value[seq_len(12)], 0,0,0,1)
            mat = matrix(mat, nrow = 4, ncol = 4, byrow = T)

            # This is transformation of fs space to exp space
            self$set_transform(2, mat)
          }
        }

      }

      progress$inc('Loading pials')

      # load mesh
      lapply(spec_info, function(info){
        surf_name = info[['FreeSurferSurface']]
        surf_name %?<-% info[['SurfaceName']]
        surf_state = info[['SurfaceState']]
        f = self$find_file(surf_name, default = NULL, alt_dir = dirname(spec_file))

        if(length(f) && file.exists(f)){
          if(stringr::str_detect(surf_name, '^lh')){
            which = 1
            progress$inc(message = fprintf('Loading left pial'))
          }else if(stringr::str_detect(surf_name, '^rh')){
            which = 2
            progress$inc(message = fprintf('Loading right pial'))
          }else if(include_electrodes){
            # Load electrodes from electrodes.asc
            which = 3
            progress$inc(message = fprintf('Try to locate electrodes'))

          }else{
            return(NULL)
          }

          # check type
          if(stringr::str_detect(f, '[gG][iI][iI]$')){
            # this is gifti file
            if('gif')
            tryCatch({
              mesh_data = threejsr::read.freesurf.gii(f)
            }, error = function(e){
              # gifti package is not loaded
              logger('Please run `install.packages("gifti")` to enable reading from gii files.', level = 'WARNING')
              return(NULL)
            }) ->
              mesh_data
            if(is.null(mesh_data)){
              return()
            }
          }else if(stringr::str_detect(f, '[aA][sS][cC]$')){
            mesh_data = threejsr::read.freesurf.asc(f)
          }else{
            require()
          }

          if(which %in% c(1,2)){
            # generate threejsr GEOM object
            self$set_pial(which, vertices = mesh_data$vertices, faces = mesh_data$faces, position = c(0,0,0))
          }else{
            # electrodes
            nn = rave_options('suma_nodes_per_electrodes')
            n_electrodes = mesh_data$header[1] / nn
            # Need to clear electrode lists
            private$electrodes = list()
            private$three_electrodes = list()
            lapply(seq_len(n_electrodes), function(ii){
              position = colMeans(mesh_data$vertices[ii*nn - seq_len(nn) + 1, ])[1:3]
              self$set_electrode(which = ii, position = position, label = sprintf('Electrode %d', ii))
            })
          }

          return(list(
            surf_name = surf_name,
            surf_state = surf_state,
            meta_info = info,
            which = which
          ))
        }
        return(NULL)
      }) ->
        surf_info

      if(nearest_face){
        progress$inc('Compute nearest vertices')
        lapply(seq_along(private$three_electrodes), function(ii){
          self$compute_nearest_face(ii, ...)
        })
      }

      invisible(dropNulls(surf_info))
    },
    to_freesurf_space = function(pos){

      if(!is.matrix(pos)){
        pos = matrix(pos, ncol = 3, byrow = T)
      }

      if(length(private$tf_2)){
        pos = ((private$tf_2) %*% t(cbind(pos, -1))) * c(-1,-1,1, 0)
        pos = t(pos)[,1:3]
      }

      pos
    },
    get_electrode_position = function(freesurf_space = T){
      t(
        sapply(private$electrodes, function(e){
          e$position
        })
      ) ->
        re
      if(freesurf_space){
        re = self$to_freesurf_space(re)
      }
      re
    },
    get_face_position = function(which_pial, face_id){
      # Get pial

      vertex_idx = private$three_pial[[which_pial]]$get_face(face_id)
      pos = rowMeans(sapply(vertex_idx, function(i){
        private$three_pial[[which_pial]]$get_vertex_position(i, start_from = 0)
      }))
      pos
    },
    get_nearest_face_id = function(compute = T){
      if(compute){
        lapply(seq_along(private$electrodes), function(e){
          self$compute_nearest_face(e, max_dist = Inf)
        })
      }
      t(
        sapply(private$electrodes, function(e){
          fidx = e$nearest_face$which_faces

          if(length(fidx) > 1){

            pial = private$three_pial[[e$nearest_face$which_pial]]

            e_pos = self$to_freesurf_space(e$position)

            sapply(fidx, function(f){
              vid = pial$get_face(f)
              mean(sapply(vid, function(v){
                pos = pial$get_vertex_position(v, start_from = 0)
                sum((e_pos - pos)^2)
              }))
            }) ->
              d
            fidx = fidx[which.min(d)]
          }else if(length(fidx) == 0){
            fidx = NA
          }
          c(
            e$nearest_face$which_pial,
            fidx
          )
        })
      )
    },
    compute_nearest_face = function(electrode, which_pials = c(1,2), max_dist = 30){
      # get electrode info
      e = private$electrodes[[electrode]]

      nearest_face = NULL

      # apply inverse transformation:
      if(length(private$tf_2)){
        sp2_position = (private$tf_2[1:3, ] %*% c(e$position, -1)) * c(-1,-1,1)
      }else{
        sp2_position = e$position
      }

      for(ii in which_pials){
        pial = private$three_pial[[ii]]
        v = pial$get_data('vertices', reshape = F)
        f = pial$get_data('faces', reshape = F)
        dim(v) = c(3, length(v) / 3)

        v = v - as.vector(sp2_position)
        # d = apply(v, 2, function(x){sum(x^2)})
        d = colSums(v^2)

        wm = which.min(d)
        md = sqrt(d[[wm]])
        if(md <= max_dist){
          d = f == wm - 1 # should be sum = 6
          dim(d) = c(3, length(d) / 3)
          face_ind = which(rutabaga::collapse(d, 2) > 0)
          # register
          if(length(nearest_face)){
            if(nearest_face$min_dist > md){
              nearest_face = NULL
            }
          }

          if(!length(nearest_face)){
            nearest_face = list(
              which_pial = ii,
              min_dist = md,
              which_vertex = wm,
              which_faces = face_ind
            )
            private$electrodes[[electrode]]$nearest_face = nearest_face
          }
        }
      }
    },
    hook_electrodes = function(which){
      if(missing(which)){
        which = seq_along(private$three_electrodes)
      }
      for(ii in which){
        try({
          # Set electrode hooks
          if(length(private$electrodes))
            info = private$electrodes[[ii]]
          nearest_face = get_val(info, 'nearest_face', NULL)
          if(length(nearest_face)){
            # Find hooker
            nearest_face$target_name = c("Left Pial", "Right Pial")[nearest_face$which_pial]
            private$three_electrodes[[ii]]$set_hook(nearest_face)
          }else{
            # Remove hooks
            private$three_electrodes[[ii]]$set_hook()
          }
        }, silent = T)
      }
    },

    render_electrodes = function(which, pal = colorRampPalette(c('navy', 'white', 'red'))(101), center = 0, name = 'Animation'){
      if(missing(which)){
        which = seq_along(private$three_electrodes)
      }
      # Get all the values for electrodes
      vals = unlist(lapply(private$ani_electrodes, '[', 'value'))
      if(!length(vals)){
        # No value assigned, stop
        return()
      }
      val_range = range(vals)
      key_frames = unlist(lapply(private$ani_electrodes, '[', 'keyframe'))
      key_frames = sort(unique(key_frames))

      # center the value
      n_pal = length(pal)
      scale = floor(n_pal / 2) / max(abs(val_range - center))

      f = function(x){
        x = round((x - center) * scale + floor(n_pal / 2)+1)
        x[x<1] = 1; x[x>n_pal] = n_pal
        col2rgb(pal[x]) / 255
      }


      for(ii in which){
        el = private$three_electrodes[[ii]]
        if(!is(el, 'TGeom')){
          next
        }
        tryCatch({
          # Set electrode hooks
          info = private$ani_electrodes[[ii]] # keyframe and value
          if(!length(info)){
            stop('')
          }
          el$animation_event(
            name = name,
            event_data = t(f(info$value)),
            key_frames = info$keyframe,
            loop = F
          )
        }, error = function(err){
          el$remove_event(event_type = 'animation', name)
        })
      }
    },
    view = function(show_mesh = T, pal = colorRampPalette(c('navy', 'white', 'red'))(101), center = 0,
                    width = '100vw', height = '100vh', control_gui = F, ...){


      # Add gui controls to pials
      if(show_mesh && length(private$three_pial) == 2){
        for(ii in 1:2){
          pial = private$three_pial[[ii]]
          if(length(private$tf_2)){
            pial$set_transform(mat = diag(c(-1,-1,1,1)), append = F)
            pial$set_transform(mat = private$tf_2, append = T)
          }
          pial$add_visibility_control(
            type = pial$name,
            name = 'visibility',
            label = 'Show/Hide',
            initial = TRUE,
            index = 1
          )
          pial$add_custom_control(
            type = pial$name,
            l = list(
              name = 'wireframe',
              label = 'Wireframe',
              initial = FALSE,
              callback = 'function(value, mesh){mesh.material.wireframe=value;}'
            ),
            index = 2
          )
        }
        pials = private$three_pial
        # Link electrodes to closest pial vertex
        self$hook_electrodes()
      }else{
        pials = NULL
      }

      # set color/animations to electrodes
      self$render_electrodes(pal = pal, center = center)

      # Render via threejsr
      elements = c(pials, private$three_electrodes)
      elements = dropNulls(elements)
      if(!length(elements)){
        return('No elements detected!')
      }
      threejsr:::threejs_scene.default(
        elements = elements,
        width = width,
        height = height,
        control_gui = control_gui,
        mouse_control_target = c(0,0,30),
        ...
      )
    },
    set_electrode_value = function(which, value, keyframe){
      # keyframe always starts from 0, hence if length of value is 1, keyframe will be set to 0
      if(length(value) == 1){
        keyframe = c(0, 100)
        value = rep(value, 2)
      }
      private$ani_electrodes[[which]] = list(
        keyframe = keyframe,
        value = value
      )
    },
    set_electrode_label = function(which, label, name){
      for(ii in which){
        el = private$three_electrodes[[ii]]
        if(!is(el, 'TGeom')){
          next
        }
        el$mesh_info = label
        if(is.na(el$name)){
          el$set_name(label)
        }
      }
    },
    set_electrode_size = function(which, radius = 2){
      assertthat::assert_that(is.numeric(radius) && length(radius) == 1, msg = "invalid radius")
      for(ii in which){
        el = private$three_electrodes[[ii]]
        if(!is(el, 'TGeom')){
          next
        }
        el$set_radius(radius)
      }
    }
  )
)



# require(rave)
# require(stringr)
# ii=1
# self = RaveBrain$new('congruency/YAB'); private = self$.__enclos_env__$private
# self$import_spec( include_electrodes = F )
# a = lapply(1:84, function(i) { self$set_electrode_value(i, seq_len(i), seq_len(i)) })
# self$view(control_gui = T, center = 40)

# pial = private$three_pial[[1]]
