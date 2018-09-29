#' Class for Brain Mesh/Electrodes
#' @export
RaveBrain <- R6::R6Class(
  classname = 'Brain',
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

    # Sapces
    # 1: MRI space - Experiment space
    # 2: FreeSurfer space
    sp_pial = 2,
    sp_electrodes = 1,

    # Transforms: All mat are: from space n to MRI space
    tf_2 = diag(rep(1,4))
  ),
  public = list(
    initialize = function(subject){
      if(!missing(subject)){
        if(!is(subject, 'Subject')){
          subject = stringr::str_split_fixed(subject, '/', 2)
          subject = Subject$new(project_name = subject[1], subject_code = subject[2])
        }
        # try to import electrodes from subject
        private$subject = subject

        tbl = subject$electrodes
        for(ii in seq_len(nrow(tbl))){
          row = tbl[ii,]
          self$set_electrode(which = row$Electrode, position = c(row$Coord_x, row$Coord_y, row$Coord_z), label = row$Label, show_warning = FALSE)
        }




      }
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
      if(missing(spec_file)){
        if(is.null(private$subject)){
          stop('Please provide spec_file')
        }
        # try to load from private subject
        suma_dir = private$subject$dirs$suma_dir

        # get spec file
        spec_file = file.path(suma_dir, rave_options('suma_spec_file'))
      }
      if(!file.exists(spec_file)){
        stop('spec_file not exists')
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

          mesh_data = threejsr::read.freesurf.asc(f)

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
    compute_nearest_face = function(electrode, which_pials = c(1,2), max_dist = 10){
      # get electrode info
      e = private$electrodes[[electrode]]

      nearest_face = NULL

      # apply inverse transformation:
      sp2_position = (private$tf_2[1:3, ] %*% c(e$position, -1)) * c(-1,-1,1)
      for(ii in which_pials){
        pial = private$three_pial[[ii]]
        v = pial$.__enclos_env__$private$vertices
        f = pial$.__enclos_env__$private$faces
        dim(v) = c(3, length(v) / 3)

        v = v - as.vector(sp2_position)
        # d = apply(v, 2, function(x){sum(x^2)})
        d = colSums(v^2)

        wm = which.min(d)
        md = d[[wm]]
        if(md <= max_dist){
          d = f == wm # should be sum = 6
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
    view = function(show_mesh = T, width = '100vw', height = '100vh', control_gui = F, ...){


      if(show_mesh){
        for(ii in 1:2){
          pial = private$three_pial[[ii]]
          pial$set_transform(mat = diag(c(-1,-1,1,1)), append = F)
          pial$set_transform(mat = private$tf_2, append = T)
          pial$add_visibility_control(
            type = pial$name,
            name = 'visibility',
            label = 'Show/Hide',
            initial = TRUE
          )
          pial$add_custom_control(
            type = pial$name,
            l = list(
              name = 'wireframe',
              label = 'Wireframe',
              initial = FALSE,
              callback = 'function(value, mesh){mesh.material.wireframe=value;}'
            )
          )
        }
      }
      for(ii in seq_along(private$three_electrodes)){
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
      }
      elements = c(private$three_pial, private$three_electrodes)
      threejsr:::threejs_scene.default(
        elements = elements,
        width = width,
        height = height,
        control_gui = control_gui,
        mouse_control_target = c(0,0,30),
        ...
      )
    }
  )
)



# require(rave)
# require(stringr)
# ii=1
# self = RaveBrain$new('Complete/YAB'); private = self$.__enclos_env__$private
# self$import_spec( include_electrodes = T )
#
# self$view(control_gui = T)
#
# # pial = private$three_pial[[1]]
