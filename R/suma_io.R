
#' Make SUMA grid (I do not know what's this, John wrote it)
#' @author J.Magnnotti
#' @param con file
#' @param raw raw file or processed I guess
read_mgrid <- function(con, raw = F){
  # con = '/Volumes/data/iElVis_files/YBY/elec_recon/YBY.mgrid'
  s = readLines(con)
  ind = which(str_detect(s, '^#- - - - '))
  if(!length(ind)){
    return(NULL)
  }
  s = str_trim(s)
  re = list()
  re[['header']] = list()
  re[['groups']] = list()

  # header
  header = s[seq_len(ind[1])]
  i = which(str_detect(header, '^[^#]')); i = i[i > 1]
  val = header[i]
  key = str_remove(header[i-1], '^#')
  re[['header']][key] = val

  # grid and electrodes
  ind = cbind(ind, c(ind[-1]-1, length(s)))

  current_group = list()

  for(ii in seq_len(nrow(ind))){
    entry = s[seq(ind[ii, 1], ind[ii, 2])]
    target = str_match(entry[2], '(Electrode [a-zA-Z]*)[ ]*([0-9]*)[ ]*([0-9]*)')

    name = str_trim(target[2])

    i = which(str_detect(entry, '^[^#]')); i = i[i > 3]
    val = entry[i]
    key = str_remove(entry[i-1], '^#')
    tmp = list()
    tmp[key] = val

    switch (
      name,
      'Electrode Grid' = {
        # this is a grid decription
        if(length(current_group)){
          re[['groups']][[length(re[['groups']]) + 1]] = current_group
        }
        current_group = list()
        current_group[['group_number']] = as.integer(target[3]) + 1
        group_info = list()
        group_info[['group_name']] = tmp[['Description']]
        if(!is.null(tmp[['Radius']])){
          group_info[['radius']] = as.numeric(tmp[['Radius']]);
        }
        if(is_invalid(group_info[['radius']])){
          group_info[['radius']] = 2
        }

        if(!is.null(tmp[['Thickeness']])){
          group_info[['thickeness']] = as.numeric(tmp[['Thickeness']]);
        }
        if(is_invalid(group_info[['thickeness']])){
          group_info[['thickeness']] = 0.05
        }

        if(!is.null(tmp[['Color']])){
          group_info[['color']] = as.numeric(unlist(str_split(tmp[['Color']], '[^0-9\\.]+')))
        }
        if(is_invalid(group_info[['color']])){
          group_info[['color']] = c(0.5, 0.5, 0.5)
        }

        if(!is.null(tmp[['Dimensions']])){
          group_info[['dimension']] = as.integer(unlist(str_split(tmp[['Dimensions']], '[^0-9\\.]+')))
        }
        if(is_invalid(group_info[['dimension']])){
          group_info[['dimension']] = NULL
        }

        current_group[['group_info']] = group_info
      },
      'Electrode' = {
        # this is electrode
        elec_info = list()

        # position
        pos = tmp[['Position']]
        pos %?<-% '0 0 0'
        pos = as.numeric(unlist(str_extract_all(pos, '[0-9\\.]+')))
        if(length(pos) != 3){
          pos = c(0,0,0)
        }
        elec_info[['position']] = pos

        elec_info[['place']] = as.integer(target[3:4])


        current_group[['electrodes']] %?<-% list()
        current_group[['electrodes']][[length(current_group[['electrodes']]) + 1]] = elec_info
      }
    )

  }

  if(length(current_group)){
    re[['groups']][[length(re[['groups']]) + 1]] = current_group
  }

  # data check and finalize
  re[['groups']] = dropNulls(lapply(re$groups, function(group){
    if(!length(group$electrodes)){
      return()
    }
    dimension = group$group_info$dimension
    g_name = group$group_info$group_name
    if(is.null(g_name) || g_name == ''){
      g_name = paste0('G', group$group_number)
    }

    places = sapply(group$electrodes, function(e){
      as.integer(e$place)
    })
    width = max(places[2,]) + 1

    group$electrodes = lapply(group$electrodes, function(e){
      p = as.integer(e$place)
      e$electrode_number = p[1] * width + p[2] + 1
      e$electrode_name = sprintf('%s_%d', g_name, e$electrode_number)
      e
    })

    names(group$electrodes) = sapply(group$electrodes, '[[', 'electrode_name')
    group
  }))

  if(!raw){
    header = re$header

    lapply(re$groups, function(group){
      group_name = group$group_info$group_name
      radius = group$group_info$radius
      thickness = group$group_info$thickeness
      color = group$group_info$color
      tryCatch({
        rgb(color[1],color[2],color[3], maxColorValue = 1)
      },error = function(e){
        rgb(0.5,0.5,0.5)
      }) ->
        color

      do.call(rbind, lapply(group$electrodes, function(e){
        data.frame(
          'Coord_x' = e$position[1],
          'Coord_y' = e$position[2],
          'Coord_z' = e$position[3],
          'Label' = e$electrode_name,
          'Group' = group_name,
          'Group_Radius' = radius,
          'Group_Thickness' = thickness,
          'Group_Color' = color,
          'Reference' = 'noref'
        )
      }))
    }) ->
      re
    re = do.call(rbind, re)
    re$Electrode = seq_len(nrow(re))

    re = list(
      electrodes = re[, c('Electrode', 'Coord_x', 'Coord_y', 'Coord_z', 'Label')],
      reference = re[, c('Electrode', 'Group', 'Reference', 'Group_Radius', 'Group_Thickness', 'Group_Color')]
    )
  }

  return(re)

}


#' Parse spec file
#' @usage suma_spec_parse(subject)
#' @param subject Either characters such as 'Project/Subject' or Subject object created by Subject$new(...)
#' @param spec_file default decided by rave_options('suma_spec_file'), depending on subjects
#' @examples
#' \dontrun{
#' subject = 'Demo/Subject'
#' # or create subject object
#' subject = Subject$new('Demo', 'Subject')
#'
#' suma_spec_parse(subject)
#' }
#' @export
suma_spec_parse <- function(subject, spec_file){
  if(missing(spec_file)){
    if(is.character(subject)){
      subject = str_split_fixed(subject, '/', 2)
      subject = Subject$new(project_name = subject[1], subject_code = subject[2], strict = FALSE)
    }
    suma_dir = subject$dirs$suma_dir
    spec_file %?<-% file.path(suma_dir, fprintf('${{rave_options("suma_spec_file")}}'))
    if(!file.exists(spec_file)){
      spec_file = file.path(suma_dir, spec_file)
    }
  }

  s = readLines(spec_file)

  s = s[!str_detect(s, '^[\\ \\t]*#')]


  ind = which(str_detect(s, '^[\\ \\t]*NewSurface[\\ \\t]*$'))
  surface = list()
  if(length(ind)){
    ind = cbind(ind+1, c(ind[-1]-1, length(s)))
    ind = ind[ind[,1] <= ind[,2],,drop = F]
    n = nrow(ind)
    if(n > 0){
      lapply(seq_len(n), function(ii){
        entry = s[seq(ind[ii,1], ind[ii,2])]
        # try to evaluate them
        entry = str_split_fixed(entry, '=', 2)
        entry[, 2] = str_trim(entry[, 2])
        entry[, 1] = str_trim(entry[, 1])
        entry = entry[entry[,2] != '', , drop = F]
        val = as.list(entry[,2])
        names(val) = entry[,1]
        val
      }) ->
        surface
    }
  }
  return(surface)
}


#' Parse surface volumn file header
#' @param file_path path to brik or head file
#' @export
suma_surface_volume_parse <- function(file_path){
  if(str_detect(str_to_lower(file_path), '\\.brik$')){
    file_path = str_replace(file_path, '\\.[bB][rR][iI][kK]$', '.head')
  }
  if(!str_detect(str_to_lower(file_path), '\\.head$')){
    file_path = paste0(file_path, '.head')
  }
  # load file
  s = readLines(file_path)
  ind = which(str_detect(s, '^[\\ \\t]*type[\\ \\t]*=[\\ \\t]*[a-zA-Z0-9]+\\-attribute'))

  values = list()

  match_extract = function(x, p, row = 1, col = 2){
    s = str_match(x, p)
    if(length(s)){
      if(nrow(s) >= row && ncol(s) >= col){
        return(s[row, col])
      }
    }
    return(NA)
  }

  if(length(ind)){
    ind = cbind(ind, c(ind[-1]-1, length(s)))
    n = nrow(ind)
    for(ii in seq_len(n)){
      sub = str_trim(s[seq(ind[ii, 1], ind[ii, 2])])
      entry = paste(sub, collapse = '|')
      # parse the first three rows
      type = match_extract(entry, 'type[\\ \\t]*=[\\ \\t]*([\\w]+)\\-attribute[\\|]')
      name = match_extract(entry, 'name[\\ \\t]*=[\\ \\t]*([\\w]+)[\\|]')
      count = match_extract(entry, 'count[\\ \\t]*=[\\ \\t]*([0-9]+)[\\|]')

      if(!any(is.na(type), is.na(name), is.na(count)) && length(sub) >= 4){
        type = str_to_lower(type)
        count = as.integer(count)
        name = str_to_upper(name)

        switch (type,
          'string' = {
            sub = paste0(sub[-(1:3)], collapse = '')
            str_sub(sub, end = count)
          },
          'integer' = {
            sub = paste0(sub[-(1:3)], collapse = ' ')
            val = as.integer(unlist(str_split(sub, '[ \\.]+')))
            if(length(val) > count){
              val = val[seq_len(count)]
            }
            val
          },
          'float' = {
            sub = paste0(sub[-(1:3)], collapse = ' ')
            val = as.numeric(unlist(str_split(sub, '[ ]+')))
            if(length(val) > count){
              val = val[seq_len(count)]
            }
            val
          },{
            sub
          }
        ) ->
          val

        values[[name]] = list(
          type = type,
          count = count,
          value = val
        )
      }
    }
  }

  return(values)
}



#' Function to parse SUMA spec file and generate threejsr free mesh object
#' @param subject Subject class or character
#' @param spec_file default is subject_dir/rave/suma/test.spec
#' @param state is pial
#' @param center Geom center
freesurfer_mesh <- function(subject, spec_file = NULL, state = 'pial', center = c(0,0,30)){
  if(is.character(subject)){
    subject = str_split_fixed(subject, '/', 2)
    subject = Subject$new(project_name = subject[1], subject_code = subject[2], strict = FALSE)
  }

  spec_info = suma_spec_parse(subject, spec_file = spec_file)
  volume_file = unlist(
    lapply(spec_info, '[[', 'SurfaceVolume')
  )

  mat = diag(c(1,1,1,1))
  if(length(volume_file)){
    volume_file = unique(volume_file)
    # get file name only
    file_name = tail(unlist(str_split(volume_file, '/|\\\\')), 1)
    volume_info = suma_surface_volume_parse(file.path(subject$dirs$suma_dir, file_name))


    if(!is.null(volume_info[['VOLREG_MATVEC_000000']])){
      mat = volume_info[['VOLREG_MATVEC_000000']][['value']]
      if(length(mat) != 16){
        mat = c(mat[seq_len(12)], 0,0,0,1)
      }
      mat = matrix(mat, nrow = 4, ncol = 4, byrow = T)
    }else if(!is.null(volume_info[['VOLREG_ROTCOM_000000']])){
      mat = volume_info[['VOLREG_ROTCOM_000000']][['value']]
    }
  }

  # load mesh
  GeomFreeMesh = get_from_package('GeomFreeMesh', pkg = 'threejsr', check = FALSE)

  lapply(spec_info, function(info){
    surf_name = info[['FreeSurferSurface']]
    surf_name %?<-% info[['SurfaceName']]
    surf_state = info[['SurfaceState']]
    format = info[['SurfaceFormat']]
    format %?<-% 'Unknown'
    format = str_to_lower(format)

    if(length(surf_state) && surf_state %in% state && format == 'ascii'){
      f = file.path(subject$dirs$suma_dir, surf_name)
      if(length(f) && file.exists(f)){
        mesh_data = read.freesurf.asc(f)

        # generate threejsr GEOM object
        mesh = GeomFreeMesh$new(
          position = -center,
          mesh_name = surf_name,
          mesh_info = surf_name,
          vertices = mesh_data$vertices,
          faces = mesh_data$faces,
          hover_enabled = F,
          is_clipper = F
        )

        return(list(
          surf_name = surf_name,
          surf_state = surf_state,
          meta_info = info,
          geom_mesh = mesh
        ))
      }
    }

    return(NULL)
  }) ->
    surf_info

  surf_info = dropNulls(surf_info)
  meshes = sapply(surf_info, '[[', 'geom_mesh')
  if(length(meshes)){
    names(meshes) = sapply(surf_info, '[[', 'surf_name')
  }


  # load electrodes
  # meshes = NULL
  n = nrow(subject$electrodes)

  GeomSphere = get_from_package('GeomSphere', pkg = 'threejsr', check = FALSE)
  lapply(seq_len(n), function(ii){
    row = subject$electrodes[ii,]
    mat %?<-% diag(rep(1,4))
    pos = c(row$Coord_x, row$Coord_y, row$Coord_z, -1)
    pos = mat[1:3,] %*% pos
    pos = pos * c(-1,-1,1) - center

    GeomSphere$new(
      position = pos,
      mesh_name = fprintf('Electrode ${{row$Electrode}}'),
      radius = 2
    ) ->
      e
    e
  }) ->
    electrodes
  list(
    mesh = meshes,
    electrodes = electrodes,
    mat = mat
  )
}






