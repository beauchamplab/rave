# SUMA files IO


#' Make \code{iElvis} \code{mgrid} file
#' @author John Magnotti
#' @param con \code{mgrid} file
#' @param raw raw file or processed I guess
read_mgrid <- function(con, raw = F){
  soft_deprecated()
  # con = '/Volumes/data/iElVis_files/YBY/elec_recon/YBY.mgrid'
  s = readLines(con)
  ind = which(stringr::str_detect(s, '^#- - - - '))
  if(!length(ind)){
    return(NULL)
  }
  s = stringr::str_trim(s)
  re = list()
  re[['header']] = list()
  re[['groups']] = list()
  
  # header
  header = s[seq_len(ind[1])]
  i = which(stringr::str_detect(header, '^[^#]')); i = i[i > 1]
  val = header[i]
  key = stringr::str_remove(header[i-1], '^#')
  re[['header']][key] = val
  
  # grid and electrodes
  ind = cbind(ind, c(ind[-1]-1, length(s)))
  
  current_group = list()
  
  for(ii in seq_len(nrow(ind))){
    entry = s[seq(ind[ii, 1], ind[ii, 2])]
    target = stringr::str_match(entry[2], '(Electrode [a-zA-Z]*)[ ]*([0-9]*)[ ]*([0-9]*)')
    
    name = stringr::str_trim(target[2])
    
    i = which(stringr::str_detect(entry, '^[^#]')); i = i[i > 3]
    val = entry[i]
    key = stringr::str_remove(entry[i-1], '^#')
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
          group_info[['color']] = as.numeric(unlist(stringr::str_split(tmp[['Color']], '[^0-9\\.]+')))
        }
        if(is_invalid(group_info[['color']])){
          group_info[['color']] = c(0.5, 0.5, 0.5)
        }
        
        if(!is.null(tmp[['Dimensions']])){
          group_info[['dimension']] = as.integer(unlist(stringr::str_split(tmp[['Dimensions']], '[^0-9\\.]+')))
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
        pos = as.numeric(unlist(stringr::str_extract_all(pos, '[0-9\\.]+')))
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
  re[['groups']] = dipsaus::drop_nulls(lapply(re$groups, function(group){
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
        grDevices::rgb(color[1],color[2],color[3], maxColorValue = 1)
      },error = function(e){
        grDevices::rgb(0.5,0.5,0.5)
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


#' Parse \code{SUMA} \code{spec} file
#' @param subject either characters in format like \code{'Project/Subject'} or 
#' \code{\link[rave]{Subject}} object created by \code{Subject$new(...)}
#' @param spec_file default decided by \code{rave_options('suma_spec_file')}, 
#' depending on subjects
#' @examples
#' 
#' \dontrun{
#' subject = 'Demo/YAB'
#' # or create subject object
#' subject = Subject$new('Demo', 'YAB')
#' 
#' # Please download sample subjects first to run
#' suma_spec_parse(subject)
#' }
suma_spec_parse <- function(subject, spec_file){
  soft_deprecated()
  if(missing(spec_file)){
    if(is.character(subject)){
      subject = stringr::str_split_fixed(subject, '/', 2)
      subject = Subject$new(project_name = subject[1], subject_code = subject[2], strict = FALSE)
    }
    suma_dir = subject$dirs$suma_dir
    spec_file %?<-% file.path(suma_dir, catgl('{rave_options("suma_spec_file")}'))
    if(!file.exists(spec_file)){
      spec_file = file.path(suma_dir, spec_file)
    }
  }
  
  s = readLines(spec_file)
  
  s = s[!stringr::str_detect(s, '^[\\ \\t]*#')]
  
  
  ind = which(stringr::str_detect(s, '^[\\ \\t]*NewSurface[\\ \\t]*$'))
  surface = list()
  if(length(ind)){
    ind = cbind(ind+1, c(ind[-1]-1, length(s)))
    ind = ind[ind[,1] <= ind[,2],,drop = F]
    n = nrow(ind)
    if(n > 0){
      lapply(seq_len(n), function(ii){
        entry = s[seq(ind[ii,1], ind[ii,2])]
        # try to evaluate them
        entry = stringr::str_split_fixed(entry, '=', 2)
        entry[, 2] = stringr::str_trim(entry[, 2])
        entry[, 1] = stringr::str_trim(entry[, 1])
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


#' Parse \code{AFNI} \code{BRIK/HEAD} file
#' @param file_path path to \code{BRIK} or \code{HEAD} file
suma_surface_volume_parse <- function(file_path){
  soft_deprecated()
  if(stringr::str_detect(stringr::str_to_lower(file_path), '\\.brik$')){
    file_path = stringr::str_replace(file_path, '\\.[bB][rR][iI][kK]$', '.head')
  }
  if(!stringr::str_detect(stringr::str_to_lower(file_path), '\\.head$')){
    file_path = paste0(file_path, '.head')
  }
  # load file
  s = readLines(file_path)
  ind = which(stringr::str_detect(s, '^[\\ \\t]*type[\\ \\t]*=[\\ \\t]*[a-zA-Z0-9]+\\-attribute'))
  
  values = list()
  
  match_extract = function(x, p, row = 1, col = 2){
    s = stringr::str_match(x, p)
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
      sub = stringr::str_trim(s[seq(ind[ii, 1], ind[ii, 2])])
      entry = paste(sub, collapse = '|')
      # parse the first three rows
      type = match_extract(entry, 'type[\\ \\t]*=[\\ \\t]*([\\w]+)\\-attribute[\\|]')
      name = match_extract(entry, 'name[\\ \\t]*=[\\ \\t]*([\\w]+)[\\|]')
      count = match_extract(entry, 'count[\\ \\t]*=[\\ \\t]*([0-9]+)[\\|]')
      
      if(!any(is.na(type), is.na(name), is.na(count)) && length(sub) >= 4){
        type = stringr::str_to_lower(type)
        count = as.integer(count)
        name = stringr::str_to_upper(name)
        
        switch (type,
                'string' = {
                  sub = paste0(sub[-(1:3)], collapse = '')
                  stringr::str_sub(sub, end = count)
                },
                'integer' = {
                  sub = paste0(sub[-(1:3)], collapse = ' ')
                  val = as.integer(unlist(stringr::str_split(sub, '[ \\.]+')))
                  if(length(val) > count){
                    val = val[seq_len(count)]
                  }
                  val
                },
                'float' = {
                  sub = paste0(sub[-(1:3)], collapse = ' ')
                  val = as.numeric(unlist(stringr::str_split(sub, '[ ]+')))
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






# By Dipterix
launch_suma <- function(
  root_dir, spec_file
){
  soft_deprecated()
  if(missing(spec_file)){
    spec_file = rave_options('suma_spec_file')
  }
  # make everything absolute
  suma_path = try_normalizePath(rave_options('suma_path'))
  spec_file = try_normalizePath(file.path(root_dir, spec_file))
  
  # El Captitan 'DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace'
  # High Sierra "DYLD_FALLBACK_LIBRARY_PATH=/Applications/AFNI"
  suma_lib = rave_options('suma_lib')
  
  
  wd = getwd()
  on.exit({setwd(wd)})
  if(dir.exists(root_dir)){
    setwd(root_dir)
    system2('suma',
            args = c('-spec', sprintf('"%s"', spec_file)),
            env = c(sprintf('PATH=$PATH:"%s"', suma_path),
                    suma_lib),
            wait = F)
  }
  
  setwd(wd)
}



read.rosa <- function(path){
  soft_deprecated()
  # path = '~/Downloads/XXXXXXXXXXX 20181113 190540.ros'
  regex_number = '[-]{0,1}[0-9]*[.]{0,1}[0-9]*'
  re = list()
  dat = stringr::str_trim(readLines(path))
  # Trajectory
  idx_trajectory = which(dat == '[TRAJECTORY]')
  n_trajectory = as.integer( dat[ idx_trajectory+1 ] )
  
  idx_acpc = which(dat == '[ACPC]')
  trajectory = stringr::str_match(dat[idx_trajectory : idx_acpc], sprintf(
    '^([^\\ ]+) 0 [0-9]+ 1 (%s) (%s) (%s) [012] (%s) (%s) (%s) %s %s',
    regex_number, regex_number, regex_number, regex_number, regex_number, 
    regex_number, regex_number, regex_number
  ))
  is_traj = rowSums(!is.na(trajectory)) == 8
  if(sum(is_traj) != n_trajectory){
    catgl(sprintf('Trajectory number does not match: expected %s, found %s', n_trajectory, sum(is_traj)), level = 'WARNING')
  }
  anchors = trajectory[is_traj,3:8,drop=FALSE]
  anchors = as.numeric(anchors)
  dim(anchors) = c(n_trajectory, 6)
  anchors = apply(anchors, 1, function(x){
    list(
      start = x[4:6],
      end = x[1:3]
    )
  })
  names(anchors) = trajectory[is_traj, 2]
  
  re$trajectory = anchors
  re
}


#
# EXAMPLE USAGE
#
# write.niml(matrix(1:10, nrow=5), 1:5, c('a', 'b'), 'test') %>% system
#
# if your matrix already has column names and row names (which should be integer) the function
# will use them


# this function returns a command that needs to be run in order to actually create
# the niml file. This is done to allow parallel creation of the ConvertDset scripts and then
# batch execution of the commands and handles path issues that sometimes result from calilng
# system() within R
write.niml <- function(values_matrix, electrode_numbers=NULL, value_labels=NULL, prefix='', add_electrodes_as_column=TRUE,
                       value_file='__vals.dat', index_file='__ind.dat',
                       work_dir = './', ...) {
  soft_deprecated()
  
  AFNI_PATH = try_normalizePath(rave_options('suma_path'))
  faces_per_electrode = rave_options('suma_nodes_per_electrodes')
  
  fname = paste0(prefix, '_', stringr::str_replace_all(Sys.time(), '\\ |:|/', '_'))
  niml_fname <- paste0(fname, '.niml.dset')
  csv_fname = paste0(fname, '.csv')
  
  # get useful defaults
  # if value_labels weren't passed in, maybe there are column names?
  value_labels %?<-% colnames(values_matrix)
  # if these are still null, then just set an increasing number
  value_labels %?<-% paste0('Val_',  (1:ncol(values_matrix)))
  
  # if electrode_numbers weren't passed in, maybe there are row.names?
  if(length(electrode_numbers) != nrow(values_matrix)){
    electrode_numbers = 1:nrow(values_matrix)
  }else{
    electrode_numbers = as.numeric(electrode_numbers)
  }
  
  # adding the electrode number as a column is a nice thing to do
  if(add_electrodes_as_column){
    values_matrix <- cbind(electrode_numbers, values_matrix)
    value_labels <- c('e#', value_labels)
  }
  
  # duplicate indices and values to match #faces in SUMA spheres
  indices <- rep(electrode_numbers, each=faces_per_electrode)
  
  values_matrix = apply(values_matrix, 2, as.numeric)
  values_matrix[is.na(values_matrix)] = 0
  
  values <- values_matrix[rep(1:nrow(values_matrix), each=faces_per_electrode),]
  
  values = matrix(as.numeric(values), ncol = length(value_labels), byrow = F)
  
  # Turn the electrode ID into an ascending vertex ID,
  # this is aware that electrode numbers may not be sequential and AFNI starts at 0
  indices = (indices - 1) * faces_per_electrode + seq(0, faces_per_electrode - 1)
  
  # write out the values and indicies files
  if(!dir.exists(work_dir)){
    dir.create(work_dir, recursive = TRUE)
  }
  work_dir = try_normalizePath(work_dir)
  value_file = file.path(work_dir, value_file)
  index_file = file.path(work_dir, index_file)
  niml_fname = file.path(work_dir, niml_fname)
  csv_fname = file.path(work_dir, csv_fname)
  
  utils::write.csv(values_matrix, csv_fname, row.names = F)
  
  mapply(function(x, file) {
    utils::write.table(x, file, row.names=FALSE, col.names=FALSE)
  },list(values, indices), c(value_file, index_file))
  
  cmd <- sprintf("%s/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -dset_labels '%s' -prefix %s",
                 AFNI_PATH, value_file, index_file, paste0(value_labels, collapse=' '), niml_fname)
  
  attr(cmd, which = 'path') <- niml_fname
  
  catgl('For full cleanup, AFTER running the ConvertDset command, delete: ', value_file, ' and ', index_file)
  
  print(list(
    args = c(
      '-o_niml', '',
      '-input', sprintf('"%s"', value_file),
      '-i_1D -node_index_1D', sprintf('"%s"', index_file),
      '-dset_labels', sprintf("'%s'", paste0(value_labels, collapse=' ')),
      '-prefix', sprintf('"%s"', niml_fname)
    ),
    env = c(sprintf('PATH=$PATH:"%s"', AFNI_PATH),
            rave_options('suma_lib'))
  ))
  system2(
    'ConvertDset',
    args = c(
      '-o_niml', '',
      '-input', sprintf('"%s"', value_file),
      '-i_1D', '',
      '-node_index_1D', sprintf('"%s"', index_file),
      '-dset_labels', sprintf("'%s'", paste0(value_labels, collapse=' ')),
      '-prefix', sprintf('"%s"', niml_fname)
    ),
    env = c(sprintf('PATH=$PATH:"%s"', AFNI_PATH),
            rave_options('suma_lib')),
    wait = F
  )
  
  return (cmd)
}
