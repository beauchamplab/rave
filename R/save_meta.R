# pre-process functions to save meta files

#' @export
save_meta <- function(data, meta_type, project_name, subject_code){
  data_dir = rave_options('data_dir')
  meta_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'meta')

  if(!dir.exists(meta_dir)){
    dir.create(meta_dir, recursive = T)
  }

  if(meta_type == 'electrodes'){
    names(data)[1:4] = c('Channel', 'EpilepsyChan', 'BadChan', 'ExcludedChan')
    if(!'Coord_x' %in% names(data)){
      data$Coord_x = 0
      data$Coord_y = 0
      data$Coord_z = 0
      data$Label = ''
    }

    write.csv(data, file = file.path(meta_dir, 'electrodes.csv'), row.names = F)
  }else if(meta_type == 'time_points'){
    names(data) = c('Block', 'Time')
    write.csv(data, file = file.path(meta_dir, 'time_points.csv'), row.names = F)
  }else if(meta_type == 'frequencies'){
    names(data) = c('Frequency')
    write.csv(data, file = file.path(meta_dir, 'frequencies.csv'), row.names = F)
  }


}

#' @export
load_meta <- function(meta_type, project_name, subject_code, subject_id, meta_name){
  data_dir = rave_options('data_dir')
  if(missing(subject_id)){
    meta_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'meta')
  }else{
    meta_dir = file.path(data_dir, subject_id, 'rave', 'meta')
  }
  if(dir.exists(meta_dir)){
    if(meta_type == 'electrodes'){
      file = file.path(meta_dir, 'electrodes.csv')
      if(file.exists(file)){
        tbl = read.csv(file, stringsAsFactors = F)
        if(!'Label' %in% names(tbl)){
          tbl$Label = NA
        }
        return(tbl)
      }
    }else if(meta_type == 'time_points'){
      file = file.path(meta_dir, 'time_points.csv')
      if(file.exists(file)){
        return(read.csv(file, stringsAsFactors = F, colClasses = c(
          Block = 'character'
        )))
      }
    }else if(meta_type == 'frequencies'){
      file = file.path(meta_dir, 'frequencies.csv')
      if(file.exists(file)){
        return(read.csv(file, stringsAsFactors = F))
      }
    }else if(meta_type == 'epoch'){
      epoch_file = file.path(meta_dir, sprintf('epoch_%s.csv', meta_name))
      epochs = read.csv(epoch_file, header = T, stringsAsFactors = F,
                        colClasses = c('character', 'numeric', 'character'))
      return(epochs)
    }else if(meta_type == 'info'){
      info_file = file.path(meta_dir, 'info.yaml')
      info = yaml::yaml.load_file(info_file)
      return(info)
    }
  }

  return(NULL)
}
