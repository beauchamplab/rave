# pre-process functions to save meta files

safe_write_csv <- function(data, file, ...){
  if(file.exists(file)){
    oldfile = str_replace(file, '\\.[cC][sS][vV]$', strftime(Sys.time(), '_[%Y%m%d_%H%M%S].csv'))
    logger('Renaming file ', file, ' >> ', oldfile)
    file.rename(file, oldfile)
  }
  write.csv(data, file, ...)
}

#' @export
save_meta <- function(data, meta_type, project_name, subject_code){
  data_dir = rave_options('data_dir')
  meta_dir = file.path(data_dir, sprintf('%s/%s', project_name, subject_code), 'rave', 'meta')

  if(!dir.exists(meta_dir)){
    dir.create(meta_dir, recursive = T)
  }

  if(meta_type == 'electrodes'){
    names(data)[1] = c('Electrode')
    if(!'Coord_x' %in% names(data)){
      # try not to overwrite original data
      data$Coord_x = 0
      data$Coord_y = 0
      data$Coord_z = 0
      data$Label = ''
    }

    safe_write_csv(data, file = file.path(meta_dir, 'electrodes.csv'), row.names = F)
  }else if(meta_type == 'time_points'){
    names(data) = c('Block', 'Time')
    safe_write_csv(data, file = file.path(meta_dir, 'time_points.csv'), row.names = F)
  }else if(meta_type == 'frequencies'){
    names(data) = c('Frequency')
    safe_write_csv(data, file = file.path(meta_dir, 'frequencies.csv'), row.names = F)
  }else if(meta_type == 'time_excluded'){
    if(!is.data.frame(data)){
      data = as.data.frame(data, stringsAsFactors = F)
    }
    if(nrow(data)){
      names(data) = c('Block', 'Start', 'End')
      safe_write_csv(data, file = file.path(meta_dir, 'time_excluded.csv'), row.names = F)
    }
  }


}

#' @export
load_meta <- function(meta_type, project_name, subject_code, subject_id, meta_name){
  data_dir = rave_options('data_dir')
  if(missing(subject_id)){
    meta_dir = file.path(data_dir, sprintf('%s/%s', project_name, subject_code), 'rave', 'meta')
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
    }
    else if(meta_type == 'time_points'){
      file = file.path(meta_dir, 'time_points.csv')
      if(file.exists(file)){
        return(read.csv(
          file,
          stringsAsFactors = F,
          colClasses = c(Block = 'character')
        ))

      }
    }
    else if(meta_type == 'time_excluded'){
      # Read time_excluded.csv if exists
      time_excluded_path = file.path(meta_dir, 'time_excluded.csv')
      if(file.exists(time_excluded_path)){
        return(read.csv(
          time_excluded_path,
          stringsAsFactors = F,
          colClasses = c(Block = 'character')
        ))
      }else{
        return(data.frame(
          Block = NULL,
          Electrode = NULL,
          Start = NULL,
          End = NULL
        ))
      }
    }
    else if(meta_type == 'frequencies'){
      file = file.path(meta_dir, 'frequencies.csv')
      if(file.exists(file)){
        return(read.csv(file, stringsAsFactors = F))
      }
    }
    else if(meta_type == 'epoch'){
      epoch_file = file.path(meta_dir, sprintf('epoch_%s.csv', meta_name))
      default_cols = c('Block', 'Time', 'Trial', 'Condition', 'Duration')

      epochs = read.csv(epoch_file, header = T, stringsAsFactors = F,
                        colClasses = 'character')
      epochs$Time = as.numeric(epochs$Time)
      epochs$Trial = as.numeric(epochs$Trial)
      epochs$Duration %?<-% NA
      epochs$Duration = as.numeric(epochs$Duration)

      trial_path = file.path(meta_dir, 'trials.csv')
      if(file.exists(trial_path)){
        trials = read.csv(trial_path, stringsAsFactors = F)
        epochs = merge(epochs, trials, by = 'Trial', all.x = T, sort = F, suffixes = c('', '_y'))
      }
      epochs$Condition %?<-% 'NoCondition'
      epochs$Condition[is.na(epochs$Condition)] = 'NoCondition'
      epochs$Condition = as.character(epochs$Condition)
      # sort column orders
      nms = names(epochs)
      nms = c(default_cols, nms[!nms %in% default_cols])
      epochs = epochs[, nms]
      return(epochs)
    }
    else if(meta_type == 'info'){
      info_file = file.path(meta_dir, 'info.yaml')
      info = yaml::yaml.load_file(info_file)
      return(info)
    }
    else if(meta_type == 'time_excluded'){
      file = file.path(meta_dir, 'time_excluded.csv')
      if(!file.exists(file)){
        return(NULL)
      }
      time_excluded = read.csv(file, header = T, stringsAsFactors = F,
                        colClasses = c('character', 'numeric', 'numeric'))
      return(time_excluded)
    }
    else if(meta_type == 'references'){
      file = file.path(meta_dir, sprintf('reference_%s.csv', meta_name))
      if(!file.exists(file)){
        return(NULL)
      }
      ref_tbl = read.csv(file, header = T, stringsAsFactors = F)
      if(names(ref_tbl)[1] != 'Electrode'){
        ref_tbl = ref_tbl[,-1]
      }
      return(ref_tbl)
    }
  }

  return(NULL)
}
