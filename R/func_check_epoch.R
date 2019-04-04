err_cnd <- function(
  .subclass = NULL, ..., message = "", trace = NULL,
  parent = NULL
){
  rlang::error_cnd(.subclass = .subclass, ..., message = message, trace = trace,
                   parent = parent)
}

#' @export
check_epoch <- function(subject, epoch_name){
  subject = as_subject(subject, strict = FALSE)
  file_path = sprintf('%s/epoch_%s.csv', subject$dirs$meta_dir, epoch_name)

  if(!file.exists(file_path)){
    err = err_cnd(message = sprintf('No epoch file [epoch_%s] found', epoch_name))
    return(err)
  }

  tbl = read.csv(file_path, stringsAsFactors = F, colClasses = 'character')
  # 0. check if columns matches
  names = names(tbl)
  nms = c('Block', 'Time', 'Trial', 'Condition')
  has_names = nms %in% names
  if(!all(has_names)){
    err = err_cnd(message = sprintf('Epoch [%s] is invalid. Must have at least the following columns (case-sensitive): "Block", "Time", "Trial", "Condition"... (Missing: "%s")',
                                    epoch_name, paste(nms[!has_names], collapse = '", "')))
    return(err)
  }

  # 1. check block number
  epoch_blocks = unique(tbl$Block)
  bs = subject$preprocess_info('blocks')

  if(!all(epoch_blocks %in% bs)){
    err = err_cnd(message = sprintf('Epoch [%s] has invalid blocks ("%s", does not match "%s"), check it out.', epoch_name,
                                    paste(epoch_blocks, collapse = '", "'),
                                    paste(bs, collapse = '", "')
                                    ))
    return(err)
  }

  # 2. check time points
  tm = tbl$Time
  time = as.numeric(tm)
  if(any(is.na(time))){
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid timestamp (%s, ...), check it out.',
      epoch_name,
      tm[is.na(time)][1]
    ))
    return(err)
  }

  tp = load_meta('time_points', subject$project_name, subject$subject_code)
  tbl$Time = as.numeric(tbl$Time)

  proc_range = lapply(split(tp$Time, tp$Block), range)
  sp = split(tbl, tbl$Block)
  wrong_range = vapply(sp, function(x){
    b = unique(x$Block)
    rg = proc_range[[b]]
    any(x$Time < rg[1] | x$Time > rg[2])
  }, FALSE)

  if(any(wrong_range)){
    wb = names(sp)[wrong_range]
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid timestamp. Make sure they are time in **seconds**! (Check block "%s")',
      epoch_name,
      paste(wb, collapse = '", "')
    ))
    return(err)
  }


  # Trial must be numerical
  tl = as.numeric(tbl$Trial)

  if(any(is.na(tl)) || any(duplicated(tl))){
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid trial number. Make sure they are integers and no duplicates!',
      epoch_name
    ))
    return(err)
  }

  return(TRUE)

}
