err_cnd <- function(
  .subclass = NULL, ..., message = "", trace = NULL,
  parent = NULL
){
  rlang::error_cnd(.subclass = .subclass, ..., message = message, trace = trace,
                   parent = parent)
}

#' Check if epoch file is valid
#' @param subject subject object or string
#' @param epoch_name epoch name to check
#' @export
check_epoch <- function(subject, epoch_name){
  if(is.character(subject)){
    sub_dir = file.path(rave_options('data_dir'), subject, 'rave')
    if(!dir.exists(sub_dir)){
      return(err_cnd(message = sprintf('No subject [%s] found', subject)))
    }

    subject = as_subject(subject, strict = FALSE)
  }

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

  # Trial must be numerical
  tl = as.integer(tbl$Trial)

  if(any(is.na(tl)) || any(duplicated(tl))){
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid trial number. Make sure they are integers and no duplicates!',
      epoch_name
    ))
    return(err)
  }

  trial_number = as.integer(tbl$Trial)

  # 2. check time points
  tm = tbl$Time
  time = as.numeric(tm)
  if(any(is.na(time))){
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid timestamp (trial %s), check it out.',
      epoch_name,
      deparse_selections(trial_number[is.na(time)])
    ))
    return(err)
  }

  tp = load_meta('time_points', subject$project_name, subject$subject_code)
  tbl$Time = as.numeric(tbl$Time)

  proc_range = lapply(split(tp$Time, tp$Block), range)
  sp = split(tbl, tbl$Block)
  wrong_range = lapply(sp, function(x){
    b = unique(x$Block)
    rg = proc_range[[b]]
    w = x$Time < rg[1] | x$Time > rg[2]
    x$Trial[w]
  })
  wrong_range = unlist(wrong_range)
  if(length(wrong_range)){
    wb = deparse_selections(as.integer(wrong_range))
    err = err_cnd(message = sprintf(
      'Epoch [%s] has invalid timestamp. Make sure they are block-wise and in **seconds**! (Check trial%s %s)',
      epoch_name,
      ifelse(length(wrong_range)>1, 's', ''),
      wb
    ))
    return(err)
  }




  return(TRUE)

}
