#' Check if default data environment has object
#' @param var variable name
#' @param any whether all variables should be present of any variables should 
#' exist
#' @return Logical \code{TRUE} or \code{FALSE} indicating the existence of the 
#' variables 
check_data_repo <- function(var = c('subject'), any = FALSE){
  data_repo = getDefaultDataRepository()
  sel = var %in% names(data_repo)
  if(any && sum(sel)){
    return(TRUE)
  }
  if(!any && sum(!sel) == 0){
    return(TRUE)
  }
  
  return(FALSE)
}


#' check subject validity tools
#' @param project_name project_name
#' @param subject_code subject_code
#' @param quiet logical
check_subjects2 <- function(
  project_name, subject_code, quiet = FALSE
){
  if(quiet){
    # do not print
    cat2 = function(...){}
  }
  
  cat2('Checking: Project - ', project_name, ', Subject - ', subject_code)
  dirs = get_dir(subject_code = subject_code, project_name = project_name)
  re = list()
  # 1. Check folders
  
  # subject folder - 'project/subject'
  re[['subject_dir']] = dir.exists(dirs$subject_dir)
  
  # RAVE dir - 'project/subject/rave'
  re[['rave_dir']] = dir.exists(dirs$rave_dir)
  
  # Preprocessing dir - project/subject/rave/preprocess
  re[['preprocess_dir']] = dir.exists(dirs$preprocess_dir)
  
  # meta dir - project/subject/rave/meta
  re[['meta_dir']] = dir.exists(dirs$meta_dir)
  
  # chennel_dir - project/subject/rave/data
  re[['channel_dir']] = dir.exists(dirs$channel_dir)
  
  # power_dir - project/subject/rave/data/power
  re[['power_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'power'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'power', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'power', 'ref'))
    )
  )
  
  # phase_dir - project/subject/rave/data/phase
  re[['phase_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'phase'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'phase', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'phase', 'ref'))
    )
  )
  
  # volt_dir - project/subject/rave/data/voltage
  re[['volt_dir']] = (
    dir.exists(file.path(dirs$cache_dir, 'voltage'))
    || (
      dir.exists(file.path(dirs$cache_dir, 'cache', 'voltage', 'raw'))
      && dir.exists(file.path(dirs$cache_dir, 'cache', 'voltage', 'ref'))
    )
  )
  
  
  ## Preprocess information
  log_data = list()
  
  if(re[['rave_dir']]){
    # check if preprocess log is present
    if(re[['preprocess_dir']]){
      pre_yaml_file = file.path(dirs$preprocess_dir, 'rave.yaml')
      if(file.exists(pre_yaml_file)){
        pre_hist = yaml::read_yaml(pre_yaml_file)
        log_data[['preprocess']] = pre_hist
      }
    }
    
    # update log.yaml
    save_log = T
    yaml_file = file.path(dirs$rave_dir, 'log.yaml')
    if(file.exists(yaml_file)){
      log_data_old = yaml::read_yaml(yaml_file)
      if(!is.null(log_data[['preprocess']])){
        # compare
        if(identical(log_data_old[['preprocess']], log_data[['preprocess']], num.eq = T, ignore.environment = T, ignore.bytecode = T)){
          cat2('Cached log.yaml shares the same information with preprocess log file. No need to re-cache')
          save_log = F
        }else{
          log_data_old[['preprocess']] = log_data[['preprocess']]
        }
        log_data = log_data_old
      }
    }
    
    # save to log.yaml
    if(save_log){
      cat2('Creating/replacing log.yaml...')
      yaml::write_yaml(log_data, yaml_file, fileEncoding = 'utf-8')
    }
    
  }
  
  # if log_data is not null, then we have preprocess information
  if(is.null(log_data) || is.null(log_data[['preprocess']])){
    checklevel = 0
  }else{
    checklevel = log_data$preprocess$checklevel
    if(length(checklevel)!=1 || !is.numeric(checklevel)){
      checklevel = 0
    }
  }
  val = c(rep(T, checklevel), rep(F, 10))[1:4]
  key = c('started_preprocess', 'notch_filter', 'wavelet', 'reference')
  re[key] = as.list(val)
  
  
  # Check meta files
  re[['meta_electrode']] = file.exists(file.path(dirs$meta_dir, 'electrodes.csv'))
  re[['meta_time']] = file.exists(file.path(dirs$meta_dir, 'time_points.csv'))
  re[['meta_frequency']] = file.exists(file.path(dirs$meta_dir, 'frequencies.csv'))
  
  # Find epoch and referenced
  re[['meta_epoch']] = length(list.files(dirs$meta_dir, pattern = '^epoch_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')) > 0
  re[['meta_reference']] = length(list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')) > 0
  
  # check validity
  if((re$reference && !re$meta_reference) || (re$wavelet && !re$reference)){
    # create a default reference table with noref for all electrodes
    tbl = data.frame(
      Electrode = log_data$preprocess$channels,
      Group	= 'noref',
      Reference	= 'noref',
      Type = 'No Reference',
      stringsAsFactors = F
    )
    ref_file = file.path(dirs$meta_dir, 'reference_default.csv')
    if(!file.exists(ref_file)){
      utils::write.csv(tbl, ref_file, row.names = FALSE)
      re$meta_reference = TRUE
      re$reference = TRUE
    }
  }
  
  # get references
  references = list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')
  
  if(re$reference || length(references)){
    references = list.files(dirs$meta_dir, pattern = '^reference_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')
    references = stringr::str_match(references, '^reference_([a-zA-Z0-9_]*)\\.[cC][sS][vV]$')[,2]
    re$reference = TRUE
  }else{
    references = ''
  }
  
  # Get epoch names
  if(re$meta_epoch){
    epochs = list.files(dirs$meta_dir, pattern = '^epoch_[a-zA-Z0-9_]*\\.[cC][sS][vV]$')
    epochs = stringr::str_match(epochs, '^epoch_([a-zA-Z0-9_]*)\\.[cC][sS][vV]$')[,2]
  }else{
    epochs = ''
  }
  
  
  
  list(check = re, log = log_data, epochs = epochs, references = references)
}



#' check subject validity tools (use check_subjects2)
#' @param project_name project_name
#' @param subject_code subject_code
#' @param check check is internally used
#' @param folders folders to check
#' @param preprocess preprocess to check
#' @param Meta Meta to check
check_subjects <- function(
  project_name, subject_code, check = TRUE,
  folders = c('Subject Folder', 'RAVE Folder', 'Preprocessing Folder', 'Meta Folder', 'Channel Folder'),
  preprocess = c('Started Preprocess', 'Notch Filter', 'Wavelet'),
  Meta = c("Electrode File", "Time point File", "Frequency File", "Epoch File")
){
  utils = rave_preprocess_tools()
  miss_subject_code = missing(subject_code)
  miss_project_name = missing(project_name)
  if(miss_project_name){
    projects = get_projects()
  }else{
    projects = project_name
  }
  sapply(projects, function(project_name){
    if(miss_subject_code){
      sc = list.dirs(file.path(rave_options('data_dir'), project_name), full.names = F, recursive = F)
    }else{
      sc = subject_code
    }
    sapply(sc, function(subject_code){
      if(!check){
        return(paste0(project_name, '/', subject_code))
      }
      
      # Need to check if preprocess folder exists
      # dirs = get_dir(subject_code = subject_code, project_name = project_name)
      re = utils$check_load_subject(subject_code = subject_code, project_name = project_name)
      errs = list()
      l = unlist(re$Folders[folders]); l = l[!l]
      if(length(l)){
        errs[['Subject hierarchy is wrong. Please check existence of following folders: ']] = names(l)
      }
      
      l = unlist(re$Preprocess[preprocess]); l = l[!l]
      if(length(l)){
        errs[['Preprocess is needed: ']] = names(l)
      }
      
      l = unlist(re$Meta[Meta]); l = l[!l]
      if(length(l)){
        errs[['Subject meta file missing: ']] = names(l)
      }
      
      errs
    }, simplify = F, USE.NAMES = T)
  }, simplify = F, USE.NAMES = T) ->
    re
  if(!miss_project_name && !miss_subject_code){
    if(length(re[[1]][[1]])){
      error = re[[1]][[1]]
    }else{
      error = FALSE
    }
    re = list(
      adapter = utils,
      error = error
    )
  }
  re
}



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
  
  tbl = utils::read.csv(file_path, stringsAsFactors = F, colClasses = 'character')
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
      dipsaus::deparse_svec(trial_number[is.na(time)])
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
    wb = dipsaus::deparse_svec(as.integer(wrong_range))
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


#' Check if data is loaded for current module
#' @param ... see details
#' @param data same as \code{...}, but can be a vector
#' @param .raise_error whether to raise error if data is missing
#' @details This function checks whether "ECoG" data is loaded. The format is: 
#' \code{"DATA+(blankspace)+TYPE"}. \code{"DATA"} can be "power" (wavelet 
#' transform amplitude), "phase" (complex angle), or "volt"/"voltage" (Before 
#' wavelet). \code{"TYPE"} can be "raw" (no reference), "referenced" 
#' (referenced by common average reference, white matter reference, or 
#' bipolar reference). For voltage data, there is one more special type 
#' "full" which loads voltage data for all electrodes.
#' @export
#' @name rave_checks
NULL
.rave_checks <- function(..., data = NULL, .raise_error = TRUE){
  data = unlist(c(data, list(...)))
  if(!length(data)){
    return()
  }
  rave_data = getDefaultDataRepository()
  module_tools = rave_data$module_tools
  preload_info = rave_data$preload_info
  subject = rave_data$subject
  n1 = nrow(module_tools$get_meta(name = 'trials'))
  n2 = length(preload_info$frequencies)
  n3 = length(preload_info$time_points)
  n4 = length(preload_info$electrodes)
  srate_wave = module_tools$get_sample_rate(original = F)
  srate_volt = module_tools$get_sample_rate(original = T)
  
  data = unlist(stringr::str_split(data, ','))
  data = stringr::str_to_lower(data)
  data = stringr::str_split(data, '\\ ')
  
  quos = NULL
  msg = NULL
  for(d in data){
    referenced = 'referenced' %in% d
    full = 'full' %in% d
    
    # 8 bytes is the default value. However, reference might not be cached, therefore in reference cases RAM size doubles. 8.25 takes into account for left-over objects
    base_size = ifelse(referenced, 16.5, 8.25)
    
    if('power' %in% d){
      dat = module_tools$get_power(force = F, referenced = referenced)
      if(is.null(dat)){
        quos = c(quos, rlang::quo({
          module_tools$get_power(referenced = !!referenced)
        }))
        size = dipsaus::to_ram_size(n1 * n2 * n3 * n4 * base_size)
        
        msg = c(msg, sprintf('Power (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
      }
      rm(dat)
    }else if('phase' %in% d){
      dat = module_tools$get_phase(force = F, referenced = referenced)
      if(is.null(dat)){
        quos = c(quos, rlang::quo({
          module_tools$get_phase(referenced = !!referenced)
        }))
        size = dipsaus::to_ram_size(n1 * n2 * n3 * n4 * base_size)
        msg = c(msg, sprintf('Phase (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
      }
      rm(dat)
    }else if('volt' %in% d || 'voltage' %in% d){
      if(full){
        data_env = getDefaultDataRepository()
        dat = data_env$.private[['volt_unblocked']]
        rm(data_env)
        if(is.null(dat)){
          quos = c(quos, rlang::quo({
            module_tools$get_voltage2()
          }))
          n_tp = nrow(subject$time_points) / srate_wave * srate_volt
          n_el = nrow(subject$electrodes)
          size = dipsaus::to_ram_size(n_el * n_tp * base_size)
          msg = c(msg, sprintf('Voltage (No epoch, %s)', size))
        }
      }else{
        dat = module_tools$get_voltage(force = F, referenced = referenced)
        if(is.null(dat)){
          quos = c(quos, rlang::quo({
            module_tools$get_voltage(referenced = !!referenced)
          }))
          size = dipsaus::to_ram_size(n1 * n3 * n4 * base_size / srate_wave * srate_volt)
          msg = c(msg, sprintf('Voltage (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
        }
      }
      rm(dat)
    }
  
  }
  
  if(length(quos)){
    # we have data pending to be loaded
    order = order(msg)
    msg = msg[order]
    quos = quos[order]
    
    if( .raise_error ){
      cat2('Data is not loaded: \n\t', paste(msg, collapse = '\n\t'), level = 'FATAL')
    }else{
      cat2('Data is not loaded: \n\t', paste(msg, collapse = '\n\t'), level = 'ERROR')
    }
  }
  
  list(
    msg = msg,
    quos = quos
  )
}

#' @rdname rave_checks
#' @export
rave_checks <- rave_context_generics('rave_checks', .rave_checks)
#' @export
rave_checks.default <- function(...){
  if(!any_subject_loaded()){
    stop('Please run rave_prepare(...) first.')
  }
  .rave_checks(...)
}
#' @export
rave_checks.rave_running_local <- function(...){}
#' @export
rave_checks.rave_module_debug <- function(...){
  rave_context()
  mount_demo_subject()
  .rave_checks(...)
  invisible()
}
#' @export
rave_checks.rave_running <- function(..., .raise_error = FALSE){
  ctx = rave_context()
  res = .rave_checks(..., .raise_error = FALSE)
  
  if(length(res$quos)){
    ctx$instance$internal_reactives$miss_data = TRUE
    ctx$instance$internal_reactives$miss_data_message = res$msg
    ctx$instance$internal_reactives$miss_data_comps = res$quos
    rave_failure('Needs to load data', level = 'INFO')
  }else{
    ctx$instance$internal_reactives$miss_data = FALSE
  }
}

