#' Check if default data environment has object
#' @param var variable name
#' @param any whether all variables should be present of any variables should 
#' exist
#' @param data_repo internally used
#' @return Logical \code{TRUE} or \code{FALSE} indicating the existence of the 
#' variables 
check_data_repo <- function(var = c('subject'), any = FALSE, 
                            data_repo = getDefaultDataRepository()){
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
    catgl = function(...){}
  }
  
  catgl('Checking: Project - ', project_name, ', Subject - ', subject_code)
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
        pre_hist = as.list(raveio::load_yaml(pre_yaml_file))
        log_data[['preprocess']] = pre_hist
      }
    }
    
    # update log.yaml
    save_log = T
    yaml_file = file.path(dirs$rave_dir, 'log.yaml')
    if(file.exists(yaml_file)){
      log_data_old = as.list(raveio::load_yaml(yaml_file))
      if(!is.null(log_data[['preprocess']])){
        # compare
        if(identical(log_data_old[['preprocess']], log_data[['preprocess']], num.eq = TRUE, ignore.environment = TRUE, ignore.bytecode = TRUE)){
          catgl('Cached log.yaml shares the same information with preprocess log file. No need to re-cache')
          save_log = FALSE
        }else{
          log_data_old[['preprocess']] = log_data[['preprocess']]
        }
        log_data = log_data_old
      }
    }
    
    # save to log.yaml
    if(save_log){
      catgl('Creating/replacing log.yaml...')
      raveio::save_yaml(log_data, yaml_file, fileEncoding = 'utf-8')
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

#' @title Complete validity check a RAVE subject
#' @param subject character, must be \code{"project/subject"} format
#' @param stop_on_error logical, whether stop when error occurs
#' @return None
#' @export
check_subject <- function(subject, stop_on_error = FALSE){
  stopifnot2(is.character(subject), msg = '`subject` MUST be characters')
  rave_setup_workers()
  
  raise <- function(..., level = 'WARNING'){
    if(stop_on_error){ stop(gl(...)) }
    catgl(gl(..., .envir = parent.frame()), level = level)
  }
  msg <- function(..., level = 'INFO'){
    catgl(gl(..., .envir = parent.frame()), level = level)
  }
  
  
  tmp = strsplit(subject, '/')[[1]]
  project_name = tmp[1]
  subject_code = tmp[2]
  data_dir = normalizePath(rave_options('data_dir'))
  raw_dir = normalizePath(rave_options('raw_data_dir'))
  msg('----------------',
        ' Project [{project_name}] - Subject [{subject_code}] ',
        '----------------')
  # ------------------ 1. Check file structure ------------------------
  msg('1. Check folder existence', level='DEFAULT')
  # raw folder
  raw_subdir = file.path(raw_dir, subject_code)
  if(dir.exists(raw_subdir)){
    msg('Check raw file directory - Exist')
  } else{
    raise('Raw folder is missing - {raw_subdir}\n',
          '-     If you have imported subject, create an empty',
          ' folder will fix.')
  }
  
  # data folder
  sub_dir = file.path(data_dir, project_name, subject_code)
  if(dir.exists(sub_dir)){
    msg('Check RAVE file directory - Exist')
  } else{
    raise('RAVE folder is missing - {sub_dir}')
  }
  
  # ------ Check subject data folder - sub folders ------
  res = check_subjects2(project_name = project_name, 
                  subject_code = subject_code, quiet = TRUE)
  if(!res$check$power_dir){
    raise('Power directory missing - ',
             '{file.path(sub_dir, "rave", "data", "power")}')
  }
  if(!res$check$phase_dir){
    raise('Phase directory missing - ',
             '{file.path(sub_dir, "rave", "data", "phase")}')
  }
  if(!res$check$volt_dir){
    raise('Voltage directory missing - ',
             '{file.path(sub_dir, "rave", "data", "voltage")}')
  }
  
  
  msg('2. Check preprocess steps', level='DEFAULT')
  if( !res$check$started_preprocess ){
    raise('Preprocess not started!')
  }
  if( !res$check$notch_filter ){
    raise('No Notch filters applied')
  }
  if( !res$check$wavelet ){
    raise('No power-frequency decomposition applied (wavelet/hilbert/multitaper)')
  }
  if( !res$check$reference ){
    raise('You might want to reference electrodes?')
  }
  if( !res$check$meta_dir ){
    raise('No meta folder detected - {file.path(sub_dir, "rave", "meta")}', 
          level = 'ERROR')
  }
  
  if( !res$check$meta_electrode ){
    raise('No {sQuote("electrodes.csv")} detected\n',
          '-     Please provide electrode label & coordinate file\n',
          '-     {file.path(sub_dir, "rave", "meta", "electrodes.csv")}\n',
          '-     Run preprocess (wavelet) should fix',
          level='ERROR')
  }
  if( !res$check$meta_time ){
    raise('No {sQuote("time_points.csv")} detected - Run preprocess (wavelet) should fix',
          level = 'ERROR')
  }
  if( !res$check$meta_epoch ){
    raise('No epoch {sQuote("epoch_***.csv")} detected\n',
          '-     Please provide at least one epoch file, for example:\n',
          '-     {file.path(sub_dir, "rave", "meta", "epoch_default.csv")}',
          level = 'ERROR')
  }
  if( !res$check$meta_reference ){
    raise('No reference {sQuote("reference_***.csv")} detected\n',
          '-     Please provide at least one reference file, for example:\n',
          '-     {file.path(sub_dir, "rave", "meta", "reference_default.csv")}\n',
          '-     Run preprocess (wavelet) should fix this problem',
          level = 'ERROR')
  }
  
  blocks = res$log$preprocess$blocks
  channels = res$log$preprocess$channels
  volt_srate = res$log$preprocess$srate
  
  if( !length(blocks) ){
    raise('No block/session found! (FATAL ERROR)', level = 'FATAL')
  }
  if( !length(channels) ){
    raise('No channels/electrode found! (FATAL ERROR)', level = 'FATAL')
  }
  if( length(volt_srate) != 1 || volt_srate <= 1 ){
    raise('iEEG/ECoG data has no sample rate detected', level = 'FATAL')
  }
  
  msg('Blocks - {paste(blocks, collapse = ", ")}')
  msg('Electrodes - {dipsaus::deparse_svec(channels)}')
  msg('Original sample rate - {volt_srate} Hz')
  
  if( volt_srate < 50 ){
    raise('Analogtraces has sample rate lower than 50Hz?')
  }
  
  # ------------------ Check raw folder ------------------
  msg('3. Check raw files', level='DEFAULT')
  # raw files
  
  if( dir.exists(raw_subdir) && length(list.dirs(raw_subdir, recursive = FALSE)) > 0){
    if(all(file.exists(file.path(raw_subdir, blocks)))){
      pt = 'ch([0-9]+)\\.[mM][aA][Tt]$'
      mis = sapply(blocks, function(b){
        fs = list.files(file.path(raw_subdir, b), pattern = pt)
        if(length(fs)){
          fs = stringr::str_match(fs, pt)[,2]
          fs = as.integer(fs)
          return(setdiff(channels, fs))
        }
        return(NULL)
      }, simplify = FALSE, USE.NAMES = TRUE)
      mis = dipsaus::drop_nulls(mis)
      if(length(mis)){
        mis = sapply(names(mis), function(nm){
          sprintf('-     Block %s has missing file(s) - electrode %s',
                  nm, dipsaus::deparse_svec(mis[[nm]]))
        })
        raise('Electrode files are missing\n', paste(mis, collapse = '\n'))
      }
    }else{
      raise('Raw block folders are missing. Check existence of folders:\n',
            paste('-     ', file.path(file.path(raw_subdir, blocks)), collapse = '\n'))
    }
  }else{
    msg('-     skipped because raw folder is empty or format is non-standard', level = 'DEFAULT')
  }
  
  # ------------------ preprocess/voltage ------------------------
  msg('4. Validate - rave/preprocess/voltage/...', level='DEFAULT')
  dirs = get_dir(subject_code = subject_code, project_name = project_name)
  pre_dir = dirs$preprocess_dir
  
  pre_elecs = file.path(pre_dir, 'voltage', sprintf('electrode_%d.h5', channels))
  fe = file.exists(pre_elecs)
  if(!all(fe)){
    mis = dipsaus::deparse_svec(channels[!fe])
    raise('Electrode(s) {mis} not properly imported (FATAL ERROR)', level = 'FATAL')
  }
  
  validity = dipsaus::lapply_async2(
    channels, function(e){
    pre_elec = file.path(pre_dir, 'voltage', sprintf('electrode_%d.h5', e))
    
    validity = sapply(blocks, function(b){
      # -1 : error loading 
      # 0: missing ot length not match
      raw_len = tryCatch({
        d = load_h5(pre_elec, name = sprintf('/raw/%s', b), ram = FALSE)
        length(d)
      }, error = function(e){-1})
      
      if( raw_len <= 0 ){
        return(raw_len)
      }
      
      if(res$check$notch_filter){
        notch_len = tryCatch({
          d = load_h5(pre_elec, name = sprintf('/notch/%s', b), ram = FALSE)
          length(d)
        }, error = function(e){-1})
      }else{
        notch_len = raw_len
      }
      if( raw_len != notch_len ){
        return(0)
      }
      return(raw_len)
      
    })
    
    return(validity)
    
  }, 
    callback = function(e){sprintf('Checking electrode %d', e)},
    plan = FALSE)
  
  validity = do.call(rbind, validity)
  
  # check corrupted file - raw preprocess
  corrupted = rowSums(validity < 0) > 0
  unequallen = rowSums(validity == 0) > 0
  
  if(any(corrupted)){
    raise("Preprocess files corrupted at {file.path(pre_dir, 'voltage', 'electrode_xxx.h5')}:",
          '-     Electrodes are {dipsaus::deparse_svec(channels[corrupted])}')
  }else if(any(unequallen)){
    raise("Notch filter produces different length than original signals\n',
          '-     Check electrode ({dipsaus::deparse_svec(channels[corrupted])}) at\n',
          '-     {file.path(pre_dir, 'voltage', 'electrode_xxx.h5')}\n",
          '-     Details: HDF5 dataset /raw/block has different lengths than /notch/block')
  }else{
    msg('Subject data imported and passed initial validity check')
  }
  
  
  # ------------------ data/voltage files ------------------
  
  signal_length = apply(validity, 2, max)
  
  if(any(signal_length <= 0)){
    raise('Invalid analogtrace(s) found', level = 'FATAL')
  }
  
  msg('5. Validate - rave/data/voltage/...', level='DEFAULT')
  volt_dir = file.path(dirs$channel_dir, 'voltage')
  if(dir.exists(volt_dir)){
    # voltage directory
    
    fs = file.path(volt_dir, sprintf('%d.h5', channels))
    fe = file.exists(fs)
    if(!all(fe)){
      mis = dipsaus::deparse_svec(channels[!fe])
      raise('Missing electrode(s) {mis}. Please check folder rave/data/voltage/')
    } else{
      # check validity
      
      validity = do.call('rbind', dipsaus::lapply_async2(channels, function(e){
        
        f = file.path(volt_dir, sprintf('%d.h5', e))
        
        if(!file.exists(f)){
          return(c(FALSE, FALSE, FALSE))
        }
        
        # check length
        raw_lens = sapply(blocks, function(b){
          l = 0
          l = try({
            raw = load_h5(f, sprintf('/raw/voltage/%s', b), ram = FALSE)
            length(raw)
          }, silent = TRUE)
          l
        })
        ref_lens = sapply(blocks, function(b){
          l = 0
          l = try({
            raw = load_h5(f, sprintf('/ref/voltage/%s', b), ram = FALSE)
            length(raw)
          }, silent = TRUE)
          l
        })
        
        
        c(all(raw_lens > 0), all(raw_lens == signal_length), 
          all(ref_lens == signal_length))
      }, 
      callback = function(e){sprintf('Checking electrode %d', e)},plan = FALSE))
      
      if(!all(validity[,1])){
        raise('Possible corrupted files found in folder\n',
              '-     {volt_dir}\n',
              '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,1]])}')
      } else if(!all(validity[,2])){
        raise('Voltage data (raw) lengths not match with preprocess in folder\n',
              '-     {volt_dir}\n',
              '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,2]])}')
      } else if (!all(validity[,3])){
        raise('Voltage data (ref) lengths not match with (raw) in folder\n',
              '-     {volt_dir}\n',
              '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,3]])}')
      } else{
        msg('Subject has valid voltage data')
      }
      
    }
    
  }
  
  # ------------------ data/power, phase files ------------------
  # check wavelet files
  if(res$check$wavelet){
    wave_log = res$log$preprocess$wavelet_log
    wave_log = wave_log[[length(wave_log)]]
    
    n_freq = length(wave_log$frequencies)
    wave_srate = wave_log$target_srate
    
    expected_length = floor(signal_length / volt_srate * wave_srate) + 1
    
    
    for(dtype in c('power', 'phase')){
      
      dpath = file.path(dirs$rave_dir, 'data', dtype)
      
      fs = file.path(dpath, sprintf('%d.h5', channels))
      fe = file.exists(fs)
      if(!all(fe)){
        mis = dipsaus::deparse_svec(channels[!fe])
        raise('Missing electrode(s) {mis}. Please check folder rave/data/{dtype}/')
      } else{
        # check validity
        
        validity = do.call('rbind', dipsaus::lapply_async2(channels, function(e){
          f = file.path(dpath, sprintf('%d.h5', e))
          
          if(!file.exists(f)){
            return(c(FALSE, FALSE, FALSE))
          }
          
          # check length
          raw_lens = sapply(blocks, function(b){
            l = 0
            l = try({
              raw = load_h5(f, sprintf('/raw/%s/%s', dtype, b), ram = FALSE)
              raw = dim(raw)
              if(raw[1] != n_freq){return(0)}
              raw[2]
            }, silent = TRUE)
            l
          })
          ref_lens = sapply(blocks, function(b){
            l = 0
            l = try({
              raw = load_h5(f, sprintf('/ref/%s/%s', dtype, b), ram = FALSE)
              raw = dim(raw)
              if(raw[1] != n_freq){return(0)}
              raw[2]
            }, silent = TRUE)
            l
          })
          
          tryCatch({
            c(all(raw_lens > 0), all(abs(raw_lens - expected_length) < 10), 
              all(ref_lens == raw_lens))
          }, error = function(e){
            c(FALSE,FALSE,FALSE)
          })
        }, 
        callback = function(e){sprintf('Checking electrode %d', e)},plan = FALSE))
        
        if(!all(validity[,1])){
          raise('Possible corrupted files found in folder\n',
                '-     {dpath}\n',
                '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,1]])}')
        } else if(!all(validity[,2])){
          raise('Voltage data (raw) lengths not match with preprocess in folder\n',
                '-     {dpath}\n',
                '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,2]])}')
        } else if (!all(validity[,3])){
          raise('Voltage data (ref) lengths not match with (raw) in folder\n',
                '-     {dpath}\n',
                '-     Electrodes: {dipsaus::deparse_svec(channels[!validity[,3]])}')
        } else{
          msg('Subject has valid {dtype} data')
        }
        
      }
      
      
      
    }
    
  } else{
    fs = list.files(file.path(dirs$cache_dir, c('power', 'phase')), 
                    pattern = '\\.h5$', all.files = TRUE, full.names = TRUE, recursive = TRUE)
    if(length(fs)){
      validity = unlist(dipsaus::lapply_async2(fs, function(f){
        raveio::h5_valid(f, 'r', close_all = TRUE)
      }, plan = FALSE))
      if(!all(validity)){
        mis = paste(fs[!validity], collapse = '\n')
        raise('The following HDF5 files might be broken:\n', mis)
      }
    }else{
      msg('Skipped power & phase data checks because no data found', level = 'DEBUG')
    }
  }
  # ------------------ reference content ------------------
  msg('7 Validate - rave/data/reference', level = 'DEFAULT)')
  ref_dir = file.path(dirs$cache_dir, 'reference')
  refs = list.files(ref_dir, '\\.h5$')
  if(length(refs)){
    lapply(refs, function(ref){
      f = file.path(ref_dir, ref)
      # p = hdf5r::H5File$new(f, mode = 'r')
      
      vlen = sapply(blocks, function(b){
        tryCatch({
          length(load_h5(f, sprintf('/voltage/%s', b), ram = FALSE))
        }, error = function(e){
          0
        })
      })
      
      if(!all(vlen == signal_length)){
        raise('Reference file rave/data/reference/{f} has invalid data length')
        return()
      }
      
      if(res$check$wavelet){
        vlen = sapply(blocks, function(b){
          tryCatch({
            dim = dim(load_h5(f, sprintf('/wavelet/coef/%s', b), ram = FALSE))
            stopifnot(dim[[1]] == n_freq, dim[3] == 2)
            dim[[2]]
          }, error = function(e){
            0
          })
        })
        
        if(!all(abs(vlen - expected_length) < 10)){
          raise('Reference file rave/data/reference/{f} has invalid data length')
          return()
        }
      }
    })
  }
  
  
  # ------------------ epoch files ------------------
  msg('8. Validate epoch files - rave/meta/epoch_***.csv', level = 'DEFAULT)')
  # epoch
  if(res$check$meta_epoch){
    lapply(res$epochs, function(epoch){
      f = sprintf('epoch_%s.csv', epoch)
      dat = read.csv(file.path(dirs$meta_dir, f), colClasses = 'character')
      # 1. check required names
      headers = c('Block', 'Time', 'Trial', 'Condition')
      mis = !headers %in% names(dat)
      pass = TRUE
      if(any(mis)){
        raise('Epoch {sQuote(f)} - misses headers: {paste(sQuote(headers[mis]), collapse=",")}')
        pass = FALSE
      } else{
        # check if blocks are valid
        if(!setequal(unique(dat$Block), blocks)){
          raise('Mis-matches found in {sQuote(f)} in column {sQuote("Block")}\n',
                '-     Blocks registered: {paste(sQuote(blocks), collapse=",")}\n',
                '-     Blocks found in epoch: {paste(sQuote(unique(dat$Block)), collapse=",")}')
          pass = FALSE
        }
        
        # check if time is valid
        time_cap = signal_length / volt_srate
        time = as.numeric(dat$Time)
        if(any(is.na(time))){
          mis = dipsaus::deparse_svec(which(is.na(time)))
          raise('Epoch {sQuote(f)} - NA found in column {sQuote("Time")}, row {mis}. Please remove.')
          pass = FALSE
        }
        
        time[is.na(time)] = 1
        
        validity = unlist(sapply(seq_along(blocks), function(ii){
          b = blocks[ii]
          sel = dat$Block == b
          validity = time < time_cap[ii] & time > 0 & sel
          which(sel & !validity)
        }))
        
        if(length(validity)){
          mis = dipsaus::deparse_svec(validity)
          raise('Epoch {sQuote(f)}: Invalid onset time found at row {mis}')
          pass = FALSE
        }
        
      }
      
      if(pass){
        msg('Epoch {sQuote(f)} - passed')
      }
    })
  }else{
    msg('Skipped because no epoch files found', level = 'DEBUG')
  }
  
  
  # ------------------ electrodes files ------------------
  msg('9. Validate - rave/meta/electrodes.csv', level = 'DEFAULT)')
  electrodes = load_meta('electrodes', project_name, subject_code)
  msg('Skipped - (Validation not yet implemented)', level = 'DEBUG')
  
  # ------------------ reference files ------------------
  msg('10. Validate - rave/meta/reference_***.csv', level = 'DEFAULT)')
  if(res$check$meta_reference){
    lapply(res$references, function(ref){
      f = sprintf('reference_%s.csv', ref)
      dat = read.csv(file.path(dirs$meta_dir, f), stringsAsFactors = FALSE)
      # 1. check required names
      headers = c('Electrode', 'Group', 'Reference', 'Type')
      mis = !headers %in% names(dat)
      pass = TRUE
      if(any(mis)){
        raise('Reference {sQuote(f)} - misses headers: {paste(sQuote(headers[mis]), collapse=",")}')
        pass = FALSE
      } else{
        # check if electrodes are valid
        if(!setequal(dat$Electrode, electrodes$Electrode)){
          raise('Reference {sQuote(f)}: electrodes not match with {sQuote("electroddes.csv")}')
          pass = FALSE
        }
        # check reference existence
        refs <- dat$Reference
        refs = unique(refs[refs != 'noref'])
        refs = stringr::str_match(refs, '^ref_([0-9,\\-]+)$')[,2]
        
        ref1 = sapply(refs, function(x){length(dipsaus::parse_svec(x)) > 1})
        ref2 = refs[!ref1]
        ref1 = refs[ref1]
        
        if(length(ref1)){
          # check ref folder
          fs = sprintf('ref_%s.h5', ref1)
          fe = file.exists(file.path(dirs$reference_dir, fs))
          if(!all(fe)){
            raise('Reference {sQuote(f)}: file(s) claimed but miss from rave/data/reference/\n',
                  paste('-     ', fs[!fe], collapse = '\n'))
            pass = FALSE
          }
        }
        if(length(ref2)){
          fs = sprintf('%s.h5', ref2)
          fe = sapply(ref2, function(e){
            b = blocks[1]
            # check existence
            tryCatch({
              for(dtype in c('voltage', 'power', 'phase')){
                dat = load_fst_or_h5(
                  fst_path = file.path(dirs$cache_dir, 'cache', dtype, 'raw', b, 
                                       sprintf('%s.fst', e)),
                  h5_path = file.path(dirs$cache_dir, dtype, sprintf('%s.h5', e)), 
                  h5_name = sprintf('/raw/%s/%s', dtype, b), ram = FALSE
                )
              }
              TRUE
            }, error = function(e){
              FALSE
            })
            
          })
          
          if(!all(fe)){
            raise('Reference {sQuote(f)}: Electrode required but missing or broken\n',
                  paste('-     ', fs[!fe], collapse = '\n'))
            pass = FALSE
          }
        }
        
      }
      if(pass){
        msg('Reference {sQuote(f)} - passed')
      }
    })
    
  }else{
    msg('Skipped because no reference files found', level = 'DEBUG')
  }
  
  # ------------------ Redundancy check ------------------
  msg('11. Redundancy check - rave/data/cache/', level = 'DEFAULT)')
  cache_dir = file.path(dirs$cache_dir, 'cache')
  if(!dir.exists(cache_dir)){
    msg('Skipped - No cache found', level = 'DEBUG')
  }else{
    # test whether we can open fst files, do not check dimension
    fs = list.files(cache_dir, pattern = '\\.fst$', 
                    full.names = FALSE, recursive = TRUE, all.files = TRUE)
    if(length(fs)){
      fe = unlist(dipsaus::lapply_async2(fs, function(f){
        tryCatch({
          LazyFST$new(file_path = file.path(cache_dir, f), transpose = FALSE)
          TRUE
        }, error = function(e){
          FALSE
        })
      }, plan = FALSE, callback = function(f){
        'checking...'
      }))
      
      if(!all(fe)){
        mis = fs[!fe]
        if(length(mis) > 10){
          mis = c(mis[1:5], '...', mis[length(mis) - c(4:1)])
        }
        mis = paste('-     ', mis, collapse = '\n')
        raise('Cannot open cache files\n', mis)
      } else{
        msg('Cached data files are valid')
      }
    }
    
    cache_ref = file.path(cache_dir, 'cached_reference.csv')
    if(file.exists(cache_ref)){
      cache_ref = read.csv(cache_ref)
      validity = dipsaus::lapply_async2(seq_len(nrow(cache_ref)), function(ii){
        row = cache_ref[ii,]
        e = row$Electrode
        ref = row$Reference
        
        # 1. check whether reference match with H5 files
        for(dtype in c('voltage', 'power', 'phase')){
          validity = tryCatch({
            f = file.path(dirs$cache_dir, dtype, sprintf('%d.h5', e))
            ref_orig = load_h5(f, 'reference', ram = TRUE)
            if(ref_orig != ref){
              FALSE
            }
            TRUE
          }, error = function(e){
            FALSE
          })
          
          if(!validity){
            return(FALSE)
          }
        }
        return(TRUE)
        
      }, plan = FALSE, callback = function(ii){
        'Checking cached references..'
      })
      
      validity = unlist(validity)
      if(!all(validity)){
        mis = cache_ref$Electrode[!validity]
        mis = dipsaus::deparse_svec(mis)
        raise('Cached references mismatch with data files: Electrode(s) ', mis)
      }else{
        msg('Cached references match with data files')
      }
    }
    
    
  }
  
  # ------------------ end ------------------
  msg('Done')
  invisible()
  
}


#' check subject validity tools (use check_subjects2)
#' @param project_name project_name
#' @param subject_code subject_code
#' @param check check is internally used
#' @param folders folders to check
#' @param preprocess preprocess to check
#' @param Meta Meta to check
check_subjects_old <- function(
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
    }, simplify = FALSE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE) ->
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
  # subject2 <- raveio::as_rave_subject(subject$id, strict = FALSE)
  
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
#' @param rave_data internally used
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
.rave_checks <- function(..., data = NULL, .raise_error = TRUE,
                         rave_data = getDefaultDataRepository()){
  data = unlist(c(data, list(...)))
  if(!length(data)){
    return()
  }
  module_tools = rave_data$module_tools
  preload_info = rave_data$preload_info
  subject = rave_data$subject
  n1 = nrow(module_tools$get_meta(name = 'trials'))
  n2 = length(preload_info$frequencies)
  n3 = length(preload_info$time_points)
  n4 = length(preload_info$electrodes)
  srate_wave = module_tools$get_sample_rate(original = FALSE)
  srate_volt = module_tools$get_sample_rate(original = TRUE)
  
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
        dat = rave_data$.private[['volt_unblocked']]
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
      catgl('Data is not loaded: \n\t', paste(msg, collapse = '\n\t'), level = 'FATAL')
    }else{
      catgl('Data is not loaded: \n\t', paste(msg, collapse = '\n\t'), level = 'ERROR')
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
  warning('function rave_checks is to be depricated. Please see rave_validate for details.')
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


