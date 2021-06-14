#' Import Subject "Matlab" File and Create "HDF5" files
#' @param project_name project name
#' @param subject_code subject code
#' @param blocks blocks to be imported
#' @param electrodes to be imported (please check file existence before calling this function)
#' @param ... ignored
cache_raw_voltage <- function(project_name, subject_code, blocks, electrodes, ...){
  args = list(...)
  dirs = get_dir(subject_code = subject_code, project_name = project_name,
                 mkdirs = c('preprocess_dir'))
  # cfile = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
  # if(file.exists(cfile)){
  #   unlink(cfile)
  # }
  
  
  progress = progress('Reading raw voltage signals from MATLAB...', max = length(electrodes)+1 )
  on.exit({progress$close()})
  
  lapply_async(electrodes, function(e){
    cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e))
    if(file.exists(cfile)){
      unlink(cfile)
    }
    
    sapply(blocks, function(block_num){
      s = pre_import_matlab(subject_code, project_name, block_num, e)
      # save s to cfile - /raw/block_num
      save_h5(as.vector(s), cfile, name = sprintf('/raw/%s', block_num), chunk = 1024, replace = TRUE)
      NULL
    }, simplify = FALSE, USE.NAMES = TRUE) ->
      re
    return(re)
  }, .call_back = function(i){
    e = electrodes[i]
    progress$inc(sprintf('Electrode - %d', e))
  }) ->
    re
  
  invisible()
}


#' @title Import Raw Signal from Non-standard Formats
#' @param subject_code subject code to search for in the raw folder
#' @param project_name project name to create
#' @param launch_preprocess whether to launch preprocess app, default is true
#' @export
rave_import_rawdata <- function(subject_code, project_name, launch_preprocess = TRUE){
  subject = subject_code
  
  utils = rave_preprocess_tools()
  utils$create_subject(subject_code = subject, project_name = project_name)
  if(utils$has_raw_cache()){
    catgl('Subject already imported. If you want to re-import the data, please remove subject folder first',
         level = 'ERROR')
    return(invisible())
  }
  srate = utils$get_sample_rate()
  srate = dipsaus::ask_or_default('Please enter the sampling rate in Hz.\n', default = as.character(srate))
  srate = as.numeric(srate)
  if(is.na(srate) || srate < 0){
    catgl('Invalid sample rate, setting it to ', sQuote('0'), 
         '\n   Please enter it in rave pre-process module', level = 'WARNING')
    srate = 0
  }
  if( srate > 0 ){
    utils$set_sample_rate(srate)
  }
  
  
  raw_dir = rave_options('raw_data_dir')
  
  subject_dir = normalizePath(file.path(raw_dir, subject), mustWork = TRUE)
  
  blocks = list.dirs(subject_dir, full.names = FALSE, recursive = FALSE)
  
  default_pal = list('DEFAULT' = 'grey60')
  
  nblocks = length(blocks)
  if(nblocks == 0){
    catgl(sprintf('No folder (block) found under directory - %s', subject_dir), level = 'ERROR')
    return(invisible())
  }
  catgl(sprintf('%d folders (blocks) found in raw directory - %s:', length(blocks), subject_dir), level = 'DEFAULT', pal = default_pal)
  
  # Check file structures within blocks
  fs = sapply(seq_along(blocks), function(ii){
    b = blocks[ii]
    files = list.files(file.path(subject_dir, b))
    nfiles = length(files)
    catgl(sprintf('  [%d] %s (%d files)', ii, b, nfiles), level = 'DEFAULT', pal = default_pal)
    files
  })
  
  
  choice = dipsaus::ask_or_default(sprintf('Select blocks 1%s:\n ', ifelse(nblocks == 1, '', paste0('-', nblocks))), 
                          default = dipsaus::deparse_svec(seq_len(nblocks)))
  choice = dipsaus::parse_svec(choice, sort = TRUE)
  choice = choice[choice %in% seq_len(nblocks)]
  nchoices = length(choice)
  if(nchoices == 0){
    catgl('Cannot find the blocks entered', level = 'ERROR')
    return(invisible())
  }
  
  sel_blocks = blocks[choice]
  sel_files = fs[choice]
  
  ptypes = lapply(choice, function(ii){
    block = blocks[[ii]]
    files = fs[[ii]]
    files = stringr::str_to_lower(files)
    potential_types = c() # 1: mat file/elect 2.whole file (mat) 3. text file
    
    if(any(stringr::str_detect(files, '([0-9]+)\\.mat$'))){
      potential_types = c(potential_types, 1)
    }
    if(any(stringr::str_detect(files, '[^0-9]\\.(mat|h5)$'))){
      potential_types = c(potential_types, 2)
    }
    if(any(stringr::str_detect(files, '\\.(txt|csv)$'))){
      potential_types = c(potential_types, 3)
    }
    potential_types
  })
  
  ptypes = unlist(ptypes)
  
  ptype = 0
  if( sum(ptypes == 1) == nchoices ){
    
    # now look for electrodes
    elecs = lapply(choice, function(ii){
      files = fs[[ii]]
      re = stringr::str_match(files, '([0-9]+)\\.mat$')[,2]
      re = re[!is.na(re)]
      as.integer(re)
    })
    elecs = table(unlist(elecs))
    elecs = as.integer(names(elecs)[elecs == nchoices])
    etxt = dipsaus::deparse_svec(elecs)
    
    
    ans = dipsaus::ask_yesno(sprintf('Found %d files in end with - [number].mat. ', length(elecs)),
                'Is the data stored one file Matlab per electrode?')
    if( ans ){
      ptype = 1 
      ans = dipsaus::ask_or_default('Enter the electrode/channel numbers.', default = etxt)
      ans = dipsaus::parse_svec(ans)
      sel = ans %in% elecs
      if( !all(sel) ){
        catgl(sprintf('Not all electrodes are found... Electrode(s) %s missing', dipsaus::deparse_svec(ans[!sel])), level = 'WARNING')
      }
      sel_elec = ans[sel]
      if( !length(sel_elec) ){
        catgl('No electrode is selected!', level = 'ERROR')
        return(invisible())
      }
      ans = dipsaus::ask_yesno(sprintf('Import electrodes %s from block %s?', dipsaus::deparse_svec(sel_elec), paste(sel_blocks, collapse = ', ')))
      
      if(!ans){ return(invisible()) }
      
      # Import electrodes and blocks
      utils$set_blocks(blocks = sel_blocks)
      utils$set_electrodes(electrodes = sel_elec)
      utils$collect_raw_voltage()
      if(launch_preprocess){
        return(rave_preprocess(project_name = project_name, subject_code = subject_code))
      }else{
        catgl('Loaded. Please launch preprocess:\n    rave::rave_preprocess()', level = 'INFO')
        return(ptype)
      }
      
    }
    
  }
  if( ptype == 0 && (sum(ptypes == 2) == nchoices)){
    ans = dipsaus::ask_yesno('Found files in end with - mat/h5. Are all the data stored in one Matlab/HDF5 file for each block?')
    if( ans ){
      ptype = 2
      # print all the files
      files = lapply(choice, function(ii){
        files = fs[[ii]]
        re = stringr::str_match(files, '^(.*[^0-9]\\.(mat|h5))$')[,2]
        re = re[!is.na(re)]
        re
      })
      
      
      # filenames, must be length >= 1 as otherwise we wouldn't enter the clause
      files = unlist(files)
      opt = unique(files)
      
      ans = dipsaus::ask_or_default('The following txt files were found in the first block. Which file contains the raw analog traces? (This file name MUST be used by all blocks)\n\t',
                                    paste(opt, collapse = ', '), '\n', default = opt[[1]])
      
      if( sum(files == ans) != nchoices ){
        catgl(sprintf('Cannot find file %s in all blocks', sQuote(ans)), level = 'ERROR')
        return(invisible())
      }
      
      # check the file
      mat_file = ans
      
      mat_paths = file.path(subject_dir, sel_blocks, mat_file)
      
      catgl('Trying to read from ', mat_paths[1], level = 'DEFAULT', pal = default_pal)
      
      sample = read_mat(mat_paths[1])
      dnames = names(sample)
      if(length(dnames) > 1){
        # check which name 
        for(nm in dnames){
          if(is.numeric(sample[[dnames]]) && length(dim(sample[[dnames]])) == 2){
            break()
          }
        }
        nm = dipsaus::ask_or_default('More than one data set are found in file ', mat_file, '\n',
                            '  Which one stores the data? \n\t', paste('    * ', dnames, '\n', collapse = ''), '\n',
                            default = nm)
      }else{
        nm = dnames
      }
      
      dims = dim(sample[[nm]])
      ans = dipsaus::ask_yesno('Dataset ', nm, ' is a ', sprintf('%dx%d', dims[[1]], dims[[2]]), ' array. Is ',
                  sQuote(max(dims)), ' the number of time points?\n')
      if(ans){ 
        n_elecs = min(dims) 
      }else{
        n_elecs = max(dims) 
      }
      ans = dipsaus::ask_or_default('Please confirm the electrode numbers. Must have length of ', n_elecs,
                           '\n', default = sprintf('1-%d', n_elecs))
      sel_elec = dipsaus::parse_svec(ans)
      
      if(length(sel_elec) != n_elecs){
        sel_elec = seq_len(n_elecs)
        catgl('Length of electrodes does not match with the data dimensions. Ignore the user input ', 
             ans, ' and reset to ', dipsaus::deparse_svec(sel_elec), level = 'WARNING')
      }
      
      if(!dipsaus::ask_yesno('Ready to import?')){
        return(invisible())
      }
      
      # Import electrodes and blocks
      utils$set_blocks(blocks = sel_blocks)
      utils$set_electrodes(electrodes = sel_elec)
      
      dirs = get_dir(subject_code = subject, project_name = project_name, mkdirs = c('preprocess_dir'))
      # file = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
      
      elec_dim = which(dims == n_elecs)
      
      # load raw data
      lapply(sel_blocks, function(block){
        mat_path = file.path(subject_dir, block, mat_file)
        src = read_mat(mat_path)
        src = src[[nm]]
        if(elec_dim == 2){ src = t(src) }
        
        lapply(seq_along(sel_elec), function(ii){
          e = sel_elec[ii]
          cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e))
          # if(file.exists(cfile)){ unlink(cfile) }
          
          save_h5(as.vector(src[ii,]), cfile, name = sprintf('/raw/%s', block), chunk = 1024, replace = TRUE)
          NULL
        })
        NULL
      })
      # set subject meta data, flag it as cached
      utils$save_to_subject(checklevel = 1)
      if(launch_preprocess){
        return(rave_preprocess())
      }else{
        catgl('Loaded. Please launch preprocess:\n    rave::rave_preprocess()', level = 'INFO')
        return(ptype)
      }
    }
  }
  if( ptype == 0 && (sum(ptypes == 3) == nchoices)){
    ans = dipsaus::ask_yesno('Found txt/csv files in the directory. Is data stored in a giant file?')
    if( ans ){
      ptype = 3
      # print all the files
      files = lapply(choice, function(ii){
        files = fs[[ii]]
        re = stringr::str_match(files, '^.*\\.(csv|txt)$')[,1]
        re = re[!is.na(re)]
        re
      })
      
      # filenames, must be length >= 1 as otherwise we wouldn't enter the clause
      files = unlist(files)
      opt = unique(files)
      
      ans = dipsaus::ask_or_default('The following txt files were found in the first block. Which file contains the raw analog traces? (This file name MUST be used by all blocks)\n\t',
                              paste(opt, collapse = ', '), '\n', default = opt[[1]])
      
      if( sum(files == ans) != nchoices ){
        catgl(sprintf('Cannot find file %s in all blocks', sQuote(ans)), level = 'ERROR')
        return(invisible())
      }
      
      
      # check the file
      plain_file = ans
      
      txt_paths = file.path(subject_dir, sel_blocks, plain_file)
      
      catgl('Trying to read from ', txt_paths[1], level = 'DEFAULT', pal = default_pal)
      
      sample = read_csv_no_header(txt_paths[1])
      ncols = ncol(sample)
      
      catgl('-------------------- Print the first 3 rows --------------------', level = 'DEFAULT', pal = default_pal)
      print(utils::head(sample, 3))
      catgl('-------------------- Print the last 3 rows --------------------', level = 'DEFAULT', pal = default_pal)
      print(utils::tail(sample, 3))
      catgl('-------------------- Print headers --------------------', level = 'DEFAULT', pal = default_pal)
      print(matrix(names(sample), nrow = 1), quote = FALSE)
      
      ans = dipsaus::ask_or_default('Dataset has ', ncols, ' columns. Please enter the column indices to import',
                           '\n  For example, ', sQuote('1-3,6,7,10'), ' will import columns 1, 2, 3, 6, 7, 10.\n',
                           default = dipsaus::deparse_svec(seq_len(ncols)))
      ans = dipsaus::parse_svec(ans)
      sel = ans %in% seq_len(ncols)
      if(!all(sel)){ 
        catgl('You entered column numbers that do not exist, column(s) ', dipsaus::deparse_svec(ans[!sel]),
             ' Reset to all columns', level = 'WARNING')
      }else{
        ans = ans[sel]
      }
      
      if(!dipsaus::ask_yesno('Ready to import column(s) ', dipsaus::deparse_svec(ans), ' as electrodes? All N/A values will be replaced by 0')){
        return(invisible())
      }
      sel_elec = ans
      rm(sample)
      
      # Import electrodes and blocks
      utils$set_blocks(blocks = sel_blocks)
      utils$set_electrodes(electrodes = sel_elec)
      
      dirs = get_dir(subject_code = subject, project_name = project_name, mkdirs = c('preprocess_dir'))
      # file = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
      drops = which(!seq_len(ncols) %in% sel_elec)
      
      # load raw data
      lapply(sel_blocks, function(block){
        fpath = file.path(subject_dir, block, plain_file)
        src = read_csv_no_header(fpath, drop = drops)
        
        lapply(seq_along(sel_elec), function(ii){
          e = sel_elec[ii]
          cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e))
          # if(file.exists(cfile)){ unlink(cfile) }
          s = as.numeric(src[[ii]])
          s[is.na(s)] = 0
          save_h5(s, cfile, name = sprintf('/raw/%s', block), chunk = 1024, replace = TRUE)
          NULL
        })
        NULL
      })
      # set subject meta data, flag it as cached
      utils$save_to_subject(checklevel = 1)
      if(launch_preprocess){
        return(rave_preprocess())
      }else{
        catgl('Loaded. Please launch preprocess:\n    rave::rave_preprocess()', level = 'INFO')
        return(ptype)
      }
      
      
    }
  }
  
  catgl('Cannot find valid raw data files. All selected blocks MUST use the same data format.', level = 'ERROR')
  catgl('Acceptable data formats are:',
       '\n\t1. One file per electrode per block',
       '\n\t2. One file containing all electrodes per block in Matlab or HDF5 format',
       '\n\t3. One text/csv file containing all electrodes per block. One electrode per column with or without column names.',
       level = 'DEFAULT', pal = default_pal)
  return(0)
  
}







#' Create local cache to speed up loading speed
#' @param project_name project name
#' @param subject_code subject code
#' @param epoch epoch name
#' @param time_range time range to cache
#' @export
create_local_cache <- function(project_name, subject_code, epoch, time_range){
  
  soft_deprecated()
  
  cache_dir = subject_cache_dir()
  
  
  # TODO: check data
  check_results = check_subjects2(project_name = project_name, subject_code = subject_code, quiet = TRUE)
  
  # 1. get directories
  subject = Subject$new(subject_code = subject_code, project_name = project_name, strict = FALSE)
  dirs = subject$dirs
  subject_cache_dir = file.path(cache_dir, project_name, subject_code, epoch)
  
  dir_create(file.path(subject_cache_dir, 'coef'))
  dir_create(file.path(subject_cache_dir, 'voltage'))
  dir_create(file.path(subject_cache_dir, 'ref'))
  
  # 2. get epoch data
  epoch_tbl = load_meta(meta_type = 'epoch', subject_id = subject$id, meta_name = epoch)
  # sort by trial orders
  epoch_tbl = epoch_tbl[order(epoch_tbl$Trial), ]
  
  # 3. get sample rate, time points
  srate_wave = subject$sample_rate
  time_pts_wave = seq(-time_range[1] * srate_wave, time_range[2] * srate_wave)
  srate_volt = subject$preprocess_info(key = 'srate')
  time_pts_volt = seq(-time_range[1] * srate_volt, time_range[2] * srate_volt)
  
  # 2. store all raw phase and power
  electrodes = subject$electrodes$Electrode
  progress = progress('Create cache', max = length(electrodes) + 1)
  on.exit({progress$close()})
  gc(); gc()
  lapply_async(electrodes, function(e){
    # read power, phase, volt
    elec = Electrode$new(subject = subject, electrode = e, preload = c('raw_power', 'raw_phase', 'raw_volt'), reference_by = 'noref', is_reference = FALSE)
    
    # get power
    power = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]
      
      time = round(row$Time * srate_wave)
      
      as.vector(elec$raw_power[[row$Block]][, time + time_pts_wave, drop = F])
    })
    
    dims = dim(power)
    dim(power) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
    power = aperm(power, c(3,1,2))
    
    # get phase
    phase = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]
      
      time = round(row$Time * srate_wave)
      
      as.vector(elec$raw_phase[[row$Block]][, time + time_pts_wave, drop = F])
    })
    
    dims = dim(phase)
    dim(phase) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
    phase = aperm(phase, c(3,1,2))
    
    coef = as.vector(sqrt(power) * exp(1i * phase))
    rm(power, phase)
    
    
    coef = data.frame(
      re = Re(coef),
      im = Im(coef)
    )
    
    fst_file = file.path(subject_cache_dir, 'coef', sprintf('%d.fst', e))
    raveio::save_fst(coef, fst_file, compress = 100)
    rm(coef)
    
    # get voltage
    volt = sapply(seq_len(nrow(epoch_tbl)), function(ii){
      row = epoch_tbl[ii, ]
      
      time = round(row$Time * srate_volt)
      
      as.vector(elec$raw_volt[[row$Block]][time + time_pts_volt])
    })
    volt = data.frame(volt = as.vector(t(volt)))
    fst_file = file.path(subject_cache_dir, 'voltage', sprintf('%d.fst', e))
    raveio::save_fst(volt, fst_file, compress = 100)
    rm(volt)
    
  }, .call_back = function(ii){
    progress$inc(sprintf('Electrode %d', electrodes[[ii]]))
    # progress$inc(sprintf('Electrode %d', e))
  }, .globals = c('electrodes', 'e', 'subject', 'epoch_tbl', 'srate_wave', 'time_pts_wave', 'subject_cache_dir', 'srate_volt', 'time_pts_volt'))
  
  # save references
  ref_dir = file.path(dirs$cache_dir, 'reference')
  progress$inc('Caching References')
  if(dir.exists(ref_dir)){
    ref_files = list.files(ref_dir, pattern = '*\\.h5')
    for(f in ref_files){
      progress$inc(f)
      fpath = file.path(ref_dir, f)
      volt_h5 = load_h5(fpath, '/voltage/008', ram = TRUE)
      
      # get voltage
      volt = sapply(seq_len(nrow(epoch_tbl)), function(ii){
        row = epoch_tbl[ii, ]
        time = round(row$Time * srate_volt)
        as.vector(volt_h5[time + time_pts_volt])
      })
      volt = data.frame(Volt = as.vector(t(volt)))
      fst_file = file.path(subject_cache_dir, 'ref', sprintf('%s.volt.fst', f))
      raveio::save_fst(volt, fst_file, compress = 100)
      rm(volt)
      
      
      coef = load_h5(fpath, '/wavelet/coef/008', ram = TRUE)
      coef = coef[,,1, drop = F] * exp(1i * coef[,,2, drop = FALSE])
      coef = sapply(seq_len(nrow(epoch_tbl)), function(ii){
        row = epoch_tbl[ii, ]
        time = round(row$Time * srate_wave)
        as.vector(coef[, time + time_pts_wave, 1])
      })
      dims = dim(coef)
      dim(coef) = c(dims[1] / length(time_pts_wave), length(time_pts_wave), dims[2])
      coef = aperm(coef, c(3,1,2))
      coef = as.vector(coef)
      coef = data.frame(
        re = Re(coef),
        im = Im(coef)
      )
      
      fst_file = file.path(subject_cache_dir, 'ref', sprintf('%s.coef.fst', f))
      raveio::save_fst(coef, fst_file, compress = 100)
      rm(coef)
    }
  }
  
  # generate yamls
  
  config = list(
    project_name = subject$project_name,
    subject_code = subject$subject_code,
    epoch = epoch,
    srate_volt = subject$preprocess_info('srate'),
    srate_wave = subject$sample_rate,
    frequecies = subject$frequencies$Frequency,
    time_range = c(2, 4),
    electrodes = dipsaus::deparse_svec(electrodes)
  )
  
  config$digest = digest::digest(config)
  config$epoch_signatures = digest::digest(epoch_tbl[, c('Block', 'Time', 'Trial')]) # sorted by trial!
  
  
  raveio::save_yaml(config, file.path(subject_cache_dir, 'config.yaml'), fileEncoding = 'utf-8')
  
  utils::write.csv(epoch_tbl, file.path(subject_cache_dir, 'epoch.csv'), row.names = FALSE)
  
  # save all references
  sapply(check_results$references, function(ref){
    tbl = load_meta('references', project_name = project_name, subject_code = subject_code, meta_name = ref)
    utils::write.csv(tbl, file.path(subject_cache_dir, 'ref', sprintf('reference_%s.csv', ref)), row.names = FALSE)
  })
  
  invisible()
}


#' Load local cache for fast importing voltage, power, and phase
#' @param project_name project name
#' @param subject_code subject code
#' @param epoch epoch name
#' @param time_range time range to cache
#' @param frequency_range frequency range to cache
#' @param electrodes electrodes to cache
#' @param referenced which reference to be used
#' @param data_type which type(s) of data to cache
load_local_cache <- function(project_name, subject_code, epoch, time_range,
                             frequency_range = NULL, electrodes,
                             referenced = FALSE, data_type = 'voltage'){
  
  # project_name = 'demo'
  # subject_code = 'sub_large'
  # epoch = 'Auditory'
  # time_range = c(1,2)
  
  # first, check if cache exists
  cache_dir = file.path(subject_cache_dir(), project_name, subject_code, epoch)
  if(!dir.exists(cache_dir)){
    # cache missing
    return(invisible())
  }
  
  # 2, load cached configs
  tryCatch({
    config = raveio::load_yaml(file.path(cache_dir, 'config.yaml'))
    
    epoch_cached = utils::read.csv(
      file.path(cache_dir, 'epoch.csv'),
      colClasses = 'character',
      stringsAsFactors = FALSE
    )[, c('Block', 'Time', 'Trial')]
    epoch_cached$Time = as.numeric(epoch_cached$Time)
    epoch_cached$Trial = as.numeric(epoch_cached$Trial)
    
    
    # get epoch file if possible
    epoch_tbl = NULL
    subject = NULL
    try({
      epoch_tbl = load_meta('epoch', project_name = project_name, subject_code = subject_code, meta_name = epoch)
      epoch_tbl = epoch_tbl[order(epoch_tbl$Trial), c('Block', 'Time', 'Trial')]
      
      subject = Subject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
    })
    
    if(!is.null(subject)){
      stopifnot2(
        config$srate_volt == subject$preprocess_info('srate'),
        config$srate_wave == subject$sample_rate
      )
    }
    
    if(is.data.frame(epoch_tbl)){
      stopifnot2(
        nrow(epoch_tbl) == nrow(epoch_cached),
        all(epoch_tbl$Block == epoch_cached$Block),
        all(abs(epoch_tbl$Time - epoch_cached$Time) < 0.01),
        all(epoch_tbl$Trial == epoch_cached$Trial)
      )
    }
    
    
    # check if cache contains all data
    stopifnot2(
      config$epoch == epoch,
      all(time_range <= config$time_range),
      all(electrodes %in% dipsaus::parse_svec(config$electrodes))
    )
    
    # get data
    coef_dir = file.path(cache_dir, 'coef')
    volt_dir = file.path(cache_dir, 'voltage')
    ref_dir = file.path(cache_dir, 'ref')
    
    # get reference table
    if(isFALSE(referenced)){
      ref_table = NULL
    }else{
      ref_file = file.path(ref_dir, sprintf('reference_%s.csv', referenced))
      if(file.exists(ref_file)){
        ref_table = utils::read.csv(ref_file, row.names = NULL, stringsAsFactors = F)
      }else{
        ref_table = load_meta('references', project_name = project_name, subject_code = subject_code, meta_name = referenced)
      }
      
      stopifnot2(!is.null(ref_table))
    }
    
    re = list()
    if(any(data_type %in% c('volt', 'voltage'))){
      # load voltage
      volt = load_cached_voltage(
        cache_dir = cache_dir,
        electrodes = electrodes,
        time_range = config$time_range,
        srate_volt = config$srate_volt,
        trial = epoch_cached$Trial,
        ref_table = ref_table,
        subset_time = time_range
      )
      
      dims = volt$dims
      
      el = Tensor$new(0, dim = c(1,1,1), varnames = c('Trial', 'Time', 'Electrode'), hybrid = FALSE)
      el$dim = dims
      el$dimnames = list(
        Trial = epoch_cached$Trial,
        Time = volt$time_points,
        Electrode = electrodes
      )
      el$set_data(NULL)
      el$use_index = TRUE
      el$hybrid = TRUE
      volt$data = data.frame(volt$data)
      names(volt$data) = paste0('V', seq_len(ncol(volt$data)))
      raveio::save_fst(volt$data, el$swap_file)
      
      rm(volt)
      
      re[['volt']] = el
    }
    
    if(any(c('power', 'phase') %in% data_type)){
      coef = load_cached_wave(
        cache_dir = cache_dir,
        electrodes = electrodes,
        time_range = config$time_range,
        srate_wave = config$srate_wave,
        trial = epoch_cached$Trial,
        frequency = config$frequecies,
        ref_table = ref_table,
        data_type = data_type,
        subset_time = time_range,
        subset_freq = frequency_range
      )
      dims = coef$dims
      frequency_range %?<-% config$frequecies
      
      el = ECoGTensor$new(0, dim = c(1,1,1,1), varnames = c('Trial', 'Frequency', 'Time', 'Electrode'), hybrid = FALSE)
      el$dim = dims
      el$dimnames = list(
        Trial = epoch_cached$Trial,
        Frequency = config$frequecies[config$frequecies %within% frequency_range],
        Time = coef$time_points,
        Electrode = electrodes
      )
      el$set_data(NULL)
      el$use_index = TRUE
      el$hybrid = TRUE
      
      # colnames(coef$data) = paste0('V', seq_len(ncol(coef$data)))
      
      
      if(all(c('power', 'phase') %in% data_type)){
        el2 = el$clone(deep = TRUE)
        el2$swap_file = tempfile()
        
        power = data.frame(V1 = coef$data[[1]])
        for(ii in seq_len(length(coef$data))){
          power[[paste0('V', ii)]] = Mod(coef$data[[ii]])^2
        }
        raveio::save_fst(power, el$swap_file)
        rm(power)
        
        phase = data.frame(V1 = coef$data[[1]])
        for(ii in seq_len(length(coef$data))){
          phase[[ii]] = Arg(coef$data[[ii]])
        }
        
        raveio::save_fst(phase, el2$swap_file)
        rm(coef)
        rm(phase)
        
        re[['phase']] = el2
        re[['power']] = el
      }else{
        
        raveio::save_fst(as.data.frame(coef$data, col.names = paste0('V', seq_len(length(coef$data)))), el$swap_file)
        if('power' %in% data_type){
          re[['power']] = el
        }else{
          re[['phase']] = el
        }
      }
      
      
    }
    
    re
  }, error = function(e){
    # error occured, no data returned
    NULL
  }) ->
    re
  return(re)
  
  
}


load_cached_wave = function(cache_dir, electrodes, time_range,
                            srate_wave, trial, frequency,
                            ref_table = NULL, data_type,
                            subset_time, subset_freq){
  
  soft_deprecated()
  subset_time[1] = -subset_time[1]
  progress = progress('Load from local cache - Power/Phase', max = length(electrodes) + 2)
  on.exit({progress$close()})
  
  coef_dir = file.path(cache_dir, 'coef')
  ref_dir = file.path(cache_dir, 'ref')
  tp = seq(- time_range[1] * srate_wave, time_range[2] * srate_wave) / srate_wave
  
  # calculate dimensions
  n_pt_cached = length(tp)
  subset_freq %?<-% frequency
  n_freq_cached = length(frequency)
  
  n_trial = length(trial)
  
  # create sample data
  final_dim = c(n_trial, n_freq_cached, n_pt_cached)
  idx = array(seq_len(prod(final_dim)), final_dim)
  idx = idx[, frequency %within% subset_freq, tp %within% subset_time]
  final_dim = c(dim(idx), length(electrodes))
  idx = as.vector(idx)
  idx_range = range(idx)
  idx = idx - idx_range[1] + 1
  
  
  
  
  # load all references
  ref_data = list()
  need_reference = FALSE
  if(is.data.frame(ref_table)){
    progress$inc('Prepare references')
    need_reference = TRUE
    refs = unique(ref_table$Reference)
    for(f in refs){
      ref_e = dipsaus::parse_svec(f)
      if(length(ref_e) == 1){
        # this is bipolar-ish reference
        d = raveio::load_fst(file.path(coef_dir, sprintf('%d.fst', ref_e)),
                     from = idx_range[1], to = idx_range[2])
        ref_data[[f]] = d[idx, ]
        rm(d)
      }
      if(length(ref_e) > 1){
        # this is car-ish reference
        d = raveio::load_fst(file.path(ref_dir, sprintf('%s.h5.coef.fst', f)),
                     from = idx_range[1], to = idx_range[2])
        ref_data[[f]] = d[idx, ]
        rm(d)
      }
      
    }
  }else{
    progress$inc('Initializing')
  }
  
  # check if both phase and power is needed
  need_both = all(c('power', 'phase') %in% data_type)
  
  
  
  
  data = lapply_async(electrodes, function(e){
    fst_file = file.path(coef_dir, sprintf('%d.fst', e))
    
    d = raveio::load_fst(fst_file, from = idx_range[1], to = idx_range[2])[idx, ]
    # d = d[idx, ]
    
    # trial x freq x time
    
    
    if(need_reference){
      f = ref_table$Reference[ref_table$Electrode == e]
      ref = ref_data[[f]]
      if(!is.null(ref)){
        d = d - ref[idx, ]
      }
    }
    if(need_both){
      d = d$re + (d$im) * 1i
    }else{
      if('power' %in% data_type){
        d = (d$re)^2 + (d$im)^2
      }else{
        d = atan2(d$im, d$re)
      }
    }
    return(d)
  }, .call_back = function(ii){
    progress$inc(sprintf('Loading electrode %d', electrodes[[ii]]))
  }, .globals = c('electrodes', 'e', 'coef_dir', 'idx_range', 'need_reference', 'ref_table', 'ref_data',
                  'need_both', 'data_type'))
  
  list(
    data = data,
    time_points = tp[tp %within% subset_time],
    dims = final_dim
  )
}



load_cached_voltage <- function(cache_dir, electrodes, time_range, srate_volt, trial, ref_table = NULL,
                               subset_time){
  
  soft_deprecated()
  subset_time[1] = -subset_time[1]
  progress = progress('Load from local cache - Voltage', max = length(electrodes) + 2)
  on.exit({progress$close()})
  
  volt_dir = file.path(cache_dir, 'voltage')
  ref_dir = file.path(cache_dir, 'ref')
  tp = seq(- time_range[1] * srate_volt, time_range[2] * srate_volt) / srate_volt
  
  n_trials = length(trial)
  n_tp = length(tp)
  idx = array(seq_len(n_trials * n_tp), c(n_trials, n_tp))
  idx = idx[, tp %within% subset_time]
  final_dim = dim(idx)
  idx = range(idx)
  
  # load all references
  ref_data = list()
  need_reference = FALSE
  if(is.data.frame(ref_table)){
    progress$inc('Prepare references')
    need_reference = TRUE
    refs = unique(ref_table$Reference)
    for(f in refs){
      ref_e = dipsaus::parse_svec(f)
      if(length(ref_e) == 1){
        # this is bipolar-ish reference
        d = raveio::load_fst(file.path(volt_dir, sprintf('%d.fst', ref_e)), from = idx[1], to = idx[2])[,1]
        ref_data[[f]] = d
      }
      if(length(ref_e) > 1){
        # this is car-ish reference
        d = raveio::load_fst(file.path(ref_dir, sprintf('%s.h5.volt.fst', f)), from = idx[1], to = idx[2])[,1]
        ref_data[[f]] = d
      }
    }
  }else{
    progress$inc('Initializing')
  }
  
  
  
  
  data = sapply(electrodes, function(e){
    progress$inc(sprintf('Loading electrode %d', e))
    fst_file = file.path(volt_dir, sprintf('%d.fst', e))
    d = raveio::load_fst(fst_file, from = idx[1], to = idx[2])[,1]
    
    if(need_reference){
      f = ref_table$Reference[ref_table$Electrode == e]
      ref = ref_data[[f]]
      if(!is.null(ref)){
        d = d - ref
      }
    }
    return(as.vector(d))
  })
  
  list(
    data = data,
    time_points = tp[tp %within% subset_time],
    dims = c(final_dim, length(electrodes))
  )
}



#' Function to download demo data to data repository
#' @param subject demo subject
#' @param version rave release version
#' @param ... other parameters passed to \code{download_subject_data}
#' @return Nothing
#' @export
download_sample_data <- function(subject, version = 'v0.1.8-beta', ...){
  
  if(missing(subject)) {
    sbj_names = c('KC', 'YAB', '_group_data')
    version = 'v0.1.8-beta'
    sapply(sbj_names, download_sample_data, version=version, ...)
    return (invisible())
  }
  
  
  url = sprintf('https://github.com/beauchamplab/rave/releases/download/%s/demo_%s.zip', version, subject)
  download_subject_data(url, ...)
}

#' Function to download subjects from internet/local
#' @param con an url or local file path
#' @param replace_if_exists Automatically replace current subject if subject 
#' files exist (default FALSE)
#' @param temp_dir temp directory to store downloaded zip files and extracted 
#' files
#' @param remove_zipfile clear downloaded zip files? if \code{con} is local 
#' file, this will be forced to FALSE
#' @param subject_settings override \code{"subject.yaml"} see details
#' @param override_project if not null, project will be renamed to this value
#' @param override_subject if not null, subject will be renamed to this value
#' @param mode,... passed to \code{\link[utils]{download.file}}
#' @examples
#' \dontrun{
#' # Normal example
#' download.file(
#'   'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip',
#'   destfile = "~/rave_data/data-small.zip", mode = "wb")
#' download_subject_data(con = "~/rave_data/data-small.zip")
#'
#' # or the following
#' # download_subject_data(
#' # 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
#' # )
#'
#' # rename project to demo_junk
#' download_subject_data(con = "~/rave_data/data-small.zip",
#'            override_project = 'demo_junk')
#'
#' # override settings
#' download_subject_data(
#'   con = "~/rave_data/data-small.zip",
#'   subject_settings = list(
#'     # subject conf
#'     'demo_project/demo_subject' = list(
#'       data_dir = 'data 2/data_dir/demo/sub1',
#'       raw_dir = 'data 2/raw_dir/sub1'
#'     )
#'   )
#' )
#' }
#' @details
#' Each downloaded zip file should have a \code{"subject.yaml"} file indicating 
#' default project name, subject code, data directory and raw data directory. 
#'
#' If you want to override subject settings, you need to implement your own
#' \code{subject_settings}. See examples.
#' @export
download_subject_data <- function(
  con, replace_if_exists = FALSE, override_project = NULL, override_subject = NULL,
  temp_dir = tempdir(), remove_zipfile = TRUE, subject_settings = NULL,
  mode = "wb", ...){
  
  # Large files, need long time to download
  opt_timeout <- getOption('timeout', 1e10)
  on.exit({
    options('timeout' = opt_timeout)
  }, add = TRUE, after = TRUE)
  options('timeout' = 1e10)
  
  
  
  url = con
  # url = 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
  # url = "/var/folders/rh/4bkfl5z50wgbbjd85xvc695c0000gn/T//RtmpmUoaTy/junk_45d3370d10d.zip"
  
  if(!file.exists(url)){
    # this is not a local file, download
    
    # First, try to download subject data
    catgl('Download from - ', url, level = 'INFO')
    
    # prepare files
    temp_file = tempfile(pattern = 'junk_', temp_dir, fileext = '.zip')
    # download
    utils::download.file(url, destfile = temp_file, mode = mode, ...)
  }else{
    remove_zipfile = FALSE
    temp_file = url
  }
  
  extract_dir = file.path(temp_dir, paste(sample(LETTERS, 10), collapse = ''))
  dir_create(extract_dir)
  on.exit({
    # clean up
    unlink(extract_dir, recursive = TRUE)
    if(remove_zipfile){
      unlink(temp_file, recursive = TRUE)
    }else{
      catgl('Please manually remove zip file by running:\n',
             sprintf('unlink("%s")', temp_file), level = 'INFO')
    }
  })
  
  
  # Extract
  catgl('Unzip the folder', level = 'INFO')
  utils::unzip(temp_file, exdir = extract_dir, overwrite = TRUE)
  
  # Check folder
  # look for meta.yaml
  if(is.null(subject_settings)){
    yaml_files = list.files(extract_dir, pattern = 'subjects.yaml$', recursive = TRUE, full.names = TRUE)
    
    if(length(yaml_files)){
      depth = stringr::str_count(yaml_files, '(/|\\\\)')
      file = yaml_files[which.min(depth)[1]]
      meta = as.list(raveio::load_yaml(file))
    }else{
      catgl('No subjects.yaml found! Please use "subject_settings" argument to specify subject settings', level = 'FATAL')
    }
  }else{
    meta = subject_settings
  }
  
  
  # check data
  for(ii in seq_along(meta)){
    catgl('----------------------------', level = 'INFO')
    subject_id = names(meta)[[ii]]
    subject_id %?<-% ''
    
    # get subject project name and subject code
    s = strsplit(subject_id, '/|\\\\')[[1]]
    
    s = s[s!='']
    if(length(s) < 2){
      catgl('Invalid subject ID - ', subject_id, ' (abort)', level = 'ERROR')
      next()
    }
    if(is.null(override_project)){
      project_name = s[1]
    }else{
      project_name = override_project
    }
    
    if(is.null(override_subject)){
      subject_code = s[2]
    }else{
      subject_code = override_subject
    }
    catgl('Project Name: [', project_name, ']; Subject Code: [', subject_code, '] (checking)', level = 'INFO')
    
    
    
    data_dir = meta[[ii]][['data_dir']]
    raw_dir = meta[[ii]][['raw_dir']]
    
    # check if data_dir exists
    rdir = extract_dir
    ds = list.dirs(extract_dir, full.names = FALSE, recursive = FALSE)
    if( !'data_dir' %in% ds ){
      ds = ds[stringr::str_length(ds) == 1 | stringr::str_detect(ds, '^[^.~][^_]')]
      ds = ds[!ds %in% c('.', '_', '~', '^')]
      if(length(ds)){
        rdir = file.path(rdir, ds[1])
      }
    }
    
    if(length(data_dir) == 1 && is.character(data_dir)){
      # try to find dir
      data_dir = file.path(rdir, data_dir)
      if(!dir.exists(data_dir)){
        catgl('\n\tdata_dir not exists\n',
               '\tPlease check existence of data_dir: \n', data_dir, level = 'WARNING')
      }else{
        catgl('\tdata directory found! - \n', data_dir, level = 'INFO')
      }
    }else{
      data_dir = NULL
      catgl('No "data_dir" in subject settings', level = 'WARNING')
    }
    
    
    if(length(raw_dir) == 1 && is.character(raw_dir)){
      # try to find dir
      raw_dir = file.path(rdir, raw_dir)
      if(!dir.exists(raw_dir)){
        catgl('\n\traw_dir not exists\n',
               '\tPlease check existence of raw_dir: \n', raw_dir, level = 'WARNING')
      }else{
        catgl('\traw directory found! - \n', raw_dir, level = 'INFO')
      }
    }else{
      raw_dir = NULL
      catgl('No "raw_dir" in subject settings, abort this one', level = 'WARNING')
    }
    
    
    # Check subject existence
    rave_data_dir = rave_options('data_dir')
    rave_raw_dir = rave_options('raw_data_dir')
    check_existence = function(subject_code){
      has_subject = c(FALSE, FALSE)
      exist_proj = list.dirs(rave_data_dir, full.names = FALSE, recursive = FALSE)
      if(project_name %in% exist_proj){
        # need to check if subject exists
        exist_subs = list.dirs(file.path(rave_data_dir, project_name), full.names = FALSE, recursive = FALSE)
        if(subject_code %in% exist_subs){
          has_subject[2] = TRUE
        }
      }
      if(subject_code %in% list.dirs(rave_raw_dir, full.names = FALSE, recursive = FALSE)){
        has_subject[1] = TRUE
      }
      has_subject
    }
    
    
    if(!replace_if_exists && any(check_existence(subject_code))){
      count = 5
      choice = subject_code
      while(count > 0){
        count = count - 1
        catgl('\nSubject [', choice, '] already exists. Replace? or enter new subject code here:\n',
               '\t- yes, or Y(y) to overwrite\n',
               '\t- any other characters for new subject code\n',
               '\t- or leave it blank to cancel importing this subject', level = 'WARNING')
        choice = readline(prompt = ':')
        choice = stringr::str_trim(choice)
        if(!stringr::str_detect(stringr::str_to_lower(choice), '^(y$)|(yes$)')){
          # rename
          if(choice == ''){
            catgl('Cancel importing ', subject_code, level = 'INFO')
            subject_code = ''
            break
          }
          if(!any(check_existence(choice))){
            catgl('Rename subject to ', choice, level = 'INFO')
            catgl('Renaming subjects might cause some problems for SUMA', level = 'WARNING')
            subject_code = choice
            
            break()
          }
        }else{
          catgl('Overwrite subject ', subject_code, level = 'INFO')
          break()
        }
      }
      
      
    }
    
    # Now we need to check
    if(subject_code == ''){
      next()
    }
    
    # importing subject
    catgl('Copy files:', level = 'INFO')
    # raw dir
    to_dir = file.path(rave_raw_dir, subject_code)
    if(length(raw_dir)){
      dir_create(to_dir)
      lapply(list.files(raw_dir, all.files = TRUE, full.names = TRUE, recursive = FALSE), function(d){
        file.copy(d, to_dir, overwrite = TRUE, recursive = TRUE)
      })
      catgl('[New raw dir] ', to_dir, level = 'INFO')
    }else{
      catgl('Raw data is not imported.')
    }
    
    
    # data dir
    to_dir = file.path(rave_data_dir, project_name, subject_code)
    if(length(data_dir)){
      dir_create(to_dir)
      lapply(list.files(data_dir, all.files = TRUE, full.names = TRUE, recursive = FALSE), function(d){
        file.copy(d, to_dir, overwrite = TRUE, recursive = TRUE)
      })
      catgl('[New data dir] ', to_dir, level = 'INFO')
    }else{
      catgl('RAVE data is not imported.')
    }
    
    catgl('\n\t[', project_name, '/', subject_code, '] Done.\n', level = 'INFO')
  }
  
  catgl('\n----------------------------', level = 'INFO')
  
}


# download_subject_data(con = "~/rave_data/data-small.zip",
# override_project = 'demo_junk', replace_if_exists = F)


#' Archive Subject into Zipped file
#' @description Save subject data, including brain imaging files into a zipped 
#' file. Notice this function does not guarantee every file is in. Please always
#' double check what's inside.
#' @param project_name project name
#' @param subject_code subject code
#' @param include_cache whether to include cache for faster loading. Default is 
#' false
#' @param include_fs whether to include 'FreeSurfer' and 'AFNI/SUMA' files
#' @param include_raw whether to include raw data
#' @param save_to directory to save file to
#' @export
archive_subject <- function(project_name, subject_code, 
                            include_cache = FALSE, include_fs = TRUE, include_raw = FALSE,
                            save_to = tempdir()){
  subject = Subject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
  root_dir = file.path(tempdir(check = TRUE), 'archive')
  rave_dir = file.path(root_dir, 'data_dir', project_name, subject_code, 'rave')
  raw_dir = file.path(root_dir, 'raw_dir', subject_code)
  wd = getwd()
  on.exit({
    unlink(root_dir, recursive = TRUE, force = FALSE)
    setwd(wd)
  })
  
  # setwd(subject$dirs$rave_dir)
  dir_create(rave_dir)
  dir_create(raw_dir)
  
  dirs = list.dirs(subject$dirs$rave_dir, full.names = FALSE, recursive = TRUE)
  
  dirs = dirs[!stringr::str_detect(dirs, '^fs(/|$)')]
  
  sapply(file.path(rave_dir, dirs), function(d){
    dir_create(d)
  })
  
  paths = c(
    'data/cache/cached_reference.csv',
    'data/power/',
    'data/phase/',
    'data/reference/',
    'data/voltage/',
    'log.yaml',
    'meta/',
    'preprocess/'
  )
  if(include_cache){
    paths[1] = 'data/cache/'
  }
  
  from_paths = file.path(subject$dirs$rave_dir, paths)
  sel = file.exists(from_paths)
  from_paths = from_paths[sel]
  to_paths = file.path(rave_dir, paths)[sel]
  
  apply(cbind(from_paths, to_paths), 1, function(x){
    x[2] = dirname(x[2])
    dir_create(x[2])
    print(x[2])
    if(file.info(x[1])[['isdir']]){
      file.copy(x[1], x[2], overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE, recursive = TRUE)
    }else{
      file.copy(x[1], x[2], overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
    }
  })
  
  if(include_fs){
    fs_paths = file.path(subject$dirs$rave_dir, c('fs', '../fs'))
    sel = dir.exists(fs_paths)
    if(any(sel)){
      fs_paths = normalizePath(fs_paths[sel][1])
      target_fs = normalizePath(file.path(rave_dir, '..'))
      file.copy(fs_paths, target_fs, overwrite = TRUE, recursive = TRUE, copy.mode = FALSE, copy.date = TRUE)
    }
    if(!include_cache){
      unlink(file.path(rave_dir, '..', 'fs', 'RAVE'), recursive = TRUE)
    }
  }
  
  if(include_raw){
    raws = list.dirs(subject$dirs$pre_subject_dir, full.names = TRUE, recursive = FALSE)
    lapply(raws, function(f){
      file.copy(f, raw_dir, recursive = TRUE, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
    })
  }
  
  # yaml file
  conf = list(list(
    data_dir = sprintf('data_dir/%s/%s', project_name, subject_code),
    raw_dir = sprintf('raw_dir/%s', subject_code)
  ))
  names(conf) = subject$id
  
  raveio::save_yaml(conf, file = file.path(root_dir, 'subjects.yaml'), fileEncoding = 'utf-8')
  dir_create(save_to)
  save_to = file.path(normalizePath(save_to), sprintf('%s_%s.zip', project_name, subject_code))
  setwd(root_dir)
  utils::zip(zipfile = save_to, files = '.')
  setwd(wd)
  
  catgl('Please check zip file at ', save_to, level = 'INFO')
}






# functions for group analysis

#' Find module analysis names
#' @param module_id module id
#' @param project_name project name
#' @param data_env internally used
#' @export
module_analysis_names <- function(
  module_id, project_name, data_env = getDefaultDataRepository()){
  
  soft_deprecated()
  # if missing project_name, get from current repository
  if(missing(project_name)){
    project_name = data_env$subject$project_name
  }
  
  # abuse function get_dir
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir
  
  if(!dir.exists(lookup_dir)){
    dir.create(lookup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  if(!missing(module_id)){
    module_id = stringr::str_to_upper(module_id)
    pattern = sprintf('^%s\\-([a-zA-Z0-9_]+).[cC][sS][vV]$', module_id)
  }else{
    pattern = '\\-([a-zA-Z0-9_]+).[cC][sS][vV]$'
  }
  filenames = list.files(path = lookup_dir, pattern = pattern)
  analysis_names = NULL
  if(length(filenames)){
    analysis_names = stringr::str_match(filenames, pattern)[,2]
  }
  analysis_names
}


#' Create temp file in subject module folder
#' @param module_id module id
#' @param fun_name function name (usually export_"function_name" in the module)
#' @param project_name project name
#' @param subject_code subject code
#' @param pattern passed to \code{\link{tempfile}}
#' @param data_env internally used
#' @export
subject_tmpfile <- function(module_id, fun_name = '', project_name, 
                            subject_code, pattern = 'file_',
                            data_env = getDefaultDataRepository()){
  soft_deprecated()
  if(missing(project_name)){
    project_name = data_env$subject$project_name
    subject_code = data_env$subject$subject_code
  }
  
  stopifnot2(!is.blank(project_name) && !is.blank(subject_code), msg = 'subject_code or project_name is blank while creating subject tmpfile.')
  
  tmpdir = get_dir(subject_code = subject_code, project_name = project_name)$module_data_dir
  tmpdir = file.path(tmpdir, module_id, fun_name)
  if(!dir.exists(tmpdir)){
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  }
  tempfile(tmpdir = tmpdir, pattern = pattern)
}

module_analysis_table <- function(project_name, module_id, 
                                  analysis_name, check_valid = FALSE, 
                                  data_env = getDefaultDataRepository()){
  soft_deprecated()
  # if missing project_name, get from current repository
  if(missing(project_name)){
    project_name = data_env$subject$project_name
  }
  module_id = stringr::str_to_upper(module_id)
  analysis_name = stringr::str_to_upper(analysis_name)
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir
  lookup_file = file.path(lookup_dir, sprintf('%s-%s.csv', module_id, analysis_name))
  if(file.exists(lookup_file)){
    tbl = utils::read.csv(lookup_file, stringsAsFactors = F)
    if(check_valid){
      # Check if all path is valid
      # Only check valid ones
      lapply(tbl$file, function(path){
        find_path(path)
      }) ->
        paths
      is_valid = vapply(paths, function(p){!is.null(p)}, FUN.VALUE = TRUE)
      use_safe = F
      if(any(!is_valid)){
        use_safe = TRUE
      }
      tbl$valid = is_valid
      tbl$file[is_valid] = unlist(paths[is_valid])
      tbl = tbl[tbl$valid, ]
      # write back to file
      if(use_safe){
        safe_write_csv(tbl, lookup_file, row.names = F)
      }else{
        utils::write.csv(tbl, lookup_file, row.names = F)
      }
    }else{
      tbl = tbl[tbl$valid, ]
    }
    
  }else{
    tbl = data.frame()
  }
  return(tbl)
}


module_analysis_save <- function(project_name, subject_code, module_id, analysis_name, file, meta = NULL, data_env = getDefaultDataRepository()){
  soft_deprecated()
  # if missing project_name, get from current repository
  if(missing(project_name) || missing(subject_code)){
    project_name = data_env$subject$project_name
    subject_code = data_env$subject$subject_code
  }
  
  # abuse function get_dir
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir
  
  if(!dir.exists(lookup_dir)){
    dir.create(lookup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  module_id = stringr::str_to_upper(module_id)
  analysis_name = stringr::str_extract_all(analysis_name, '[a-zA-Z0-9_]', simplify = TRUE)
  analysis_name = paste(analysis_name, collapse = '')
  analysis_name = stringr::str_to_upper(analysis_name)
  
  lfile = file.path(lookup_dir, sprintf('%s-%s.csv', module_id, analysis_name))
  
  if(file.exists(lfile)){
    old_data = utils::read.csv(lfile)
  }else{
    old_data = data.frame()
  }
  
  valid = FALSE
  if(file.exists(file)){
    catgl('File detected, export meta info')
    file = normalizePath(file)
    valid = TRUE
  }else{
    catgl('File not detected, meta info will be ignored')
    return()
  }
  
  v = data.frame(
    project_name = project_name,
    subject_code = subject_code,
    module_id = module_id,
    analysis_name = analysis_name,
    file = file,
    timestamp = as.numeric(Sys.time()),
    valid = valid
  )
  
  if(is.data.frame(meta) && nrow(meta) == 1){
    nv = cbind(v, meta)
  }else{
    nv = v
  }
  tryCatch({
    new_data = rbind(nv, old_data)
    utils::write.csv(new_data, file = lfile, row.names = F)
  }, error = function(e){
    if(setequal(names(v), names(old_data))){
      # get rid of meta data
      new_data = rbind(v, old_data)
      utils::write.csv(new_data, file = lfile, row.names = F)
    }else{
      safe_write_csv(v, lfile, row.names = F)
    }
  })
}
