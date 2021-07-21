#' Function to create RAVE preprocess tools
#' @param env environment to save tools to
#' @param ... ignored
#' @export
rave_preprocess_tools <- function(env = new.env(), ...){
  stopifnot2(is.environment(env), msg = 'env MUST be an environment')
  
  # initialize subject
  env$subject = list()
  
  
  # Functions
  utils = new.env()
  
  local({
    # Set check levels for subjects
    NO_SUBJECT = 0
    HAS_CACHE = 1
    NOTCH_FILTERED = 2
    WAVELETED = 3
    REFERENCED = 4  # Soft deprecated since the reference module is moved to main module
    ###### UI override ######
    showNotification = function(msg, type = 'message', session = getDefaultReactiveDomain()){
      if(is.null(session)){
        level = switch (
          type,
          'error' = 'ERROR',
          'warning' = 'WARNING',
          'message' = 'INFO',
          'success' = 'INFO',
          type
        )
        catgl(msg, level = level)
      }else{
        shiny::showNotification(p(HTML(paste(msg, collapse = '<br/>'))), type = type)
      }
    }
    ###### Load/save subject ######
    clear_subject = function(){
      env$subject = list()
    }
    check_load_subject = function(subject_code, project_name, details = T, strict = TRUE){
      checklist = list()
      ind = 1
      # Step 1: check if subject folders exists
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      check_names = list(
        'subject_dir' = 'Subject Folder',
        'rave_dir' = 'RAVE Folder',
        'preprocess_dir' = 'Preprocessing Folder',
        'meta_dir' = 'Meta Folder',
        'channel_dir' = 'Channel Folder',
        'suma_dir' = 'SUMA Folder',
        'suma_out_dir' = 'SUMA-RAVE Folder',
        'pre_subject_dir' = 'Raw Directory'
      )
      sapply(names(check_names), function(nm){
        l = list(
          dir.exists(dirs[[nm]])
        )
        names(l) = check_names[[nm]]
        l
      }, simplify = TRUE, USE.NAMES = FALSE) ->
        l
      checklist[['Folders']] = l
      
      # step 2: Check if subject has preprocessed yet
      l = list(
        'Started Preprocess' = FALSE,
        'Notch Filter' = FALSE,
        'Wavelet' = FALSE,
        'Referenced' = FALSE
      )
      try({
        utils$load_subject(subject_code = subject_code, project_name = project_name, strict = strict)
        l[seq_len(utils$get_check_level())] = TRUE
      }, silent = TRUE)
      
      checklist[['Preprocess']] = l
      
      
      # step 3: Check if subject meta data is ready
      meta_dir = dirs$meta_dir
      meta_files = list.files(meta_dir)
      
      l = list(
        'Electrode File' = 'electrodes.csv' %in% meta_files,
        'Time point File' = 'time_points.csv' %in% meta_files,
        'Frequency File' = 'frequencies.csv' %in% meta_files,
        'Epoch File' = any(stringr::str_detect(meta_files, '^epoch_[a-zA-Z0-9]+\\.[cC][sS][vV]$')),
        'Reference file' = any(stringr::str_detect(meta_files, '^reference_[a-zA-Z0-9]+\\.[cC][sS][vV]$'))
      )
      
      checklist[['Meta']] = l
      
      checklist
    }
    load_subject = function(subject_code, project_name, strict = TRUE){
      catgl('Loading Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      stopifnot2(dir.exists(dirs$preprocess_dir), msg = paste0('Subject ' , subject_code , ' has no project folder ' , project_name))
      s = SubjectInfo2$new(project_name = project_name, subject_code = subject_code, strict = strict)
      env[['subject']] = s
      
      catgl('Loaded Subject: ', utils$get_subject_id())
    }
    save_subject = function(subject_code, project_name){
      catgl('Saving Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      is_created = FALSE
      if(!dir.exists(dirs$preprocess_dir)){
        dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = c('preprocess_dir', 'cache_dir', 'meta_dir', 'suma_dir'))
        is_created = TRUE
      }else{
        # save informations if subject exists
        if(utils$has_subject() && (
          env$subject$subject_code == subject_code &&
          env$subject$project_name == project_name
        )){
          env$subject$save(action = 'Manual Save', message = '')
        }
      }
      
      # call to load subject
      utils$load_subject(subject_code, project_name)
      
      catgl('saved Subject')
      return(is_created)
    }
    
    ###### Status check ######
    # Low level function to get status, this function
    get_check_level = function(){
      if(!utils$has_subject()){
        checklevel = 0
      }else{
        checklevel = utils$get_from_subject('checklevel', 0, customized = T)
      }
      return(checklevel)
    }
    
    has_raw_cache = function(){
      utils$get_check_level() >= HAS_CACHE
    }
    
    has_subject = function(){
      !is.null(env[['subject']]) && inherits(env[['subject']], 'SubjectInfo2')
    }
    notch_filtered = function(){
      utils$get_check_level() >= NOTCH_FILTERED
    }
    waveleted = function(){
      utils$get_check_level() >= WAVELETED
    }
    
    ###### Get/set Subject Info ######
    # Get subject ID
    get_subject_id = function(){
      subject_code = get_val(env$subject, 'subject_code', default = '')
      project_name = get_val(env$subject, 'project_name', default = '')
      return(paste0(project_name , '/' , subject_code))
    }
    # Basic and low level function to get from subject (pre-set or customized data)
    get_from_subject = function(key, default, customized = F){
      if(customized){
        logger = get_val(env$subject, 'logger', default = default, .invalids = 'null')
        if(inherits(logger, 'RAVEHistory')){
          logger$get_or_save(key, default, save = F)
        }else{
          return(default)
        }
      }else{
        get_val(env$subject, key, default = default)
      }
    }
    # Basic and low level function to set to subject (customized data only!)
    save_to_subject = function(...){
      if(utils$has_subject()){
        env$subject$logger$save(...)
        return(TRUE)
      }
      return(FALSE)
    }
    # Block, electrodes (to be consistent, all 'channels' are switched to "electrodes")
    # get/set vals
    get_blocks = function(){
      utils$get_from_subject('blocks', NULL)
    }
    set_blocks = function(blocks, notification = TRUE, force = FALSE){
      msg = 'Blocks set'
      type = 'message'
      on.exit({
        if(notification){
          utils$showNotification(msg, type = type)
        }
      })
      
      if(!utils$has_subject()){
        msg = 'Subject not loaded'; type = 'error'
        return(FALSE)
      }
      if(!setequal(env$subject$blocks, blocks)){
        if(!length(blocks)){
          msg = 'No block selected'; type = 'error'
          return(FALSE)
        }
        if(utils$has_raw_cache()){
          msg = 'Subject already imported and cached. Cannot change blocks, unless you remove current subject folder.'
          type = 'error'
          return(FALSE)
        }
        # now set blocks
        env$subject$set_blocks(blocks = blocks, force = force)
        env$subject$save(action = 'Set Blocks', message = paste(blocks, collapse = ', '))
        msg = 'Blocks set'
        type = 'message'
        return(TRUE)
      }
      notification = FALSE
      return(FALSE)
    }
    get_electrodes = function(name){
      if(missing(name)){
        return(utils$get_from_subject('channels', NULL))
      }else{
        return(utils$get_from_subject(name, NULL))
      }
    }
    set_electrodes = function(electrodes, name = 'channels', notification = TRUE){
      channels = electrodes
      is_set = FALSE
      msg = 'Electrodes set'
      type = 'message'
      on.exit({
        if(notification){
          utils$showNotification(msg, type = type)
        }
      })
      
      if(utils$has_subject()){
        if(is.character(channels)){
          if(length(channels) > 1){
            channels = as.integer(channels)
          }else{
            channels = dipsaus::parse_svec(channels)
          }
        }
        # checks
        if(name == 'channels' && length(channels) == 0){
          msg = 'No electrodes selected'
          type = 'error'
          return(FALSE)
        }
        if(!setequal(env$subject[[name]], channels)){
          if(name == 'channels' && utils$has_raw_cache()){
            msg = 'Subject already imported and cached. Cannot set electrodes, unless you remove current subject folder.'
            type = 'error'
            return(FALSE)
          }
          # set channels
          env$subject$set_channels(channels = channels, name = name)
          env$subject$save(action = paste0('Set Electrodes (' , name ,')'), message = dipsaus::deparse_svec(channels))
          msg = paste0('Electrodes set (' , name , ')')
          type = 'message'
          return(TRUE)
        }
      }
      notification = F
      return(FALSE)
    }
    get_srate = function(){
      utils$get_from_subject('srate', 1)
    }
    set_srate = function(srate, notification = T){
      is_set = FALSE
      msg = 'Sample rate is set'
      type = 'message'
      on.exit({
        if(notification){
          utils$showNotification(msg, type = type)
        }
      })
      if(utils$has_subject()){
        if(!(length(srate) && srate >= 1)){
          msg = 'Sample rate cannot be blank or less than 1'
          type = 'error'
          return(FALSE)
        }
        # srate = max(1, floor(srate))
        if(is.null(env$subject$srate) || env$subject$srate != srate){
          if(utils$notch_filtered()){
            msg = 'Notch filter already applied. Cannot change sample rate.'
            type = 'error'
            return(FALSE)
          }
          # set srate
          env$subject$srate = srate
          env$subject$save(action = 'Set Sample Rate', message = paste(srate))
          return(TRUE)
        }
      }
      notification = F
      return(FALSE)
    }
    
    metas = function(pattern){
      dirs = utils$get_from_subject('dirs')
      f = list.files(dirs$meta_dir, pattern = pattern)
      if(length(f)){
        e = stringr::str_match(f, pattern)[,2]
        return(e)
      }else{
        NULL
      }
    }
    epochs = function(){
      pattern = '^epoch_([a-zA-Z0-9]+)\\.[cC][sS][vV]$'
      metas(pattern)
    }
    references = function(){
      pattern = '^reference_([a-zA-Z0-9]+)\\.[cC][sS][vV]$'
      metas(pattern)
    }
    
    ###### Get/set Voltage, wavelet data ######
    ## Voltage
    collect_raw_voltage = function(force = F){
      if(!utils$has_subject()){
        return(FALSE)
      }
      dirs = utils$get_from_subject('dirs', list(), customized = F)
      file = file.path(dirs$preprocess_dir, 'raw_voltage.h5')
      msg = 'Raw voltage signal has already been cached.'
      type = 'message'
      on.exit(utils$showNotification(msg, type = type))
      if(force || !file.exists(file)){
        # load raw data
        cache_raw_voltage(
          project_name = utils$get_from_subject('project_name'),
          subject_code = utils$get_from_subject('subject_code'),
          blocks = utils$get_blocks(),
          electrodes = utils$get_electrodes()
        )
        
        # set subject meta data
        env$subject$logger$save(checklevel = HAS_CACHE)
        utils$showNotification('Subject imported from raw folder.')
      }
    }
    
    load_voltage = function(electrodes, blocks, raw = F, time = NULL){
      if(!utils$has_raw_cache()){
        return(NULL)
      }
      if(missing(blocks)){
        blocks = utils$get_blocks()
      }
      if(missing(electrodes)){
        electrodes = utils$get_electrodes()
      }else{
        electrodes = electrodes[electrodes %in% utils$get_electrodes()]
      }
      srate = utils$get_srate()
      # need to know the index
      ind = sapply(electrodes, function(e){which(e == utils$get_electrodes())})
      dirs = utils$get_from_subject('dirs', list(), customized = F)
      
      cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', electrodes))
      if(raw){
        group = '/raw/'
      }else{
        group = '/notch/'
      }
      
      sapply(blocks, function(block){
        t(sapply(cfile, function(file){
          d = load_h5(file, name = paste0(group , block))
          if(length(time) != 2){
            return(d[])
          }else{
            col_ind = seq(ceiling(time[1] * srate), floor(time[2] * srate), by = 1)
            d[col_ind]
          }
        })) ->
          d
      }, simplify = F, USE.NAMES = T) ->
        re
      re
    }
    
    load_wavelet = function(electrode, blocks, referenced = F){
      stopifnot2(utils$waveleted(), msg = 'Wavelet is needed. Run preprocess first.')
      # only load raw wavelet
      blocks %?<-% utils$get_blocks()
      dirs = utils$get_from_subject('dirs')
      fpower = file.path(dirs$channel_dir, 'power', sprintf('%d.h5', electrode))
      fphase = file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', electrode))
      prefix = ifelse(referenced, '/ref', '/raw')
      sapply(blocks, function(b){
        power = load_h5(fpower, name = sprintf('%s/power/%s', prefix, b), ram = T)
        phase = load_h5(fphase, name = sprintf('%s/phase/%s', prefix, b), ram = T)
        sqrt(power) * exp(1i * phase)
      }, simplify = F, USE.NAMES = T) ->
        re
      re
    }
    ###### pre-Process ######
    apply_notch = function(bandwidths = list(
      'default' = list(
        centers = c(60,120,180),
        widths = c(1,2,2)
      )
    )){
      
      electrodes = utils$get_electrodes()
      blocks = utils$get_blocks()
      srate = utils$get_srate()
      
      progress = progress('Notch filter in progress...', max = length(electrodes))
      on.exit({progress$close()})
      
      dirs = utils$get_from_subject('dirs', list(), customized = F)
      file = file.path(dirs$preprocess_dir, 'notch_voltage.h5')
      
      if(file.exists(file)){
        unlink(file)
      }
      
      lapply_async(seq_along(electrodes), function(ii){
        e = electrodes[[ii]]
        cfile = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e))
        # load from file
        for(block in blocks){
          bandwidths[[block]] %?<-% bandwidths[['default']]
          bands = bandwidths[[block]][['centers']]
          width = bandwidths[[block]][['widths']]
          s = load_h5(cfile, name = sprintf('/raw/%s', block), 
                      read_only = TRUE, ram = TRUE)
          v = notch_channel(s, sample_rate = srate, bands = bands, width = width)
          # Save filtered
          save_h5(as.vector(v), file = cfile, name = paste0('/notch/' , block), chunk = c(1024))
        }
        
      }, .packages = 'rave', .call_back = function(i){
        progress$inc(sprintf('Electrode %d', electrodes[[i]]))
      }) ->
        re
      
      # save subject data
      env$subject$logger$save(checklevel = NOTCH_FILTERED)
      
    }
    
    apply_wavelet = function(electrodes, target_srate, frequencies, wave_num, ncores = 4){
      fast_cache = rave_options('fast_cache')
      fast_cache %?<-% TRUE
      # vars
      blocks = utils$get_blocks()
      if(missing(electrodes)){
        electrodes = utils$get_electrodes()
      }
      srate = utils$get_srate()
      dirs = utils$get_from_subject('dirs', NULL, customized = F)
      if(!length(dirs)){
        utils$showNotification(msg = 'Fatal error: Cannot obtain subject directory information.', type = 'error')
        return()
      }
      
      compress_rate = srate / target_srate
      
      progress2 = progress('Block Progress', max = length(electrodes))
      progress1 = progress('Overall Progress', max = length(blocks))
      on.exit({
        progress2$close()
        progress1$close()
      })
      
      ### Prepare for the wavelet file. Since users may specify different sample rate, we need to remove files if they exists
      # 1. reference file
      # ref_file = file.path(dirs$channel_dir, 'reference', sprintf('ref_%s.h5', dipsaus::deparse_svec(electrodes)))
      # unlink(ref_file)
      # Create dir
      for(block in blocks){
        for(nm in c('raw', 'ref')){
          dir.create(file.path(dirs$channel_dir, 'cache', 'power', nm, block), recursive = T, showWarnings = F)
          dir.create(file.path(dirs$channel_dir, 'cache', 'phase', nm, block), recursive = T, showWarnings = F)
          dir.create(file.path(dirs$channel_dir, 'cache', 'voltage', nm, block), recursive = T, showWarnings = F)
        }
      }
      
      
      # 2. raw channel files and power/phase files
      lapply(electrodes, function(e){
        unlink(file.path(dirs$preprocess_dir, 'spectrum', sprintf('%d.h5', e)))
        unlink(file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)))
        unlink(file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', e)))
        unlink(file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)))
        for(block in blocks){
          unlink(file.path(dirs$channel_dir, 'cache', 'power', 'raw', block, sprintf('%d.fst', e)))
          unlink(file.path(dirs$channel_dir, 'cache', 'phase', 'raw', block, sprintf('%d.fst', e)))
          # unlink(file.path(dirs$channel_dir, 'cache', 'power', 'raw', block, sprintf('%d.fst', e)))
          # unlink(file.path(dirs$channel_dir, 'cache', 'power', 'raw', block, sprintf('%d.fst', e)))
        }
      })
      
      # load signals
      lapply(blocks, function(block){
        progress2$reset(detail = 'Initializing...')
        progress1$inc(paste0('Block - ' , block))
        # v = utils$load_voltage(electrodes = electrodes, blocks = block, raw = F)[[1]]
        
        lapply_async(seq_along(electrodes), function(ii){
          # This is electrode
          e = electrodes[[ii]]
          s = load_h5(
            file = file.path(dirs$preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e)),
            name = sprintf('/notch/%s', block),
            ram = T
          )
          
          
          re = wavelet(s, freqs = frequencies, srate = srate, wave_num = wave_num)
          
          # Subset coefficients to save space
          ind = floor(seq(1, ncol(re$phase), by = compress_rate))
          
          #
          coef = re$coef[, ind, drop = F]
          re$coef = NULL
          phase = re$phase[, ind, drop = F]
          re$phase = NULL
          power = re$power[, ind, drop = F]
          re$power = NULL
          rm(re)
          gc()
          
          # Save power and phase voltage
          # power
          save_h5(
            x = power,
            file = file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)),
            name = sprintf('/ref/power/%s', block),
            chunk = c(length(frequencies), 128),
            replace = T
          )
          save_h5(
            x = power,
            file = file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)),
            name = sprintf('/raw/power/%s', block),
            chunk = c(length(frequencies), 128),
            replace = T
          )
          save_h5(
            x = 'noref',
            file = file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e)),
            name = '/reference',
            chunk = 1,
            replace = T, size = 1000
          )
          # If fast_cache, cache power data to data/cache/power/raw/block
          if(fast_cache){
            # create fast cache to power
            power = as.data.frame(t(power))
            # write
            cache_file = file.path(dirs$channel_dir, 'cache', 'power', 'raw', block, sprintf('%d.fst', e))
            
            raveio::save_fst(power, cache_file, compress = 100)
          }
          
          
          # phase
          save_h5(
            x = phase,
            file = file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', e)),
            name = sprintf('/ref/phase/%s', block),
            chunk = c(length(frequencies), 128),
            replace = T
          )
          save_h5(
            x = phase,
            file = file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', e)),
            name = sprintf('/raw/phase/%s', block),
            chunk = c(length(frequencies), 128),
            replace = T
          )
          save_h5(
            x = 'noref',
            file = file.path(dirs$channel_dir, 'phase', sprintf('%d.h5', e)),
            name = '/reference',
            chunk = 1,
            replace = T, size = 1000
          )
          
          # If fast_cache, cache phase data to data/cache/phase/raw/block
          if(fast_cache){
            # create fast cache to phase
            phase = as.data.frame(t(phase))
            # write
            cache_file = file.path(dirs$channel_dir, 'cache', 'phase', 'raw', block, sprintf('%d.fst', e))
            
            raveio::save_fst(phase, cache_file, compress = 100)
          }
          
          # voltage
          save_h5(
            x = s,
            file = file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)),
            name = sprintf('/ref/voltage/%s', block),
            chunk = 1024,
            replace = T
          )
          save_h5(
            x = s,
            file = file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)),
            name = sprintf('/raw/voltage/%s', block),
            chunk = 1024,
            replace = T
          )
          save_h5(
            x = 'noref',
            file = file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e)),
            name = '/reference',
            chunk = 1,
            replace = T, size = 1000
          )
          # If fast_cache, cache phase data to data/cache/phase/raw/block
          if(fast_cache){
            # create fast cache to phase
            s = data.frame(V1 = s)
            # write
            cache_file = file.path(dirs$channel_dir, 'cache', 'voltage', 'raw', block, sprintf('%d.fst', e))
            
            raveio::save_fst(s, cache_file, compress = 100)
          }
          
          # # Raw complex coefficient
          # coef = c(coef, phase)
          # dim(coef) = c(dim(phase), 2)
          # file = file.path(dirs$preprocess_dir, 'spectrum', sprintf('%d.h5', e))
          # save_h5(coef, file = file, name = sprintf('/wavelet/coef/%s', block), chunk = c(length(frequencies), 128, 2))
          
          # Clean up
          vars = ls(all.names = T)
          vars = c(vars, 'vars')
          rm(list = vars)
          gc()
          return(NULL)
          
        }, .ncores = ncores, .call_back = function(i){
          progress2$inc(paste0('Electrode - ' , electrodes[i]))
        })
        invisible()
      })
      
      wavelet_log = env$subject$logger$get_or_save('wavelet_log', list(), save = F)
      wavelet_log[[length(wavelet_log) + 1]] = list(
        channels = electrodes, # soft-depricated, use electrodes instead
        electrodes = electrodes,
        target_srate = target_srate,
        frequencies = frequencies,
        wave_num = wave_num
      )
      env$subject$logger$save(wavelet_log = wavelet_log)
      
      # save subject data
      env$subject$logger$save(checklevel = WAVELETED)
      
      utils$gen_meta()
      
      
      # write a reference table to cache
      # safe_write_csv(data.frame(
      #   Electrode = rep(electrodes, length(blocks)),
      #   Block = rep(blocks, each = length(electrodes)),
      #   Reference = 'noref'
      # ), file.path(dirs$channel_dir, 'cache', 'cached_reference.csv'), row.names = F)
      safe_write_csv(data.frame(
        Electrode = electrodes,
        Reference = 'noref'
      ), file.path(dirs$channel_dir, 'cache', 'cached_reference.csv'), row.names = F)
      
      # write fast caches
      # if(isTRUE(fast_cache)){
      #   progress1$inc('Generating fast cache')
      #   progress2$reset(detail = 'Initializing...')
      #   utils$gen_cache(progress = progress2)
      # }
    }
    
    gen_cache = function(type = c('power', 'phase', 'voltage'), progress = NULL){
      if(!(utils$has_subject() && utils$waveleted())){
        return(FALSE)
      }
      
      inc = function(m){
        if(!is.null(progress)){
          progress$inc(m, amount = 0.5 / length(type))
        }else{
          catgl(m)
        }
      }
      
      electrodes = utils$get_electrodes()
      blocks = utils$get_blocks()
      subject_id = utils$get_subject_id()
      dirs = utils$get_from_subject('dirs')
      
      this_env = environment()
      future_list = list()
      save_to_file = function(await = FALSE){
        if(isTRUE(await) && length(future_list)){
          future::resolve(future_list)
        }else if(await && length(future_list)){
          await = min(await, length(future_list))
          future::resolve(future_list[seq_len(await)])
        }
        while (length(future_list) && future::resolved(future_list[[1]])) {
          v = future::value(future_list[[1]])
          e = v$e
          inc(sprintf('Caching electrode %d %s', e, this_env$data_type))
          v = do.call('rbind', v$v)
          raveio::save_fst(v, sprintf(this_env$target_file, e), compress = 100)
          this_env$future_list[[1]] = NULL
        }
        NULL
      }
      
      for(data_type in type){
        target_file = file.path(dirs$channel_dir, 'cache', data_type, 'raw', '%d.fst')
        path_format = file.path(dirs$channel_dir, data_type, '%d.h5')
        lapply(electrodes, function(e){
          inc(sprintf('Loading electrode %d (%s)', e, data_type))
          f = future::future({
            list(
              e = e,
              v = lapply(blocks, function(block){
                h5path = sprintf('/raw/%s/%s', data_type, block)
                orig = load_h5(sprintf(path_format, e), h5path)
                if(data_type == 'voltage'){
                  orig = as.data.frame(matrix(orig[], ncol = 1))
                }else{
                  orig = as.data.frame(t(orig[]), row.names = FALSE)
                }
              })
            )
          })
          this_env$future_list[[length(future_list) + 1]] = f
          save_to_file(FALSE)
          
        })
        save_to_file(await = TRUE)
      }
      
    }
    
    gen_meta = function(){
      
      if(utils$has_raw_cache()){
        dirs = utils$get_from_subject('dirs')
        project_name = utils$get_from_subject('project_name')
        subject_code = utils$get_from_subject('subject_code')
        
        #Electrodes
        es = utils$get_electrodes()
        save_meta(
          data = data.frame(Electrode = es),
          meta_type = 'electrodes',
          project_name = project_name,
          subject_code = subject_code
        )
      }
      
      if(utils$waveleted()){
        # Frequency
        wavelet_log = env$subject$logger$get_or_save('wavelet_log', list(), save = F)
        wavelet_log = wavelet_log[[length(wavelet_log)]]
        freq = data.frame(Frequency = wavelet_log$frequencies)
        save_meta(
          freq,
          meta_type = 'frequencies',
          project_name = project_name,
          subject_code = subject_code
        )
        
        # Time points
        srate = wavelet_log$target_srate
        e = wavelet_log$electrodes[1]
        file = file.path(dirs$channel_dir, 'power', sprintf('%d.h5', e))
        lapply(utils$get_blocks(), function(b){
          npts = dim(load_h5(file, sprintf('/raw/power/%s', b)))[2]
          data.frame(
            Block = b,
            Time = seq(0, by = 1 / srate, length.out = npts),
            stringsAsFactors = F
          )
        }) ->
          tm
        tm = do.call(rbind, tm)
        save_meta(
          tm,
          meta_type = 'time_points',
          project_name = project_name,
          subject_code = subject_code
        )
        
        
        # create reference_default.csv
        ref = data.frame(
          Electrode = es,
          Group = 'Default',
          Reference = 'noref',
          stringsAsFactors = F
        )
        safe_write_csv(ref, file.path(dirs$meta_dir, 'reference_default.csv'))
        
      }
      
      
      
    }
    ###### Visualizations ######
    
    
    
    
    
    
    
    ###### Friendly names ######
    create_subject = save_subject
    get_sample_rate = get_srate
    set_sample_rate = set_srate
  }, envir = utils)
  
  
  
  return(utils)
}




