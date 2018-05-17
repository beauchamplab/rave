#' @export
rave_preprocess_tools <- function(env = new.env(), ...){
  env$subject = list()

  NO_SUBJECT = 0
  HAS_SUBJECT = 1
  NOTCH_FILTERED = 2
  WAVELETED = 3
  REFERENCED = 4

  utils = list(

    showNotification = function(msg, type = 'message'){
      session = getDefaultReactiveDomain()
      if(is.null(session)){
        level = switch (
          type,
          'error' = 'ERROR',
          'warning' = 'WARNING',
          'message' = 'INFO',
          'success' = 'INFO',
          type
        )
        logger(msg, level = level)
      }else{
        shiny::showNotification(p(HTML(paste(msg, collapse = '<br/>'))), type = type)
      }
    },

    check_load_subject = function(subject_code, project_name, details = T){
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
      }, simplify = T, USE.NAMES = F) ->
        l
      checklist[['Folders']] = l

      # step 2: Check if subject has preprocessed yet
      l = list(
        'Started Proprocess' = F,
        'Notch Filter' = F,
        'Wavelet' = F,
        'Referenced' = F
      )
      try({
        utils$load_subject(subject_code = subject_code, project_name = project_name)
        l[seq_len(utils$get_check_level())] = T
      })

      checklist[['Preprocess']] = l


      # step 3: Check if subject meta data is ready
      meta_dir = dirs$meta_dir
      meta_files = list.files(meta_dir)

      l = list(
        'Trial File' = 'trials.csv' %in% meta_files,
        'Electrode File' = 'electrodes.csv' %in% meta_files,
        'Time point File' = 'time_points.csv' %in% meta_files,
        'Frequency File' = 'frequencies.csv' %in% meta_files,
        'Epoch File' = any(str_detect(meta_files, '^epoch_[a-zA-Z0-9]+\\.[cC][sS][vV]$'))
      )

      checklist[['Meta']] = l

      checklist
    },

    # Load subject
    load_subject = function(subject_code, project_name){
      logger('Loading Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      assert_that(dir.exists(dirs$preprocess_dir), msg = 'Subject ' %&% subject_code %&% ' has no project folder ' %&% project_name)
      s = SubjectInfo2$new(project_name = project_name, subject_code = subject_code)
      id = s$project_name %&% '/' %&% s$subject_code

      env[['subject']] = s

      logger('Loaded Subject: ' %&% id)
    },

    has_subject = function(){
      !is.null(env[['subject']]) && is(env[['subject']], 'SubjectInfo2')
    },

    notch_filtered = function(){
      utils$get_check_level() >= NOTCH_FILTERED
    },

    waveleted = function(){
      utils$get_check_level() >= WAVELETED
    },


    # Save subject
    save_subject = function(subject_code, project_name){
      logger('Saving Subject')
      dirs = get_dir(subject_code = subject_code, project_name = project_name)
      is_created = FALSE
      if(!dir.exists(dirs$preprocess_dir)){
        dirs = get_dir(subject_code = subject_code, project_name = project_name, mkdirs = c('preprocess_dir', 'cache_dir', 'meta_dir'))
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

      logger('saved Subject')
      return(is_created)
    },

    # Get subject ID
    get_subject_id = function(){
      subject_code = get_val(env$subject, 'subject_code', default = '')
      project_name = get_val(env$subject, 'project_name', default = '')
      return(project_name %&% '/' %&% subject_code)
    },

    load_signal = function(block, channel, group){
      dirs = utils$get_from_subject('dirs', NULL)
      if(length(dirs)){
        dir = dirs$preprocess_dir
        channel = as.integer(channel)
        f = file.path(dir, sprintf('chl_%d.h5', channel))
        name = '/' %&% group %&% '/' %&% block
        name = str_replace_all(name, '[/]+', '/')
        tryCatch({
          d = load_h5(f, name)
          d[]
        }, error = function(e){
          NULL
        }) ->
          re
        return(re)

      }else{
        return(NULL)
      }
    },

    get_channel_type = function(chl, as_text = F){
      if(length(chl) > 1){
        return(sapply(chl, utils$get_channel_type, as_text = as_text))
      }
      CHANNEL_TYPES = c(
        'Good',
        'Epilepsy',
        'Bad',
        'Exclude'
      )
      i = 1
      if(chl %in% utils$get_channels('epichan')){
        i = 2
      }else if(chl %in% utils$get_channels('badchan')){
        i = 3
      }else if(chl %in% utils$get_channels('exclchan')){
        i = 4
      }
      if(as_text){
        return(CHANNEL_TYPES[i])
      }else{
        return(i)
      }
    },

    # Get data from subject
    get_from_subject = function(key, default, customized = F){
      if(customized){
        logger = get_val(env$subject, 'logger', default = default, .invalids = 'null')
        if(is(logger, 'RAVEHistory')){
          logger$get_or_save(key, default, save = F)
        }else{
          return(default)
        }
      }else{
        get_val(env$subject, key, default = default)
      }
    },

    save_to_subject = function(...){
      if(utils$has_subject()){
        env$subject$logger$save(...)
        return(TRUE)
      }
      return(FALSE)
    },

    get_check_level = function(){
      if(!utils$has_subject()){
        checklevel = 0
      }else{
        checklevel = utils$get_from_subject('checklevel', 1, customized = T)
      }
      return(checklevel)
    },

    # get/set vals
    get_blocks = function(){
      utils$get_from_subject('blocks', NULL)
    },
    set_blocks = function(blocks, notification = T){
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
        if(utils$notch_filtered()){
          msg = 'Notch filter already applied. Cannot change blocks.'
          type = 'error'
          return(FALSE)
        }
        # now set blocks
        env$subject$set_blocks(blocks = blocks)
        env$subject$save(action = 'Set Blocks', message = paste(blocks, collapse = ', '))
        msg = 'Blocks set'
        type = 'message'
        return(TRUE)
      }
      notification = F
      return(FALSE)
    },
    get_channels = function(name){
      if(missing(name)){
        return(utils$get_from_subject('channels', NULL))
      }else{
        return(utils$get_from_subject(name, NULL))
      }
    },
    set_channels = function(channels, name, notification = TRUE){
      is_set = FALSE
      msg = 'Channels set'
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
            channels = rave:::parse_selections(channels)
          }
        }
        # TODO: check if all channels exist
        # checks
        if(name == 'channels' && length(channels) == 0){
          msg = 'No channel selected'
          type = 'error'
          return(FALSE)
        }
        if(!setequal(env$subject[[name]], channels)){
          if(name == 'channels' && utils$notch_filtered()){
            msg = 'Notch filter already applied. Cannot change channels.'
            type = 'error'
            return(FALSE)
          }
          # set channels
          env$subject$set_channels(channels = channels, name = name)
          env$subject$save(action = 'Set Channels (' %&% name %&%')', message = rave:::deparse_selections(channels))
          msg = 'Channels set (' %&% name %&% ')'
          type = 'message'
          return(TRUE)
        }
      }
      notification = F
      return(FALSE)
    },
    add_to_channel = function(chl, which, notification = F){
      if(which == 1){
        return(FALSE)
      }
      chl_types = c('', 'epichan', 'badchan', 'exclchan')
      old_type = utils$get_channel_type(chl)
      if(which != old_type){
        if(old_type != 1){
          name = chl_types[old_type]
          new_chls = utils$get_channels(name)
          new_chls = new_chls[new_chls != chl]
          utils$set_channels(new_chls, name = name, notification = notification)
        }
        if(which > 1){
          name = chl_types[which]
          new_chls = utils$get_channels(name)
          new_chls = c(new_chls, chl)
          utils$set_channels(new_chls, name = name, notification = notification)
        }
      }
    },
    get_srate = function(){
      utils$get_from_subject('srate', 1)
    },
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
        srate = max(1, floor(srate))
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
    },

    get_excluded_time = function(block){

      tbl = utils$get_from_subject(key = 'excluded_time', default = data.frame(), customized = T)
      if(!is.data.frame(tbl)){
        tbl = as.data.frame(tbl, stringsAsFactors = F)
      }
      if(missing(block)){
        return(tbl)
      }
      # tbl: Block, Channel, Start, End
      tbl = tbl[tbl$Block %in% block, ]
      if(nrow(tbl)){
        tbl$Staged = TRUE
        tbl = tbl[, c('Start', 'End', 'Staged')]
      }
      tbl
    },

    add_excluded_time = function(block, start, end, local_data = NULL){
      tbl = utils$get_excluded_time()
      tbl = rbind(
        tbl,
        data.frame(
          Block = block,
          Start = start,
          End = end,
          stringsAsFactors = F
        )
      )
      if(utils$has_subject()){
        utils$save_to_subject(excluded_time = tbl)
      }

      if(!is.null(local_data)){
        local_data$refresh_excluded_time = Sys.time()
      }
    },

    set_excluded_time = function(new_tbl, block, local_data = NULL){
      tbl = utils$get_excluded_time()
      tbl = tbl[!(tbl$Block %in% block), ]
      if(nrow(new_tbl)){
        new_tbl$Block = block
        new_tbl = new_tbl[, c('Block', 'Start', 'End')]
        tbl = rbind(new_tbl, tbl[, c('Block', 'Start', 'End')])
      }
      if(utils$has_subject()){
        utils$save_to_subject(excluded_time = tbl)
      }
      if(!is.null(local_data)){
        local_data$refresh_excluded_time = Sys.time()
      }
    },

    get_reference_table = function(){
      tbl = as.data.frame(utils$get_from_subject(key = 'reference_table', default = data.frame(), customized = T), stringsAsFactors = F)
      # check
      if(utils$has_subject() && !setequal(channels <- utils$get_channels(), tbl$Channel)){
        # get whatever we have
        tbl = tbl[tbl$Channel %in% channels, ]
        new_channel = channels[!channels %in% tbl$Channel]
        if(length(new_channel)){
          new_tbl = data.frame(
            Channel = new_channel,
            ChlType = 1,
            Group = '',
            Reference = '',
            RefType = '',
            stringsAsFactors = F
          )
          tbl = rbind(tbl, new_tbl)
        }
        tbl = tbl[order(tbl$Channel), ]
        tbl$ChlType = sapply(tbl$Channel, utils$get_channel_type)
      }

      tbl
    },
    save_reference_table = function(tbl){
      assert_that(sum(c('Channel', 'Group', 'Reference', 'RefType') %in% names(tbl)) == 4, msg = "Invalid reference table, MUST have column: 'Channel', 'Group', 'Reference', 'RefType'")
      assert_that(setequal(utils$get_channels(), tbl$Channel), msg = 'Invalid row count, MUST equal to # of channels')
      if(nrow(tbl)){
        tbl$ChlType = sapply(tbl$Channel, utils$get_channel_type)
      }
      utils$save_to_subject(reference_table = tbl)
    },
    set_reference_group = function(g_name, g_type, g_channels,
                                   g_epi = NULL, g_bad = NULL, g_excl = NULL,
                                   bp_e1 = NULL, bp_e2 = NULL, init = F){
      # Channel = new_channel,
      # ChlType = sapply(new_channel, utils$get_channel_type),
      # Group = '',
      # Reference = '',
      # RefType = '',
      ref_table = utils$get_reference_table()
      g_channels = g_channels[g_channels %in% utils$get_channels()]
      g_type = g_type[1]
      # 0 check if conflict with others
      sel = (ref_table$Channel %in% g_channels) & (!ref_table$Group %in% c('', g_name))
      if(sum(sel)){
        cg = paste(unique(ref_table$Group[sel]), collapse = ', ')
        session = getDefaultReactiveDomain()
        msg = c('Your group overlaps with the other groups (' %&% cg %&% ') ',
                'Please revise your channels within these groups.')
        type = 'error'
        utils$showNotification(msg, type = type)
        return(FALSE)
      }

      # 1 remove channels
      sel = ref_table$Group == g_name & !ref_table$Channel %in% g_channels
      if(sum(sel)) {
        ref_table[sel, 'Group'] = ''
        ref_table[sel, 'RefType'] = ''
        ref_table[sel, 'Reference'] = ''
      }

      # 2 Add channels
      sel = ref_table$Channel %in% g_channels
      if(g_type %in% c('Common Avg/White Matter Reference', 'Bi-polar Reference')){
        reftype = g_type
      }else{
        reftype = switch(
          g_type,
          'car' = 'Common Avg/White Matter Reference',
          'bipolar' = 'Bi-polar Reference'
        )
      }

      if(sum(sel)) {
        # 3 reset channel types
        if(!init){
          g_epi = g_epi[g_epi %in% g_channels]
          epi = utils$get_channels('epichan')
          epi = c(epi[!epi %in% g_channels], g_epi)
          utils$set_channels(epi, 'epichan', notification = FALSE)

          g_bad = g_bad[g_bad %in% g_channels]
          bad = utils$get_channels('badchan')
          bad = c(bad[!bad %in% g_channels], g_bad)
          utils$set_channels(bad, 'badchan', notification = FALSE)

          g_excl = g_excl[g_excl %in% g_channels]
          excl = utils$get_channels('exclchan')
          excl = c(excl[!excl %in% g_channels], g_excl)
          utils$set_channels(excl, 'exclchan', notification = FALSE)
        }

        ref_table[sel, 'Group'] = g_name
        ref_table[sel, 'RefType'] = reftype
        # 4 set references
        if(g_type == 'car'){
          ref_chans = g_channels[!g_channels %in% c(g_epi, g_bad, g_excl)]
          if(length(ref_chans)){
            ref_table[sel, 'Reference'] = 'CAR/WMR, ' %&% rave:::deparse_selections(ref_chans)
          }else{
            ref_table[sel, 'Reference'] = 'No CAR Reference'
          }
        }else if (g_type == 'bipolar'){
          e1 = as.numeric(bp_e1)
          ref_chans = as.numeric(bp_e2)
          sel_e1 = (ref_table$Channel %in% e1) & sel
          sel_ref = (ref_table$Channel %in% ref_chans) & sel
          if(sum(sel_e1)){
            # set reference for BP
            if(sum(sel_ref)){
              ref_table[sel_e1, 'Reference'] = 'Bipolar, ' %&% rave:::deparse_selections(ref_chans)
            }else{
              ref_table[sel_e1, 'Reference'] = 'No Bipolar Reference'
            }
          }

          sel_blank = sel & is.blank(ref_table$Reference)
          if(sum(sel_blank)){
            ref_table[sel_blank, 'Reference'] = 'No Bipolar Reference'
          }
        }
      }


      utils$save_reference_table(ref_table)

      return(TRUE)
    },

    cache_reference = function(force = F){
      if(!utils$has_subject() || !utils$notch_filtered()){
        return()
      }
      preprocess_dir = utils$get_from_subject('dirs', list())$preprocess_dir
      ref_file = file.path(preprocess_dir, 'reference_cache.h5')
      if(file.exists(ref_file)) {
        if(force){
          file.remove(ref_file)
        }else{
          return()
        }
      }
      utils$showNotification('Will take a little bit while to cache...', type = 'message')
      blocks = utils$get_blocks()
      channels = utils$get_channels()

      proc = rave:::progress(title = 'Cache Signals', max = (1 + length(channels)) * length(blocks))
      on.exit({proc$close()})

      for(block in blocks){
        name = sprintf('/notch/%s', block)
        signals = lapply_async(channels, function(chl){
          fname = file.path(preprocess_dir, sprintf('chl_%d.h5', chl))
          s = load_h5(fname, name)
          s[]
        }, .call_back = function(i){
          proc$inc(sprintf('Loading block %s, channel %d', block, channels[i]))
        })

        H5close()
        proc$inc(sprintf('Cache block %s', block))

        ## save h5 to file
        signals = t(sapply(signals, base::I))
        save_h5(signals, ref_file, name = name, chunk = c(1, 1024), new_file = F, replace = T)
        H5close()
      }

    },
    get_reference_cache_file = function(auto_cache = F){
      preprocess_dir = utils$get_from_subject('dirs', list())$preprocess_dir
      ref_file = file.path(preprocess_dir, 'reference_cache.h5')
      if(auto_cache && !file.exists(ref_file)){
        utils$cache_reference(force = F)
      }
      return(ref_file)
    },

    load_compressed_notch_signal = function(block, channels = NULL, from, to, compress_level = 20){
      if(utils$notch_filtered()){
        ref_file = utils$get_reference_cache_file(auto_cache = T)

        signals = load_h5(ref_file, name = sprintf('/notch/%s', block))

        # load and subset
        srate = utils$get_srate()
        max_ind = dim(signals)[2]
        from_ind = round(from * srate) + 1
        to_ind = round(to * srate)

        if(from_ind >= max_ind - 2 || to_ind < 2){
          return(NULL)
        }

        to_ind = min(to_ind, max_ind)

        col_ind = seq(from_ind, to_ind, by = compress_level)
        if(!is.null(channels)){
          all_channels = utils$get_channels()
          row_ind = sapply(channels, function(i){
            which(i == all_channels)
          })

          return(signals[row_ind, col_ind])
        }else{

          # set masks

          return(signals[, col_ind])
        }
      }else{
        return(NULL)
      }

    },

    # save_meta_electrodes = function(){
    #   Channel = env$subject$channels
    #   rave:::save_meta(
    #     data = meta_elec,
    #     meta_type = 'electrodes',
    #     project_name = env$subject$project_name,
    #     subject_code = env$subject$subject_code
    #   )
    # },

    # Apply Notch filters
    apply_notch = function(centers, widths){
      # create subject electrode data

      bulk_notch2(
        project_name = utils$get_from_subject('project_name'),
        subject_code = utils$get_from_subject('subject_code'),
        blocks = utils$get_from_subject('blocks'),
        channels = utils$get_from_subject('channels'),
        srate = utils$get_from_subject('srate')
      )
      # save subject data
      env$subject$logger$save(checklevel = NOTCH_FILTERED)
      # cache for reference
      utils$cache_reference(force = T)
    },

    # apply wavelets
    apply_wavelet = function(channels, target_srate, frequencies, wave_num, ncores){
      rave:::bulk_wavelet(
        project_name = utils$get_from_subject('project_name'),
        subject_code = utils$get_from_subject('subject_code'),
        blocks = utils$get_from_subject('blocks'),
        channels = channels,
        srate = utils$get_srate(),
        target_srate = target_srate,
        frequencies = frequencies,
        wave_num = wave_num,
        compress = 1, replace = T,
        save_original = F,
        ncores = ncores, plan = NULL,
        save_dir = 'preprocess_dir', filename = 'chl_%d_wavelet.h5',
        reference_name = 'notch'
      )

      wavelet_log = env$subject$logger$get_or_save('wavelet_log', list(), save = F)
      wavelet_log[[length(wavelet_log) + 1]] = list(
        channels = channels,
        target_srate = target_srate,
        frequencies = frequencies,
        wave_num = wave_num
      )
      env$subject$logger$save(wavelet_log = wavelet_log)

      # save subject data
      env$subject$logger$save(checklevel = WAVELETED)
    },

    load_epoch_time = function(epoch_name){
      tryCatch({
        epoch = load_meta(meta_type = 'epoch',
                  subject_id = utils$get_subject_id(),
                  meta_name = epoch_name)
        sapply(unique(epoch$Block), function(b){
          epoch$Time[epoch$Block == b]
        }, simplify = F, USE.NAMES = T)
      }, error = function(e){
        list()
      })
    },

    get_epochs = function(){
      dirs = utils$get_from_subject('dirs')
      fs = list.files(dirs$meta_dir, pattern = '^epoch_.+.csv$')
      if(length(fs)){
        return(str_match(fs, '^epoch_(.+).csv')[,2])
      }else{
        NULL
      }
    },

    get_wavelet_log = function(ind = -1){
      if(utils$get_check_level() < 3){
        return(NULL)
      }
      wavelet_log = utils$get_from_subject('wavelet_log', list(), T)
      if(ind == -1){
        return(wavelet_log[[length(wavelet_log)]])
      }else{
        return(wavelet_log[ind][[1]])
      }
    },


    load_wave_data = function(block, chl, complex = F, raw = T, time_points = NULL){
      dirs = utils$get_from_subject('dirs', list(), F)
      if(raw){
        wave_file = file.path(dirs$preprocess_dir, sprintf('chl_%d_wavelet.h5', chl))
      }else{
        wave_file = file.path(dirs$cache_dir, sprintf('%d.h5', chl))
      }
      if(length(time_points) == 0){  # no need, but this can speedup process
        coef = load_h5(wave_file, '/wavelet/coef/' %&% block)[]
        phase = load_h5(wave_file, '/wavelet/phase/' %&% block)[]
      }else{
        coef = load_h5(wave_file, '/wavelet/coef/' %&% block)[, time_points]
        phase = load_h5(wave_file, '/wavelet/phase/' %&% block)[, time_points]
      }

      if(complex){
        re = coef * exp(1i * phase)
      }else{
        re = list(
          coef = coef,
          phase = phase
        )
      }
    },



    get_final_file = function(chl){
      dirs = utils$get_from_subject('dirs', list(), F)
      file.path(dirs$cache_dir, sprintf('%d.h5', chl))
    },

    save_final_data = function(b, chl, complex_data){
      cfile = utils$get_final_file(chl)
      coef = Mod(complex_data)
      phase = Arg(complex_data)
      power = coef^2
      cumsum = t(apply(power,1,cumsum))
      save_h5(coef, file = cfile, name = 'wavelet/coef/' %&% b, chunk = c(1, 1024))
      save_h5(phase, file = cfile, name = 'wavelet/phase/' %&% b, chunk = c(1, 1024))
      save_h5(power, file = cfile, name = 'wavelet/power/' %&% b, chunk = c(1, 1024))
      save_h5(cumsum, file = cfile, name = 'wavelet/cumsum/' %&% b, chunk = c(1, 1024))
      rhdf5::H5close()
    },

    load_volt_data = function(block, chl){
      dirs = utils$get_from_subject('dirs', list(), F)
      vfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
      volt = load_h5(vfile, '/notch/' %&% block)[]
      volt
    },

    save_final_volt = function(b, chl, volt_data){
      dirs = utils$get_from_subject('dirs', list(), F)
      vfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
      save_h5(volt_data, file = vfile, name = '/REF/' %&% b, chunk = 1024, replace = T)
      rhdf5::H5close()
    },


    # reference_channels
    apply_ref = function(){
      tbl = utils$get_reference_table()
      dirs = utils$get_from_subject('dirs', list(), F)
      blocks = utils$get_blocks()
      channels = utils$get_channels()
      chnum = max(channels)

      if(length(tbl) == 0 || nrow(tbl) == 0){
        utils$showNotification('Cannot find reference table', type = 'error')
      }

      # generate channels for references
      progress = rave:::progress('Reference Channels', max = chnum + 2) # Dipterix you are sooo lazy!
      on.exit({progress$close()})

      ref_list = list()

      sel = tbl$RefType %in% c('Common Avg/White Matter Reference')
      if(sum(sel)){
        ref_unique = unique(tbl[sel, c('Group', 'Reference')])
        for(ii in 1:nrow(ref_unique)){

          gname = ref_unique[ii, 'Group']
          refs = ref_unique[ii, 'Reference']

          refs = rave:::parse_selections(refs)
          if(!length(refs)){
            next
          }
          progress$inc('Generating CAR reference - ' %&% gname)
          sub_progress = rave:::progress('Generating Reference Signal - ' %&% gname, max = length(blocks) * length(refs))
          nref_chls = length(refs)
          chnum = chnum + 1
          cfile = utils$get_final_file(chnum)
          if(file.exists(cfile)){
            unlink(cfile, force = T)
          }
          ref_list[[gname]] = list(num = chnum, ref_data = list())
          # load data
          for(b in blocks){

            ref_data = NULL
            ref_volt = NULL

            for(chl in refs){
              sub_progress$inc('Block - ' %&% b %&% ", Channel - " %&% chl)
              dat = utils$load_wave_data(b, chl, complex = T) / nref_chls
              volt = utils$load_signal(b, chl, group = 'notch') / nref_chls
              if(is.null(ref_data)){
                ref_data = dat
                ref_volt = volt
              }else{
                ref_data = ref_data + dat
                ref_volt = ref_volt + volt
              }
            }

            # save ref_data
            utils$save_final_data(b, chnum, ref_data)
            utils$save_final_volt(b, chnum, ref_volt)
            ref_list[[gname]][['ref_data']][[b]] = ref_data
            ref_list[[gname]][['ref_volt']][[b]] = ref_volt
          }

          sub_progress$close()
        }
      }


      lapply_async(1:nrow(tbl), function(ii){
        chl = tbl[ii, 'Channel']
        ref = rave:::parse_selections(tbl[ii, 'Reference'])
        gname = tbl[ii, 'Group']

        # light weight for async functions
        load_wave_data = function(block, chl, complex = T){
          wave_file = file.path(dirs$preprocess_dir, sprintf('chl_%d_wavelet.h5', chl))
          coef = load_h5(wave_file, '/wavelet/coef/' %&% b)[]
          phase = load_h5(wave_file, '/wavelet/phase/' %&% b)[]
          if(complex){
            re = coef * exp(1i * phase)
          }else{
            re = list(
              coef = coef,
              phase = phase
            )
          }
        }

        get_final_file = function(chl){
          file.path(dirs$cache_dir, sprintf('%d.h5', chl))
        }

        save_final_data = function(b, chl, complex_data){
          cfile = get_final_file(chl)
          coef = Mod(complex_data)
          phase = Arg(complex_data)
          power = coef^2
          cumsum = t(apply(power,1,cumsum))
          save_h5(coef, file = cfile, name = 'wavelet/coef/' %&% b, chunk = c(1, 1024))
          save_h5(phase, file = cfile, name = 'wavelet/phase/' %&% b, chunk = c(1, 1024))
          save_h5(power, file = cfile, name = 'wavelet/power/' %&% b, chunk = c(1, 1024))
          save_h5(cumsum, file = cfile, name = 'wavelet/cumsum/' %&% b, chunk = c(1, 1024))
          rhdf5::H5close()
        }

        load_volt_data = function(block, chl){
          vfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
          volt = load_h5(vfile, '/notch/' %&% block)[]
          volt
        }

        save_final_volt = function(b, chl, volt_data){
          vfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))
          save_h5(volt_data, file = vfile, name = '/REF/' %&% b, chunk = c(1, 1024), replace = T)
          rhdf5::H5close()
        }

        # check if file exists if so delete
        final_file = get_final_file(chl)
        if(file.exists(final_file)){
          unlink(final_file)
        }


        for(b in blocks){
          # get chl
          e1 = load_wave_data(b, chl, complex = T)
          v1 = load_volt_data(b, chl)
          # get ref
          e2 = NULL
          if(length(ref)){
            if(gname %in% names(ref_list)){
              e2 = ref_list[[gname]][['ref_data']][[b]]
              v2 = ref_list[[gname]][['ref_volt']][[b]]
            }else{
              # must be Bipolar
              e2 = load_wave_data(b, ref[1], complex = T)
              v2 = load_volt_data(b, ref[1])
            }

            # if no reference by default /REF/block is original signal
            save_final_volt(b, chl, v1-v2)
          }
          if(!is.null(e2)){
            e1 = e1 - e2
          }

          save_final_data(b, chl, e1)
        }


      }, .call_back = function(i){
        progress$inc(sprintf('Saving channel %d' , tbl$Channel[i]))
      })

      addition_rows = NULL
      for(gname in names(ref_list)){
        addition_rows = rbind(addition_rows,
                              data.frame(
                                Channel = ref_list[[gname]]$num,
                                ChlType = 1,
                                Group = gname,
                                Reference = 'This is a reference signal',
                                RefType = ''
                              ))
      }

      utils$save_channel_meta(
        addition_rows = addition_rows
      )

      utils$save_excltime_meta()

      env$subject$logger$save(checklevel = REFERENCED)

    },

    save_excltime_meta = function(){
      tbl = as.data.frame(utils$get_from_subject('excluded_time', data.frame(), T), stringsAsFactors = F)
      if(nrow(tbl)){
        tbl = tbl[, c('Block', 'Start', 'End')]
        save_meta(tbl, 'time_excluded',
                  project_name = utils$get_from_subject('project_name'),
                  subject_code = utils$get_from_subject('subject_code'))
      }

    },

    plot_channels = function(
      block, sub_channels = NULL,
      from, duration, space, col, new_plot = T,
      subset = NULL, space_mode = NULL, legend = FALSE, mat = NULL, show_mean = F){

      # # Debug values
      # block = '008'
      # sub_channels = NULL
      # from = 0
      # duration = 10
      # space = 0.995
      # col = 1
      # new_plot = T
      # subset = NULL
      # legend = FALSE
      # mat = NULL
      # show_mean = F

      if(is.null(env$signal_start) ||
         from < env$signal_start ||
         (from + duration) > (env$signal_start + 50) ||
         env$current_block != block){
        env$signals = utils$load_compressed_notch_signal(block = block, from = from, to = from + 50, compress_level = 1)
        env$signal_start = from
        env$current_block = block
      }
      space_mode %?<-% ifelse(space <= 1, 'quantile', 'abs')
      channels = utils$get_channels()
      nchls = length(channels)
      s = env$signals
      srate = utils$get_srate()

      # color
      col_env = to_color(col, default_length = nchls, palette = NULL, shift = 2)
      env$col_env = col_env


      if(missing(sub_channels) || length(sub_channels) == 0){
        sub_channels %?<-% rep(TRUE, nchls)
      }else{
        sub_channels = channels %in% sub_channels
      }

      if(length(subset) == 0){
        subset = sub_channels
      }else{
        subset = (channels %in% subset) & sub_channels
      }

      # operate on mat
      junk = rep(0, nchls)
      pre_mat = t(sapply(which(subset), function(i){
        junk[i] = 1
        junk
      }))

      colors = col_env$colors
      if(length(colors) == nchls){
        colors = colors[subset]
      }

      palette = col_env$palette
      channel_names = channels[subset]

      if(show_mean){
        pre_mat = rbind(sub_channels / sum(sub_channels), pre_mat)
        colors = c("#FF0000FF", colors) # add red
        palette = rbind(c("#FF0000FF", 'Average'), palette)
        channel_names = c('Average', channel_names)
      }

      if(is.matrix(mat) && ncol(mat) == nrow(s)){
        if(ncol(pre_mat) == nrow(mat)){
          pre_mat = pre_mat %*% mat
        }else if(nrow(mat) == sum(subset)){
          pre_mat = pre_mat[, subset] %*% mat
        }
      }

      print((pre_mat))
      print(dim(s))
      s = pre_mat %*% s

      re = plot_signals(
        signals = s,
        sample_rate = srate,
        space = space,
        space_mode = space_mode,
        col = colors,
        start_time = from - env$signal_start, time_shift = env$signal_start,
        duration = duration, compress = TRUE,
        channel_names = channel_names, ylab = 'Channel',
        new_plot = new_plot
      )

      # draw excluded time
      excluded_time = utils$get_excluded_time()
      excluded_time = excluded_time[excluded_time$Block %in% block, ]

      if(nrow(excluded_time)){
        start = excluded_time$Start
        end = excluded_time$End
        ymax = re$space * nrow(s)

        apply(cbind(start, end), 1, function(x){
          rect(x[1], re$space, x[2], ymax, lwd = 3, lty = 2)
          NULL
        })

      }

      if(legend){
        col_txt = palette[,1]
        col_txt[col_txt == ''] = 'No Group'
        legend('topright', col_txt, col = palette[,2], lty = 1)
      }

      return(re)
    },

    save_channel_meta = function(addition_rows = NULL){
      progress = rave:::progress('Save meta files...', max = 5)
      on.exit({progress$close()})
      # channels
      ref_table = utils$get_reference_table()
      ref_table = rbind(ref_table, addition_rows)
      ref_table = ref_table[order(ref_table$Channel), ]

      electrodes = data.frame(
        Channel = ref_table$Channel,
        EpilepsyChan = ref_table$ChlType == 2,
        BadChan = ref_table$ChlType == 3,
        ExcludedChan = ref_table$ChlType == 4,
        Group = ref_table$Group,
        Reference = ref_table$Reference,
        RefType = ref_table$RefType,
        stringsAsFactors = F
      )
      save_meta(electrodes, 'electrodes',
                project_name = utils$get_from_subject('project_name'),
                subject_code = utils$get_from_subject('subject_code'))
    },

    epoch_block = function(block, chl, epoch, srate, pre, post, raw = F, func = I){
      epoch_info = load_meta('epoch', subject_id = utils$get_subject_id(), meta_name = epoch)
      epoch_info = epoch_info$Time[epoch_info$Block == block]
      time = seq(-pre, post, by = 1/srate)
      dat = utils$load_wave_data(block, chl, complex = T, raw = raw)

      dat = rave:::dropNulls(lapply(epoch_info, function(tt){
        tryCatch({
          return(dat[, round((time + tt) * srate)])
        }, error = function(e){
          logger(e, level = 'WARNING')
          NULL
        })
      }))

      if(length(dat)){
        dat = vapply(dat, func, FUN.VALUE = func(dat[[1]]))
      }else{
        dat = NULL
      }
      dat
    },

    snapshot_power = function(power = NULL, block, chl, epoch, freq_range, pre=2, post=4, use_median = T, plot = T, cols = rave_palette(width = c(5, 35, 10)), ...){
      wave_log = utils$get_wavelet_log()
      freqs = wave_log$frequencies
      srate = wave_log$target_srate
      fs = freqs %within% freq_range

      assertthat::assert_that(any(fs), msg = 'Invalid frequency range')

      sel_f = freqs[fs]

      if(use_median){
        m = median
      }else{
        m = mean
      }

      power %?<-% utils$epoch_block(block, chl, epoch, srate, pre, post, raw = F, func = function(x){
        # power
        x = Re(x[fs, , drop = F]) ^ 2
        # Baseline
        x = apply(x, 1, function(xx){
          xx / m(xx) - 1
        })

        # Collapse
        rowMeans(x)
      })

      if(plot){
        Time = seq(-pre, post, by = 1/srate)

        image_plot(x = Time, y = seq_len(ncol(power)), z = power, ylab = 'Trial', xlab = 'Time (second)', las = 1, col = cols,
                   symmetric = T, useRaster = F, ...,
                   main = sprintf('Block %s, channel %d (Collapse %.0f-%.0f Hz)', block, as.integer(chl), min(sel_f), max(sel_f)))

      }

      invisible(power)
    },

    snapshot_phase = function(phase = NULL, block, chl, epoch, freq, pre=2, post=4, calc_diff = T, plot = T,
                              cols = rev(rave_palette(width = c(0.4, 0.2, 0.4))), crop = c(-0.5, 0.5)){
      wave_log = utils$get_wavelet_log()
      freqs = wave_log$frequencies
      srate = wave_log$target_srate
      fs = which.min(abs(freqs - freq))[1]

      sel_f = freqs[fs]

      phase %?<-% utils$epoch_block(block, chl, epoch, srate, pre, post, raw = F, func = function(x){
        # phase
        x = Arg(x[fs, ])
        # if(calc_diff){
        #   init = x[1]
        #   x = diff_arg(x, srate = srate, freq = sel_f)
        #   x = Arg(exp(1i * (init + cumsum(x))))
        # }
        x
      })

      if(plot){
        Time = seq(-pre, post, by = 1/srate)

        if(!calc_diff){
          main = sprintf('Block %s, channel %d (Phase)', block, as.integer(chl))
          rave::plot_signals(t(phase), sample_rate = srate, time_shift = -pre, space = 7, space_mode = 'abs', compress = F, ylab = 'Trial',
                             main = main)

          abline(v = 0, lty = 2, lwd = 4)
        }else{
          main = sprintf('Block %s, channel %d (Angle difference)', block, as.integer(chl))
          # image_plot(x = Time, y = seq_len(ncol(phase)), z = phase, ylab = 'Trial', xlab = 'Time (second)', las = 1, col = cols,
          #            symmetric = T, useRaster = F, crop = c(-1,1) * pi,
          #            main = main, panel.last = {
          #              abline(v = 0, lty = 2, lwd = 4)
          #            })

          dif = t(apply(phase, 1, function(x){
            xi = exp(1i* x)
            m = mean(xi)
            Mod(m - xi)
          }))

          # par(mfrow = c(2,2))
          # x = phase[1,];xi = exp(1i* x);m = mean(xi)
          # plot(xi, pch = 20, xlim = c(-1,1),ylim = c(-1,1), asp = 1, frame.plot = F, axes = F); points(m, col = 'red', pch = 20)
          # segments(0,0,Re(m), Im(m))
          # segments(-0.5,sqrt(3)/2,Re(m), Im(m), col = 'blue')
          # hist(Mod(m - xi) - 1, breaks = seq(-1, 1, by = 0.01), main = 'Histogram of blue line length')
          #
          # x = phase[222,];xi = exp(1i* x);m = mean(xi)
          # plot(xi, pch = 20, xlim = c(-1,1),ylim = c(-1,1), asp = 1, frame.plot = F, axes = F); points(m, col = 'red', pch = 20)
          # segments(0,0,Re(m), Im(m))
          # segments(-0.5,sqrt(3)/2,Re(m), Im(m), col = 'blue')
          # hist(Mod(m - xi) - 1, breaks = seq(-1, 1, by = 0.01), main = 'Histogram of blue line length')


          image_plot(x = Time, y = seq_len(ncol(dif)), z = dif - 1, ylab = 'Trial', xlab = 'Time (second)', las = 1,
                     col = cols, crop = crop,
                     symmetric = F, useRaster = F,
                     main = main, panel.last = {
                       abline(v = 0, lty = 2, lwd = 4)
                     })
        }



      }

      invisible(phase)
    },


    do_nothing = function(...){

    }
  )


  return(utils)
}

