#' Tools for module writers
#' @param env environtment to save tools in
#' @param data_env rave data repository returned by rave_prepare, internally used
#' @param quiet logical
#' @export
rave_module_tools <- function(env = NULL, data_env = NULL, quiet = FALSE) {
  if(!is.environment(data_env)){
    data_env = getDefaultDataRepository()
  }
  tools = new.env()

  local({
    ####### part 1: Data ######
    is_loaded = function(data_type){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      !is.null(repo[[data_type]])
    }


    get_power = function(force = T, referenced = T) {
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'power', 'raw_power')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        electrodes = data_env$preload_info$electrodes
        frequency_range = data_env$preload_info$frequencies
        ref_name = data_env$preload_info$reference_name

        # Try to load from cache
        re = load_local_cache(
          project_name = data_env$subject$project_name, subject_code = data_env$subject$subject_code,
          epoch = epoch_name, time_range = time_range,
          frequency_range = frequency_range, electrodes = electrodes,
          referenced = ifelse(referenced, ref_name, FALSE), data_type = c('power', 'phase')
        )

        if(!is.null(re)){
          if(isTRUE(referenced)){
            repo$power = re$power
            repo$phase = re$phase
          }else{
            repo$raw_power = re$power
            repo$raw_phase = re$phase
          }
          rm(re)
        }else{
          repo$epoch(
            epoch_name = epoch_name,
            pre = time_range[1],
            post = time_range[2],
            electrodes = electrodes,
            frequency_range = frequency_range,
            data_type = 'power',
            referenced = referenced
          )
        }
      }

      return(repo[[nm]])
    }

    get_phase = function(force = T, referenced = T){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'phase', 'raw_phase')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        electrodes = data_env$preload_info$electrodes
        frequency_range = data_env$preload_info$frequencies
        ref_name = data_env$preload_info$reference_name

        # Try to load from cache
        re = load_local_cache(
          project_name = data_env$subject$project_name, subject_code = data_env$subject$subject_code,
          epoch = epoch_name, time_range = time_range,
          frequency_range = frequency_range, electrodes = electrodes,
          referenced = ifelse(referenced, ref_name, FALSE), data_type = c('power', 'phase')
        )

        if(!is.null(re)){
          if(isTRUE(referenced)){
            repo$power = re$power
            repo$phase = re$phase
          }else{
            repo$raw_power = re$power
            repo$raw_phase = re$phase
          }
          rm(re)
        }else{
          repo$epoch(
            epoch_name = epoch_name,
            pre = time_range[1],
            post = time_range[2],
            electrodes = electrodes,
            frequency_range = frequency_range,
            data_type = 'phase',
            referenced = referenced
          )
        }
      }

      return(repo[[nm]])
    }

    get_voltage = function(force = T, referenced = T){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      nm = ifelse(referenced, 'volt', 'raw_volt')
      if(force && is.null(repo[[nm]])){
        epoch_name = data_env$.private$meta$epoch_info$name
        time_range = data_env$.private$meta$epoch_info$time_range
        electrodes = data_env$preload_info$electrodes

        ref_name = data_env$preload_info$reference_name

        # Try to load from cache
        re = load_local_cache(
          project_name = data_env$subject$project_name,
          subject_code = data_env$subject$subject_code,
          epoch = epoch_name, time_range = time_range,
          frequency_range = NULL, electrodes = electrodes,
          referenced = ifelse(referenced, ref_name, FALSE),
          data_type = 'voltage'
        )

        if(!is.null(re)){
          if(isTRUE(referenced)){
            repo$volt = re$volt
          }else{
            repo$raw_volt = re$volt
          }
        }else{
          repo$epoch(
            epoch_name = epoch_name,
            pre = time_range[1],
            post = time_range[2],
            electrodes = electrodes,
            data_type = 'volt',
            referenced = referenced
          )
        }
      }

      return(repo[[nm]])
    }

    get_voltage2 = function(){

      if(is.null(data_env$.private[['volt_unblocked']])){
        blocks = data_env$subject$preprocess_info('blocks')
        dirs = data_env$subject$dirs
        electrodes = data_env$subject$electrodes$Electrode

        progress = progress('Prepare preprocess voltage', max = length(electrodes) + 1)

        lapply_async(electrodes, function(e){
          sapply(blocks, function(b){
            f = file.path(dirs$channel_dir, 'voltage', sprintf('%d.h5', e))
            load_h5(f, '/raw/voltage/' %&% b, ram = T)
          }, simplify = F, USE.NAMES = T)
        }, .call_back = function(i){
          progress$inc(sprintf('Loading voltage data - %d', electrodes[i]))
        }) ->re

        progress$inc('Finalizing...')

        data_env$.private[['volt_unblocked']] = new.env()
        r = sapply(blocks, function(b) {
            l = list()
            l[electrodes] = lapply(re, function(comp) {
              comp[[b]]
            })
          }, simplify = F, USE.NAMES = T)

        list2env(r, envir = data_env$.private[['volt_unblocked']])

        progress$close()
        rm(list = ls(), envir = environment())
      }


      data_env$.private[['volt_unblocked']]
    }

    clean = function(items = c('raw_volt', 'raw_phase', 'raw_power')){
      for(i in items){
        data_env$.private$repo[[i]] = NULL
      }
      gc()
    }

    get_meta = function(name) {
      meta = data_env$.private$meta
      switch (
        name,
        'electrodes' = {
          meta$electrode
        },
        'frequencies' = {
          meta$frequency
        },
        'time_points' = {
          meta$time
        },
        'trials' = {
          meta$epoch_data
        }
      )
    }

    get_subject_dirs = function() {
      data_env$subject$dirs
    }

    get_subject_data = function(
      name,
      path = 'common',
      ...,
      default = NULL,
      try_open = T,
      check_cache = T
    ) {

      if(check_cache){
        val = data_env$.module_data[[path]][[name]]
        if(!is.null(val)){
          return(val)
        }
      }

      root_dir = data_env$subject$dirs$module_data_dir
      path = file.path(root_dir, path)
      if (!dir.exists(path)) {
        dir.create(path, recursive = T, showWarnings = F)
      }
      file = list.files(path,
                        pattern = sprintf('^%s\\.', name),
                        full.names = T)
      if(try_open){
        if (length(file)) {
          data = default
          try({
            data = try_load_file(file[[1]], name, ..., simplify = T)
            return(data)
          })
        }
        return(NULL)
      }else{
        return(file)
      }
    }

    get_loaded_electrodes = function() {
      repo = data_env$.private$repo
      on.exit(rm(repo))
      e = repo$raw$keys()
      e = as.numeric(e)
      e = e[!is.na(e)]
      sort(e)
    }

    save_subject_data = function(data,
                                 name,
                                 ...,
                                 path = 'common',
                                 format = 'rds') {
      format = str_to_lower(format)
      assert_that(format %in% c('rds', 'h5', 'rdata', 'csv', 'mat'),
                              msg = 'Unsupported format: MUST be rds, h5, rdata, csv, or mat')
      root_dir = data_env$subject$dirs[['rave_dir']]
      path = file.path(root_dir, 'module_data', path)
      if (!dir.exists(path)) {
        dir.create(path, recursive = T, showWarnings = F)
      }
      file = list.files(path,
                        pattern = sprintf('^%s\\.', name),
                        full.names = T)
      if (length(file)) {
        append = list(...)[['append']]
        append %?<-% FALSE
        if (!append) {
          lapply(file, function(f) {
            unlink(x = f)
          })
        }
      }

      file = file.path(path, sprintf('%s.%s', name, format))

      try_save_file(data = data,
                    name = name,
                    fpath = file,
                    ...)
    }


    get_sample_rate = function(original = F){
      if(original){
        return(data_env$.private$preproc_tools$get_srate())
      }else{
        return(data_env$subject$sample_rate)
      }
    }

    ###### Part 2: utilities #######
    get_valid_electrodes = function(electrodes = seq_len(10000)){
      data_env[['subject']]$filter_valid_electrodes(electrodes = electrodes)
    }

    # baseline = function(from, to, electrodes = NULL, ...){
    #   repo = data_env$.private$repo
    #   on.exit(rm(repo))
    #   data_env$.private$repo$baseline(from = from, to = to, electrodes = electrodes, ...)
    # }

    # TODO: Check if this works
    # baseline = rave::baseline
    baseline = baseline

    reload = function(epoch, epoch_range, reference, electrodes){
      has_change = F
      if(missing(electrodes)){
        electrodes = data_env$preload_info$electrodes
      }else{
        has_change = T
      }
      if(missing(epoch)){
        epoch = data_env$preload_info$epoch_name
      }else{
        has_change = T
      }
      if(missing(epoch_range)){
        epoch_range = range(data_env$preload_info$time_points)
        epoch_range = abs(epoch_range)
      }else{
        has_change = T
      }
      if(missing(reference)){
        reference = data_env$preload_info$reference_name
      }else{
        has_change = T
      }

      rave_prepare(
        subject = data_env$subject$subject_id,
        electrodes = electrodes,
        epoch = epoch,
        time_range = epoch_range,
        reference = reference,
        attach = F,
        data_types = NULL
      )

      group = 'main_app'
      last_entry('electrodes', electrodes, save = T, group = group)
      last_entry('epoch', epoch, save = T, group = group)
      last_entry('epoch_range', epoch_range, save = T, group = group)

      # execenv$reloadUI()
      # global_reactives$force_refresh_all = Sys.time()
      # global_reactives$has_data = Sys.time()
    }

    ###### Part 3: Visualization ######
    plot_3d_electrodes = function(
      tbl = NULL,
      electrodes,
      key_frame = NULL,   # = # of rows of values
      values = NULL,      # Each column is an electrode (# of key_frame x # of electrodes)
      marker = NULL,      # = # of electrodes
      size = NULL,
      # link_module = NULL, # Not used I guess...
      # variable_name = 'electrode',
      # link_text = 'View Electrode',
      ...
    ){

      if(missing(electrodes) || !length(values)){
        return(data_env$.private$brain$view(...))
      }

      # Validata
      ne = length(electrodes)

      if(!is.matrix(values)){
        values = matrix(values, ncol = ne, byrow = T)
      }

      key_frame %?<-% seq_len(nrow(values))
      nk = length(key_frame)

      brain = data_env$.private$brain$copy()
      if(is.null(tbl)){
        tbl = data_env$.private$repo$subject$electrodes
      }else{
        brain$load_electrodes(tbl = tbl)
      }
      n_total = nrow(tbl)


      assert_that(ne == ncol(values), msg = 'values must have column count == length of electrodes')
      assert_that(nk == nrow(values), msg = 'values must have row count == length of key_frame')
      assert_that(length(marker) %in% c(ne, 0, n_total), msg = 'marker must be 0, # of electrodes, or # of total electrodes')
      assert_that(length(size) %in% c(ne, 0, n_total), msg = 'size must be 0, # of electrodes, or # of total electrodes')



      # set value
      ms = which(length(size) == c(0, ne, n_total))[1]
      mm = which(length(marker) == c(0, ne, n_total))[1]

      # Check if 'Electrode' is in tbl
      if('Electrode' %in% names(tbl)){
        es = tbl$Electrode
      }else{
        es = seq_len(nrow(tbl))
      }

      lapply(es, function(ii){
        if(ii %in% electrodes){
          brain$set_electrode_value(which = ii, value = values[, electrodes == ii], keyframe = key_frame)
        }

        # set size
        switch (
          as.character(ms),
          '2' = {
            if(ii %in% electrodes){
              brain$set_electrode_size(which = ii, radius = size[electrodes == ii])
            }
          },
          '3' = {
            brain$set_electrode_size(which = ii, radius = size[ii])
          }
        )

        # set mesh_info
        switch (
          as.character(mm),
          '2' = {
            if(ii %in% electrodes){
              brain$set_electrode_label(
                which = ii,
                label = sprintf('Electrode %d - %s<br />%s', ii,
                                tbl$Label[tbl$Electrode == ii],
                                marker[electrodes == ii])
              )
            }
          },
          '3' = {
            brain$set_electrode_label(
              which = ii,
              label = sprintf('Electrode %d - %s<br />%s', ii, tbl$Label[tbl$Electrode == ii], marker[ii])
            )
          }
        )
      })

      brain$view(...)

    }

  }, envir = tools)



  # If env is provided, create active binds
  if(is.environment(env) && !environmentIsLocked(env)){

    makeActiveBinding('module_tools', function(){
      tools
    }, env)

    makeActiveBinding('subject', function(){
      data_env$subject
    }, env)

    makeActiveBinding('data_check', function(){
      data_env$data_check
    }, env)

    makeActiveBinding('preload_info', function(){
      data_env$preload_info
    }, env)
  }

  return(tools)


}

try_save_file <- function(data, ..., fpath, name, append = F) {
  postfix = tail(str_to_lower(as.vector(str_split(
    fpath, '\\.', simplify = T
  ))), 1)
  switch (
    postfix,
    'csv' = {
      utils::write.csv(data, fpath, ..., append = append)
    },
    'h5' = {
      args = list(...)
      ctype = args[['ctype']]
      ctype %?<-% storage.mode(data)
      save_h5(
        data,
        fpath,
        'data',
        ...,
        new_file = !append,
        replace = !append,
        ctype = ctype
      )
    },
    'rdata' = {
      nms = names(list(...))
      nms = nms[!nms == '']
      if (!missing(data)) {
        nms = c(nms, 'data')
      }
      env = new.env()
      if (append && file.exists(fpath)) {
        base::load(fpath, envir = env)
      }
      if (length(nms)) {
        nms = unique(nms)
        for (nm in nms) {
          env[[nm]] = get(nm, envir = environment(), inherits = T)
        }
        base::save(list = nms,
                   envir = env,
                   file = fpath,
                   ...)
      }
    },
    'rds' = {
      base::saveRDS(data, fpath, ...)
    },
    'mat' = {
      args = c(list(con = fpath, name = data),
               list(...))
      names(args)[2] = name

      do.call(R.matlab::writeMat,
              args = args)
    }
  )
}

try_load_file <- function(fpath, name, ..., env = new.env(parent = emptyenv()), simplify = T) {
  if (!file.exists(fpath)) {
    return(NULL)
  }
  file_info = file.info(fpath)
  if (file_info[['isdir']]) {
    return(NULL)
  }

  postfix = tail(str_to_lower(as.vector(str_split(
    fpath, '\\.', simplify = T
  ))), 1)


  if(simplify){
    env = new.env(parent = emptyenv())
  }

  switch (
    postfix,
    'csv' = {
      env$data = utils::read.csv(file = fpath, ...)
    },
    'h5' = {
      env$data = load_h5(file = fpath, name = 'data', ...)
    },
    'rdata' = {
      base::load(file = fpath, envir = env, ...)
    },
    'rds' = {
      env$data = base::readRDS(file = fpath, ...)
    },
    'mat' = {
      env$data = R.matlab::readMat(con = fpath, ...)
    }
  )

  data = as.list(env, all.names = T)

  if(simplify){
    if(length(data) == 1){
      return(data[[1]])
    }
    if(length(data) == 0){
      return(NULL)
    }
  }

  return(data)
}
