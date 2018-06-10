#' Tools for module writers
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
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'power',
          referenced = referenced
        )
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
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'phase',
          referenced = referenced
        )
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
        repo$epoch(
          epoch_name = epoch_name,
          pre = time_range[1],
          post = time_range[2],
          electrodes = data_env$preload_info$electrodes,
          data_type = 'volt',
          referenced = referenced
        )
      }
      return(repo[[nm]])
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
          })
        }
        return(data)
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
      assertthat::assert_that(format %in% c('rds', 'h5', 'rdata', 'csv', 'mat'),
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

    baseline = function(from, to, electrodes = NULL, ...){
      repo = data_env$.private$repo
      on.exit(rm(repo))
      data_env$.private$repo$baseline(from = from, to = to, electrodes = electrodes, ...)
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

#' @import stringr
#' @export
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
      rave::save_h5(
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

#' @import stringr
#' @export
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
      env$data = rave::load_h5(file = fpath, name = 'data', ...)
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
