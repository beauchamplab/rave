#' Tools for module writers
#' @export
rave_module_tools <- function(env = NULL, data_env = NULL, quiet = FALSE) {
  if(!is.environment(data_env)){
    data_env = getDefaultDataRepository()
  }


  tools =
    list(

      ####### part 1: Data ######

      get_power = function(force = F) {
        repo = data_env$.private$repo
        epoch = data_env$.private$meta$epoch_info

        power = repo$power$get('power')

        if (force && is.null(power)) {
          repo$epochs$set('signature', 'RESET')

          repo$epoch(
            epoch_name = epoch$name,
            pre = epoch$time_range[1],
            post = epoch$time_range[2],
            names = 'power',
            func = NULL,
            quiet = quiet
          )
          power = repo$power$get('power')
        }
        return(power)
      },

      get_phase = function(force = F) {
        repo = data_env$.private$repo
        epoch = data_env$.private$meta$epoch_info

        phase = repo$phase$get('phase')

        if (force && is.null(phase)) {
          repo$epochs$set('signature', 'RESET')
          repo$epoch(
            epoch_name = epoch$name,
            pre = epoch$time_range[1],
            post = epoch$time_range[2],
            names = 'phase',
            func = NULL,
            quiet = quiet
          )
          phase = repo$phase$get('phase')
        }
        return(phase)
      },

      get_voltage = function(force = F){
        voltage = data_env$.private[['voltage']]
        if(force && is.null(voltage)){
          electrodes = tools$get_loaded_electrodes()
          voltage = tools$.get_voltages(electrodes)
          data_env$.private[['voltage']] = voltage
        }
        return(data_env$.private[['voltage']])
      },

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
      },

      get_subject_dirs = function() {
        data_env$subject$dirs
      },

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
      },

      get_loaded_electrodes = function() {
        repo = data_env$.private$repo
        e = repo$raw$keys()
        e = as.numeric(e)
        e = e[!is.na(e)]
        sort(e)
      },
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
      },

      get_sample_rate = function(raw = F){
        if(raw){
          return(data_env$.private$preproc_tools$get_srate())
        }else{
          return(data_env$subject$sample_rate)
        }
      },

      incubate = function(expr, fun = NULL, each = T, electrodes = NULL, data_types = NULL,
                          parallel = FALSE, ncores = rave_options('max_worker')){
        root_env = new.env(parent = parent.frame())
        expr = substitute(expr)
        if(is.function(fun)){
          expr = body(fun)
        }

        if(is.null(electrodes)){
          electrodes = data_env$preload_info$electrodes
        }
        epoch_info = data_env$.private$meta$epoch_info



        if(each){
          if(parallel){
            lapply = function(x, fun){
              progress = rave:::progress('Running', max = length(electrodes))
              on.exit({progress$close()})
              lapply_async(x, fun, .ncores = ncores, .call_back = function(i){
                progress$inc('Electrode - ', electrodes[[i]])
              })
            }
          }
          lapply(
            electrodes, function(e){
              env = new.env()
              re = NULL
              tryCatch({
                rave::rave_prepare(
                  subject = data_env$subject$id,
                  electrodes = e,
                  epoch = epoch_info$name,
                  time_range = epoch_info$time_range,data_types = data_types, attach = F, env = env, quiet = T
                )
                eval_dirty(expr, env = root_env, data = as.list(env))
              }, error = function(e){
                return(e)
              }) -> re
              return(re)
            }
          ) ->
            re
        }else{
          env = new.env()
          re = NULL
          tryCatch({
            rave::rave_prepare(
              subject = data_env$subject$id,
              electrodes = electrodes,
              epoch = epoch_info$name,
              time_range = epoch_info$time_range,data_types = data_type, attach = F, env = env
            )
            re = eval_dirty(expr, env = root_env, data = as.list(env))
          }, error = function(e){
            return(e)
          }) -> re
        }
        return(re)
      },

      .get_voltages = function(electrodes, data_only = F){
        channels = electrodes
        epoch = T
        referenced = T
        if(length(channels) > 1){
          pg = rave:::progress(title = 'Loading Voltage Data...', max = length(channels), quiet = quiet)
          on.exit({pg$close()})

          gvt = tools$.get_voltage

          re = lapply_async(channels, function(cc) {
            gvt(cc,
                referenced = referenced,
                epoch = epoch,
                data_only = T)
          }, .call_back = function(i){
            pg$inc('Channel - ' %&% channels[i])
          })
          if(!epoch){
            names(re) = as.character(channels)
            return(re)
          }else{
            dn = dimnames(re[[1]])
            re = vapply(re, I, FUN.VALUE = re[[1]])
            dn$Electrode = channels
            dimnames(re) = dn
            if(!data_only){
              re = rave::Tensor$new(data = re, dimnames = dn, dim = dim(re), varnames = c(
                "Trial", "Time", "Electrode"
              ))
            }
          }
          return(re)
        }else{
          re = tools$.get_voltage(channels, referenced, epoch, data_only = T)
          dn = dimnames(re)
          dn$Electrode = channels
          dim(re) = c(dim(re), 1)
          dimnames(re) = dn
          if(!data_only){
            re = rave::Tensor$new(data = re, dimnames = dn, dim = dim(re), varnames = c(
              "Trial", "Time", "Electrode"
            ))
          }
          return(re)
        }
      },

      .get_voltage = function(chl, referenced = T, epoch = T, data_only = F){
        utils = data_env$.private$preproc_tools
        dirs = data_env[['subject']]$dirs
        vfile = file.path(dirs$preprocess_dir, sprintf('chl_%d.h5', chl))

        blocks = utils$get_blocks()
        sprintf(
          '/%s/%s', ifelse(referenced, 'REF', 'notch'), blocks
        ) ->
          names
        lapply(names, function(nm){
          load_h5(vfile, name = nm)[]
        }) ->
          re
        names(re) = blocks
        re$sample_rate = srate = utils$get_srate()
        if(!epoch){
          return(re)
        }
        epochs = tools$get_meta(name = 'trials')
        epoch_info = data_env$.private$meta$epoch_info
        tp = seq(-epoch_info$time_range[1] * srate, epoch_info$time_range[2] * srate)
        vapply(order(epochs$Trial), function(ii){
          b = epochs$Block[ii]
          t = round(epochs$Time[ii] * srate)
          tp

          ind = t + tp
          ind[ind <= 0] = NA

          duration = epochs$Duration[ii]
          if(!is.na(duration)){
            ind[tp > duration * srate] = NA
          }
          re[[b]][ind]
        }, FUN.VALUE = as.double(tp)) ->
          re

        re = t(re)

        if(data_only){
          dimnames(re) = list(
            Trial = sort(epochs$Trial),
            Time = tp / srate
          )
          return(re)
        }else{
          re = rave::Tensor$new(data = re, dim = dim(re), dimnames = list(
            Trial = sort(epochs$Trial),
            Time = tp / srate
          ), varnames = c('Trial', 'Time'))
          return(re)
        }
      },

      ###### Part 2: utilities #######
      get_valid_electrodes = function(electrodes = seq_len(10000)){
        data_env[['subject']]$filter_valid_electrodes(electrodes = electrodes)
      },

      baseline = function(from, to, ...){
        data_env$.private$repo$baseline(from = from, to = to, ...)
      }

    )




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
