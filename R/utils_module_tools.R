#' Tools for module writers
#' @export
rave_module_tools <- function(env = NULL) {
  tools =
    list(

      ####### part 1: Data ######

      get_power = function(force = F) {
        data_env = rave::getDefaultDataRepository()
        if (force && is.null(data_env[['power']])) {
          data_env$.repository$epochs$set('signature', 'RESET')
          data_env$.repository$epoch(
            epoch_name = data_env$epoch$name,
            pre = data_env$epoch$time_range[1],
            post = data_env$epoch$time_range[2],
            names = 'power',
            func = NULL
          )
          data_env[['power']] = data_env$.repository$power$get('power')
        }
        return(data_env[['power']])
      },

      get_phase = function(force = F) {
        data_env = rave::getDefaultDataRepository()
        if (force && is.null(data_env[['phase']])) {
          data_env$.repository$epochs$set('signature', 'RESET')
          data_env$.repository$epoch(
            epoch_name = data_env$epoch$name,
            pre = data_env$epoch$time_range[1],
            post = data_env$epoch$time_range[2],
            names = 'phase',
            func = NULL
          )
          data_env[['phase']] = data_env$.repository$phase$get('phase')
        }
        return(data_env[['phase']])
      },

      get_meta = function(name) {
        data_env = rave::getDefaultDataRepository()
        switch (
          name,
          'electrodes' = {
            data_env$meta$electrode
          },
          'frequencies' = {
            data_env$meta$frequency
          },
          'time_points' = {
            data_env$meta$time
          },
          'trials' = {
            data_env$meta$epoch_data
          }
        )
      },

      get_subject_dirs = function() {
        data_env = rave::getDefaultDataRepository()
        data_env$subject$dirs
      },

      get_subject_data = function(name,
                                  path = 'common',
                                  ...,
                                  try_open = T) {
        data_env = rave::getDefaultDataRepository()
        root_dir = data_env$subject$dirs[['rave_dir']]
        path = file.path(root_dir, 'module_data', path)
        if (!dir.exists(path)) {
          dir.create(path, recursive = T, showWarnings = F)
        }
        file = list.files(path,
                          pattern = sprintf('^%s\\.', name),
                          full.names = T)
        if (length(file) && try_open) {
          env = try_load_file(file[[1]], name, ...)
          return(env)
        }
        return(NULL)
      },

      get_loaded_electrodes = function() {
        data_env = rave::getDefaultDataRepository()
        p = data_env$.repository$power$get('power')
        p %?<-% data_env$.repository$power$get('phase')
        if(is.null(p)){
          return(NULL)
        }else{
          return(p$dimnames$Electrode)
        }
      },
      save_subject_data = function(data,
                                   name,
                                   ...,
                                   path = 'common',
                                   format = 'rds') {
        data_env = rave::getDefaultDataRepository()
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
        data_env = rave::getDefaultDataRepository()
        if(raw){
          return(data_env$.utils$get_srate())
        }else{
          return(data_env$subject$sample_rate)
        }
      },

      get_voltages = function(channels, data_only = F){
        epoch = T
        referenced = T
        if(length(channels) > 1){
          pg = rave:::progress(title = 'Loading Voltage Data...', max = length(channels))
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
        data_env = rave::getDefaultDataRepository()
        utils = data_env$.utils
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
        tp = seq(-data_env$epoch$time_range[1] * srate, data_env$epoch$time_range[2] * srate)
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
        data_env = rave::getDefaultDataRepository()
        data_env[['subject']]$filter_valid_electrodes(electrodes = electrodes)
      },

      baseline = function(from, to, ...){
        data_env = rave::getDefaultDataRepository()
        data_env$.repository$baseline(from = from, to = to, ...)
      }

    )




  # If env is provided, create active binds
  if(is.environment(env) && !environmentIsLocked(env)){
    makeActiveBinding('power', function(){
      tools$get_power()
    }, env)

    makeActiveBinding('phase', function(){
      tools$get_phase()
    }, env)

    makeActiveBinding('epoch', function(){
      data_env = rave::getDefaultDataRepository()
      data_env[['epoch']]
    }, env)

    makeActiveBinding('baseline', function(){
      tools$baseline
    }, env)

    makeActiveBinding('subject', function(){
      data_env = rave::getDefaultDataRepository()
      data_env[['subject']]$clone()
    }, env)

    makeActiveBinding('valid_electrodes', function(){
      tools$get_valid_electrodes
    }, env)

    makeActiveBinding('module_tools', function(){
      tools
    }, env)

    makeActiveBinding('preprocess_tools', function(){
      data_env = rave::getDefaultDataRepository()
      data_env[['.utils']]
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
        name,
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
try_load_file <- function(fpath, name, ..., env = new.env()) {
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



  switch (
    postfix,
    'csv' = {
      env$data = utils::read.csv(fpath, ...)
    },
    'h5' = {
      env$data = rave::load_h5(fpath, name, ...)
    },
    'rdata' = {
      base::load(fpath, envir = env, ...)
    },
    'rds' = {
      env$data = base::readRDS(fpath, ...)
    },
    'mat' = {
      env$data = R.matlab::readMat(fpath, ...)
    }
  )

  as.list(env)
}
