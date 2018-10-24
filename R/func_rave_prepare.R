#' Function to load subject and create ECoG data environment
#' @param subject characters, format: "PROJECT/SUBJECT"
#' @param electrodes numeric vector, which electrodes to be loaded
#' @param epoch, characters, depending on subject meta data. For example, use "epoch1" if exists epoch file "epoch_epoch1.csv"
#' @param time_range, vector of length 2 - before and after onset. For example, c(1,2), means 1 second before onset and 2 seconds after onset.
#' @param frequency_range, vector of length 2 - lowest and highest frequencies. By default is all frequencies. Only applied to power and phase data.
#' @param data_types, vector of characters, data to be preloaded. "power" - referenced power data, "phase" - referenced phase data, "volt" - referenced voltage data
#' @param reference, similar to epoch, For example, use "default" if exists reference file "reference_default.csv"
#' @param attach, characters or NULL, NULL if you don't want to attach it, "r" if want to load data as R environment, "py" if python, "matlab" for matlab.
#' @param data_env, environment to load data into.
#' @details Usually this function is for module writters and for debug use, or adhoc analysis.
#' @export
rave_prepare <- function(
  subject,
  electrodes,
  epoch , time_range,
  frequency_range,
  data_types = c('power'),
  reference = 'default', attach = 'r',
  load_brain = TRUE,
  data_env = rave::getDefaultDataRepository()
){
  # subject = 'congruency1/YAB'; electrodes = 14:15; epoch = 'YABa'; time_range = c(1,2); data_types = NULL; reference = 'default'
  if(missing(subject)){
    # detach enironment
    if('rave_data' %in% search()){
      base::detach('rave_data', character.only = T, force = T)
    }
    return(invisible())
  }
  logger('Preparing subject [', subject,']')
  logger('# of electrodes to be loaded: ', length(electrodes))
  logger('Data type(s): ', paste(data_types, collapse = ', '))
  logger('Epoch name: ', epoch)
  logger('From: ', -(time_range[1]), ' sec - to: ', time_range[2], ' sec.')
  if(!missing(frequency_range)){
    logger('Frequencies: ', (frequency_range[1]), ' Hz - to: ', frequency_range[2], ' Hz.')
  }else{
    frequency_range = NULL
    logger('Frequencies: All')
  }

  repo = ECoGRepository$new(subject = subject, autoload = F, reference = reference)
  repo$load_electrodes(electrodes = electrodes, reference = reference)

  # Set epoch information
  tmp = c('power', 'phase', 'volt'); tmp = tmp[tmp %in% data_types]
  repo$epoch(
    epoch_name = epoch,
    pre = time_range[1],
    post = time_range[2],
    electrodes = electrodes,
    frequency_range = frequency_range,
    data_type = tmp,
    referenced = T,
    func = NULL
  )

  # Load brain
  brain = RaveBrain$new()
  brain$load_subject(subject = repo$subject)

  if(load_brain){
    try({
      brain$import_spec(nearest_face = F)
    }, silent = TRUE)
  }



  # Register to data_env
  subject = repo$subject
  meta = subject$meta
  meta[['epoch_data']] = load_meta('epoch', subject_id = subject$subject_id, meta_name = epoch)
  meta[['epoch_info']] = list(
    name = epoch,
    time_range = time_range
  )
  # Change frequencies if specified
  freqs = subject$frequencies$Frequency
  if(!is.null(frequency_range)){
    freq_subset = freqs %within% frequency_range
    if(!sum(freq_subset)){
      freq_subset[which.min(abs(freqs - frequency_range[1]))] = T
    }
    freqs = freqs[freq_subset]
  }
  data_env$.private = new.env(parent = baseenv())
  data_env$.private$repo = repo
  data_env$.private$meta = meta
  data_env$.private$brain = brain
  data_env$.private$preproc_tools = rave:::rave_preprocess_tools()
  data_env$data_check = data_env$.private$preproc_tools$check_load_subject(subject_code = subject$subject_code, project_name = subject$project_name)
  data_env$subject = subject
  data_env$preload_info = list(
    epoch_name = data_env$.private$meta$epoch_info$name,
    reference_name = reference,
    time_points = seq(-time_range[1], time_range[2], by = 1/subject$sample_rate),
    electrodes = subject$filter_valid_electrodes(electrodes),
    frequencies = freqs,
    condition = unique(data_env$.private$meta$epoch_data$Condition)
  )
  data_env$.module_data = new.env(parent = baseenv())
  # Load other data
  other_data_types = data_types[!data_types %in% c('power', 'volt')]
  if(length(other_data_types)){
    module_data_dir = subject$dirs$module_data_dir
    lapply(other_data_types, function(path){
      data_env$.module_data[[path]] %?<-% new.env(parent = emptyenv())
      lapply(list.files(file.path(module_data_dir, path)), function(name){
        re = data_env$module_tools$get_subject_data(path = path, name = name)
        data_env$.module_data[[path]][[name]] = re
        NULL
      })
      NULL
    })
  }

  # register util functions
  data_env$module_tools = rave_module_tools(data_env = data_env)


  ##### Attach data_env #####
  if(attach != FALSE){
    if('rave_data' %in% search()){
      base::detach('rave_data', character.only = T, force = T)
    }

    base::attach(data_env, name = 'rave_data')


    if(attach == TRUE || attach %in% c('R', 'r')){
      prefix = ''
      dev_env = 'r'
    }else if (str_to_lower(attach) %in% c('py2', 'py3')){
      prefix = 'r.'
      dev_env = 'python'
      dev_ver = str_to_lower(attach)
    }

    logger('Environment for subject [', subject$id, '] has been created!',
           ' Here are some variables that can be used directly: ', level = 'INFO')
    for(nm in ls(envir = data_env, all.names = F)){
      cat('- ', prefix, nm, '\n', sep = '')
    }
    logger('Check ?rave_prepare for details.', level = 'INFO')

    if(dev_env == 'python'){
      py_console(py3 = (dev_ver == 'py3'))
    }
  }
  re = data_env
  re
}


.rave_prepare <- function(
  subject, electrodes,
  epoch , time_range,
  data_types = c('power'),
  attach = 'r', quiet = F,
  env = rave::getDefaultDataRepository(),
  func = NULL, baseline = NULL
){
  if(missing(subject)){
    # detach enironment
    if('rave_data' %in% search()){
      base::detach('rave_data', character.only = T, force = T)
    }
    return(invisible())
  }
  logger('Preparing subject [', subject,']')
  logger('# of electrodes to be loaded: ', length(electrodes))
  logger('Data type(s): ', paste(data_types, collapse = ', '))
  logger('Epoch name: ', epoch)
  logger('From: -', time_range[1], ' sec - to: ', time_range[2], ' sec.')
  repo = ECoGRepository$new(subject = subject, autoload = F)



  repo$load_electrodes(electrodes = electrodes, quiet = quiet)
  if(!is.null(baseline)){
    data_types = 'power'
    post_process = function(e_power){
      e_power = repo$..baseline(baseline[1], baseline[2], electrode = NULL, e_power = e_power,
                                 print.time = !quiet)
      if(is.function(func)){
        func(e_power)
      }else{
        e_power # if func is not a function, return baselined (eq to func=I)
      }
    }
  }else{
    post_process = func
  }

  if(is.function(post_process)){
    repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2], names = data_types, func = post_process, quiet = quiet) ->
      re
    force(re)
    rave_setup()
    logger('You have either specified func or baseline. I assume that you want adhoc analysis.', level = 'INFO')
  }else{
    # actually load data to environment

    # load power phase
    tmp = c('power', 'phase', 'volt', 'voltage'); tmp = tmp[tmp %in% data_types]
    if(length(tmp)){
      repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2], names = tmp, quiet = quiet)
    }



    subject = repo$subject




    meta = subject$meta
    meta[['epoch_data']] = load_meta('epoch', subject_id = subject$subject_id, meta_name = epoch)
    meta[['epoch_info']] = list(
      name = epoch,
      time_range = time_range
    )

    # bindings
    env$.private = new.env(parent = baseenv())
    env$.private$repo = repo
    env$.private$meta = meta

    env$.private$preproc_tools = rave_preprocess_tools()
    env$data_check = env$.private$preproc_tools$check_load_subject(subject_code = subject$subject_code, project_name = subject$project_name)
    env$subject = subject
    env$module_tools = rave_module_tools(data_env = env, quiet = quiet)
    env$preload_info = list(
      epoch_name = env$.private$meta$epoch_info$name,
      time_points = seq(-time_range[1], time_range[2], by = 1/subject$sample_rate),
      electrodes = electrodes,
      frequencies = subject$frequencies$Frequency,
      condition = unique(env$.private$meta$epoch_data$Condition)
    )

    env$.module_data = new.env(parent = baseenv())

    # Load voltage and customized data
    # if('voltage' %in% data_types){
    #   env$.private[['voltage']] = env$module_tools$get_voltage(force = T)
    # }

    # Load other data
    other_data_types = data_types[!data_types %in% c('power', 'phase', 'voltage', 'volt')]
    if(length(other_data_types)){
      module_data_dir = subject$dirs$module_data_dir
      lapply(other_data_types, function(path){
        env$.module_data[[path]] %?<-% new.env(parent = emptyenv())
        lapply(list.files(file.path(module_data_dir, path)), function(name){
          re = env$module_tools$get_subject_data(path = path, name = name)
          env$.module_data[[path]][[name]] = re
          NULL
        })
        NULL
      })
    }




    if(attach != FALSE){
      if('rave_data' %in% search()){
        base::detach('rave_data', character.only = T, force = T)
      }

      base::attach(env, name = 'rave_data')


      if(attach == TRUE || attach %in% c('R', 'r')){
        prefix = ''
        dev_env = 'r'
      }else if (str_to_lower(attach) %in% c('py2', 'py3')){
        prefix = 'r.'
        dev_env = 'python'
        dev_ver = str_to_lower(attach)
      }

      if(!quiet){
        logger('Environment for subject [', subject, '] has been created!',
               ' Here are some variables that can be used directly: ', level = 'INFO')
        for(nm in ls(envir = env, all.names = F)){
          cat('- ', prefix, nm, '\n', sep = '')
        }
        logger('Check ?rave_prepare for details.', level = 'INFO')

        if(dev_env == 'python'){
          py_console(py3 = (dev_ver == 'py3'))
        }
      }
    }
    re = env
  }


  invisible(re)
}



