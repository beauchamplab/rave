# functions to create environment
#' @export
#' @export
rave_prepare <- function(
  subject, electrodes,
  epoch , time_range,
  data_types = c('power'),
  reference = 'default', attach = 'r',
  data_env = rave::getDefaultDataRepository()
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
  logger('From: ', -(time_range[1]), ' sec - to: ', time_range[2], ' sec.')

  repo = ECoGRepository$new(subject = subject, autoload = F, reference = reference)
  repo$load_electrodes(electrodes = electrodes)

  # Set epoch information
  tmp = c('power', 'phase', 'volt'); tmp = tmp[tmp %in% data_types]
  repo$epoch(
    epoch_name = epoch,
    pre = time_range[1],
    post = time_range[2],
    electrodes = electrodes,
    data_type = tmp,
    referenced = T,
    func = NULL
  )


  # Register to data_env
  subject = repo$subject
  meta = subject$meta
  meta[['epoch_data']] = load_meta('epoch', subject_id = subject$subject_id, meta_name = epoch)
  meta[['epoch_info']] = list(
    name = epoch,
    time_range = time_range
  )
  data_env$.private = new.env(parent = baseenv())
  data_env$.private$repo = repo
  data_env$.private$meta = meta
  data_env$.private$preproc_tools = rave:::rave_preprocess_tools()
  data_env$data_check = data_env$.private$preproc_tools$check_load_subject(subject_code = subject$subject_code, project_name = subject$project_name)
  data_env$subject = subject
  data_env$preload_info = list(
    epoch_name = data_env$.private$meta$epoch_info$name,
    time_points = seq(-time_range[1], time_range[2], by = 1/subject$sample_rate),
    electrodes = electrodes,
    frequencies = subject$frequencies$Frequency,
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
    tmp = c('power', 'phase'); tmp = tmp[tmp %in% data_types]
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
    if('voltage' %in% data_types){
      env$.private[['voltage']] = env$module_tools$get_voltage(force = T)
    }

    # Load other data
    other_data_types = data_types[!data_types %in% c('power', 'phase', 'voltage')]
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



