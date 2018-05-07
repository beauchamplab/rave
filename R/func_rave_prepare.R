# functions to create environment
#' @export
rave_prepare <- function(
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
  data_types = data_types[data_types %in% c('power', 'phase')]
  logger('Preparing subject [', subject,']')
  logger('# of electrodes to be loaded: ', length(electrodes))
  logger('Data type(s): ', paste(data_types, collapse = ', '))
  logger('Epoch name: ', epoch)
  logger('From: -', time_range[1], ' sec - to: ', time_range[2], ' sec.')
  repo = ECoGRepository$new(subject = subject, autoload = F)



  repo$load_electrodes(electrodes = electrodes)
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
    repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2], names = data_types, func = post_process) ->
      re
    force(re)
    rave_setup()
    logger('You have either specified func or baseline. I assume that you want adhoc analysis.', level = 'INFO')
  }else{
    repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2], names = data_types, func = post_process)
    env$.repository = repo
    env$.utils = rave_preprocess_tools()

    sid = repo$subject$id
    splits = stringr::str_split_fixed(sid, '_', 2)

    env$.utils$load_subject(subject_code = splits[1], project_name = splits[2])

    env$subject = repo$subject
    env$power = repo$power$get('power')
    env$phase = repo$phase$get('phase')
    env$valid_electrodes = repo$subject$filter_valid_electrodes

    ds = 'power'
    if(is.null(env$power)){
      ds = 'phase'
    }

    meta = repo[[ds]]$get(ds)$dimnames
    env$time_points = meta$Time
    env$trials = meta$Trial
    env$frequencies = meta$Frequency
    env$electrodes = meta$Electrode
    env$meta = repo$subject$meta
    env$meta[['epoch_data']] = repo$epochs$get('epoch_data')
    env$epoch = list(
      name = epoch,
      time_range = time_range
    )
    env$baseline = repo$baseline

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



