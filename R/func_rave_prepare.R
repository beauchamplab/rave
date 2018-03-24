# functions to create environment
#' @export
rave_prepare <- function(
  subject, electrodes,
  epoch , time_range,
  data_types = c('power'),
  attach = T, quiet = F,
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
    return(re)
  }else{
    repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2], names = data_types, func = post_process)
    env$.repository = repo
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
    env$epoch = list(
      name = epoch,
      time_range = time_range
    )
    env$baseline = repo$baseline

    if(attach){
      if('rave_data' %in% search()){
        base::detach('rave_data', character.only = T, force = T)
      }
      base::attach(env, name = 'rave_data')
      if(!quiet){
        logger('Environment for subject [', subject, '] has been created!',
               ' Here are some variables that can be used directly: ', level = 'INFO')
        for(nm in ls(envir = env, all.names = F)){
          cat('-', nm, '\n')
        }
        logger('Check ?rave_prepare for details.', level = 'INFO')
      }
    }
    invisible(env)
  }



}
