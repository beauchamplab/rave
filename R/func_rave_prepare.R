# functions to create environment
#' @export
rave_prepare <- function(
  subject, electrodes,
  epoch , time_range,
  attach = T, quiet = F,
  env = rave::getDefaultDataRepository()
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
  logger('Epoch name: ', epoch)
  logger('From: -', time_range[1], ' sec - to: ', time_range[2], ' sec.')
  repo = ECoGRepository$new(subject = subject, autoload = F)
  env$.repository = repo


  repo$load_electrodes(electrodes = electrodes)
  repo$epoch(epoch_name = epoch, pre = time_range[1], post = time_range[2])


  env$subject = repo$subject
  env$power = repo$power$get('power')
  env$phase = repo$phase$get('phase')
  env$valid_electrodes = repo$subject$filter_valid_electrodes
  meta = repo$power$get('power')$dimnames
  env$time_points = meta$Time
  env$trials = meta$Trial
  env$frequencies = meta$Frequency
  env$electrodes = meta$Electrode
  env$meta = repo$subject$meta
  env$epoch = list(
    name = epoch,
    time_range = time_range
  )

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
