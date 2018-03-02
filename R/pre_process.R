
#' @import future
#' @import stringr
#' @import signal
#' @import miniUI
#' @import shiny
#' @import magrittr
#' @export
pre_process <- function(steps = c('notch', 'car', 'wavelet'),
                        vis = c('notch', 'car', 'wavelet'),
                        save_plot = T,
                        wave_num = 7, frequencies = seq(4, 200, by = 4), compress = 2, replace = F,
                        ncores = future::availableCores() - 2, plan = future::multisession, ...){
  settings = mini_subject_info()

  dirs = get_dir(subject_code = settings$subject_code, project_name = settings$project_name)


  if('notch' %in% steps){
    logger('Notch filter started', level = 'INFO')
    bulk_notch(project_name = settings$project_name,
               subject_code = settings$subject_code,
               blocks = settings$blocks,
               channels = settings$channels, srate = settings$srate,
               replace = replace, save_plot = save_plot, new_cache = F)
  }

  if('notch' %in% vis){
    logger('Raw signal plots', level = 'INFO')
    signals = list()
    for(block_num in settings$blocks){
      signals[[block_num]] = h5read(file.path(dirs$preprocess_dir, 'all_notch.h5'), name = block_num)
    }
    mini_parallel_plots(signals, sample_rate = settings$srate,
                        channel_names = settings$channels, project_name = settings$project_name,
                        subject_code = settings$subject_code)

    logger('Channel inspection', level = 'INFO')

    mini_channel_inspection(process_name = 'notch', project_name = settings$project_name,
                            subject_code = settings$subject_code, blocks = settings$blocks,
                            channels = settings$channels, srate = settings$srate)
  }


  if('car' %in% steps){
    electrodes = load_meta('electrodes', project_name = settings$project_name, subject_code = settings$subject_code)
    if(is.null(electrodes)){
      logger('You have not chosen CAR channels yet. Trying to launch channel selection program...', level = 'WARNING')
      pre_process(settings, steps = '', vis = c('notch'))
    }
    logger('CAR Started', level = 'INFO')
    channels = electrodes$Electrode[!(electrodes$Epichan | electrodes$Badchan)]
    bulk_CAR(project_name = settings$project_name, subject_code = settings$subject_code, blocks = settings$blocks,
             srate = settings$srate, channels = channels, replace = replace, save_plot = save_plot)

  }

  if('car' %in% vis){
    logger('CAR before vs after plots', level = 'INFO')
    mini_channel_inspection(process_name = 'CAR', project_name = settings$project_name,
                            subject_code = settings$subject_code, blocks = settings$blocks,
                            channels = settings$channels, srate = settings$srate)
  }


  if('wavelet' %in% steps){
    electrodes = load_meta('electrodes', project_name = settings$project_name, subject_code = settings$subject_code)
    channels = electrodes$Electrode[!(electrodes$Epichan | electrodes$Badchan) & electrodes$Electrode %in% settings$channels]



    logger('Wavelet started (Now grab a coffee. Don\'t close this session)', level = 'INFO')
    bulk_wavelet(project_name = settings$project_name,
                 subject_code = settings$subject_code,
                 blocks = settings$blocks,
                 channels = channels,
                 srate = settings$srate,
                 frequencies = frequencies,
                 wave_num = wave_num,
                 compress = compress, replace = replace,
                 ncores = ncores, plan = plan) ->
      check

    return(check)
  }



}

#' #' Shiny gadgets for Rstudio
#' #' ECoG data pre-processing
#' NULL
#'
#' #' Function to load pre-process modules
#' #'
#' #' @details
#' #' You can write your own pre-process modules, please refer to the documents
#' #' or type cat(readLines(system.file('preprocess/demo.R', package = 'rave')),sep="\\n")
#' #' to view an example,
#' #'
#' #' Easiest way is to copy the demo files located at (run system.file('preprocess', package = 'rave'))
#' #' and edit according to instructions
#' #'
#' #' Once finished, run load_preprocess_module(script = [path to your own script])
#' load_preprocess_module <- function(script = NULL){
#'   if(is.null(script)){
#'     if(is.null(rave_opts$get_options('preprocess_module'))){
#'       script = system.file('preprocess/main.R', package = 'rave')
#'     }else{
#'       script = rave_opts$get_options('preprocess_module')
#'     }
#'   }
#'   logger('Try to load pre-processing modules - ', script)
#'   source(script, local = T)
#'
#'   env = .load_preprocess_func(local = T)
#'
#'   # if everything works, save the options
#'   rave_opts$set_options(preprocess_module = script)
#'
#'   return(env)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' pre_process_visual <- function(process_name, project_name, subject_code, blocks,
#'                                channels, srate = 2000,...){
#'   data_dir = rave_opts$get_options('data_dir')
#'   subject_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'preprocess')
#'   # env = load_preprocess_module()
#'
#'   if(process_name %in% c('notch', 'CAR')){
#'     signal_visual(process_name, project_name, subject_code, blocks, channels, ...)
#'   }
#'   if(process_name %in% c('CARinspect')){
#'     # Check if CARinspect.h5 exists
#'
#'
#'     # assume save_dir exists
#'
#'     CARinspect_file = file.path(subject_dir, 'CARinspect.h5')
#'     if(!file.exists(CARinspect_file)){
#'       rhdf5::h5createFile(CARinspect_file)
#'     }
#'     signals = list()
#'     for(block_num in blocks){
#'       if(!block_num %in% h5ls(CARinspect_file)){
#'
#'         ss = pre_inspect(
#'           process_name = process_name, project_name = project_name,
#'           subject_code = subject_code, block_num = block_num, chls = channels, compress = 20, ...
#'         )
#'
#'         h5createDataset(CARinspect_file, block_num, dim(ss), chunk = c(1, 1024), level = 3)
#'         h5write(ss, CARinspect_file, block_num)
#'         H5close()
#'       }else{
#'         ss = h5read(CARinspect_file, block_num)
#'       }
#'       signals[[block_num]] = ss
#'     }
#'
#'     signals_parallel_visual(signals, sample_rate = srate / 20, channel_names = channels, project_name, subject_code)
#'
#'   }
#' }
#'
#'
#'
#'
#'
#' pre_process <- function(process_name, project_name, subject_code,
#'                         blocks, channels, ...){
#'   # example code:
#'   # bulk_notch(project_name = 'Congruency',
#'   #            subject_code = 'YAB',
#'   #            blocks = c('010','011','012'),
#'   #            channels = 1:84)
#'   #
#'   # allsignals = bulk_CAR(project_name = 'Congruency',
#'   #          subject_code = 'YAB',
#'   #          blocks = c('010','011','012'),
#'   #          channels = (1:84)[-c(1:12, 32,64)])
#'   #
#'   # check = bulk_wavelet(project_name = 'Congruency',
#'   #          subject_code = 'YAB',
#'   #          blocks = c('008', '010','011','012'),
#'   #          channels = (1:84)[-c(1:12, 32,64)])
#'   # env = load_preprocess_module()
#'
#'   switch(
#'     process_name,
#'     'notch' = {
#'       bulk_notch(project_name, subject_code, blocks, channels, ...)
#'       return(invisible())
#'     },
#'     'CAR' = {
#'       car = bulk_CAR(project_name, subject_code, blocks, channels, ...)
#'       return(invisible(car))
#'     },
#'     'wavelet' = {
#'       bulk_wavelet(project_name, subject_code, blocks, channels, ...)
#'       return(invisible())
#'     }
#'   )
#'
#' }







