#' @title Load Subject and Create \code{iEEG/ECoG} Data Environment
#' @description Loads subject data along with \code{iEEG/ECoG} data into memory.
#' @param subject characters, format: \code{"PROJECT/SUBJECT"}
#' @param electrodes integer vector, which electrodes to be loaded
#' @param epoch characters, name for epoch data. For example, \code{"epoch1"} 
#' refers to epoch file \code{"epoch_epoch1.csv"} in subject meta folder
#' @param time_range vector of length 2. \code{time_range[1]=1} indicates 1 
#' second before onset will be loaded; \code{time_range[2]=1.5} means 1.5 
#' seconds after onset will be loaded. Make sure both are positive number in 
#' seconds
#' @param frequency_range vector of length 2 - lowest and highest frequencies. 
#' By default is all frequencies. Only applied to power and phase data.
#' @param data_types vector of characters, data to be pre-loaded. 
#' \code{"power"} refers to referenced power (power spectrum) data, 
#' \code{"phase"} refers to referenced phase data, and \code{"volt"} 
#' is referenced voltage (original analog traces) data
#' @param reference name of reference data. For example, \code{"default"} refers
#' to reference file \code{"reference_default.csv"} in subject meta folder
#' @param attach, characters or \code{NULL}, \code{NULL} if you don't want to 
#' attach it, \code{"r"} if want to load data as R environment, \code{"py"} is 
#' for python, and \code{"matlab"} is for Matlab. (python and Matlab are under 
#' construction)
#' @param data_env environment to load data into.
#' @param strict whether to check if raw data exists. Default is no (suggested)
#' @param ... ignored
#' @export
rave_prepare <- function(
  subject,
  electrodes,
  epoch , time_range,
  frequency_range,
  data_types = c('power'),
  reference = 'default', attach = 'r',
  data_env = getDefaultDataRepository(),
  strict = FALSE,
  ...
){
  # subject = 'demo/YAB'; electrodes = 14:15; epoch = 'YABaOutlier'; time_range = c(1,2); data_types = NULL; reference = 'default'; strict = FALSE; frequency_range = NULL
  if(missing(subject)){
    # detach enironment
    if('rave_data' %in% search()){
      base::detach('rave_data', character.only = TRUE, force = TRUE)
    }
    return(invisible())
  }
  if(identical(data_env, getDefaultDataRepository())){
    clear_env(data_env)
    gc()
  }
  
  catgl('Preparing subject [', as.character(subject), ']')
  catgl('# of electrodes to be loaded: ', length(electrodes))
  catgl('Data type(s): ', paste(data_types, collapse = ', '))
  catgl('Epoch name: ', epoch)
  catgl('From: ', -(time_range[1]), ' sec - to: ', time_range[2], ' sec.')
  if(!missing(frequency_range)){
    catgl('Frequencies: ', (frequency_range[1]), ' Hz - to: ', frequency_range[2], ' Hz.')
  }else{
    frequency_range = NULL
    catgl('Frequencies: All')
  }
  if(is.character(subject)){
    subject_split = unlist(strsplit(subject, '/|\\\\'))
    subject = Subject$new(project_name = subject_split[[1]], subject_code = subject_split[[2]], reference = reference, strict = strict)
  }

  subject$is_strict = strict

  repo = ECoGRepository$new(subject = subject, autoload = FALSE, reference = reference)
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
    referenced = TRUE,
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
  # Change frequencies if specified
  freqs = subject$frequencies$Frequency
  if(!is.null(frequency_range)){
    freq_subset = freqs %within% frequency_range
    if(!sum(freq_subset)){
      freq_subset[which.min(abs(freqs - frequency_range[1]))] = TRUE
    }
    freqs = freqs[freq_subset]
  }
  data_env$.private = new.env(parent = baseenv())
  data_env$.private$repo = repo
  data_env$.private$meta = meta
  data_env$.private$preproc_tools = rave_preprocess_tools()
  data_env$.private$preproc_tools$check_load_subject(subject_code = subject$subject_code, project_name = subject$project_name, strict = subject$is_strict)
  data_env$data_check = check_subjects2(project_name = subject$project_name,
                                               subject_code = subject$subject_code, quiet = TRUE)
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

    rave_idx = which(search() == "package:rave")

    if(length(rave_idx)){
      do.call('attach', list(data_env, name = 'rave_data', pos = rave_idx))
    }else{
      do.call('attach', list(data_env, name = 'rave_data'))
    }



    if(attach == TRUE || attach %in% c('R', 'r')){
      prefix = ''
      dev_env = 'r'
    }else if (stringr::str_to_lower(attach) %in% c('py2', 'py3')){
      prefix = 'r.'
      dev_env = 'python'
      dev_ver = stringr::str_to_lower(attach)
    }

    catgl('Environment for subject [', subject$id, '] has been created!',
           ' Here are some variables that can be used directly: ', level = 'INFO')
    for(nm in ls(envir = data_env, all.names = F)){
      cat('- ', prefix, nm, '\n', sep = '')
    }
    catgl('Check ?rave_prepare for details.', level = 'INFO')

    if(dev_env == 'python'){
      stop('dev_env == python is depricated')
      # py_console(py3 = (dev_ver == 'py3'))
    }
  }
  re = data_env
  re
}



