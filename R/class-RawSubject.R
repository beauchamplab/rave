# Definition of subject information
#
# @param project_name Subject project
# @param subject_code Subject code (folder)
# @param blocks All possible blocks
# @param channels All possible channels (including bad, epi channels)
# @param badchan Bad channels (will be ignored)
# @param epichan Epilepsy channels (will be ignored)
# @param exclchan Channels counted as valid but excluded from CAR
SubjectInfo2 <- R6::R6Class(
  classname = 'SubjectInfo2',
  private = list(
    raw_dir = NULL,
    pre_dir = NULL,
    settings_file = NULL,
    log_file = NULL
  ),
  public = list(
    project_name = NULL,
    subject_code = NULL,
    blocks = NULL,
    channels = NULL,
    srate = NULL,
    badchan = NULL,
    epichan = NULL,
    exclchan = NULL,
    valid = FALSE,
    dirs = NULL,
    logger = NULL,
    cacher = NULL,
    available_channels = NULL,
    available_blocks = NULL,
    
    # Init
    initialize = function(project_name, subject_code, strict = TRUE){
      self$dirs = get_dir(subject_code = subject_code, project_name = project_name)
      if(strict && !dir.exists(self$dirs$pre_subject_dir)){
        stop('Subject NOT Found!')
      }
      # make sure that preprocess_dir exists
      get_dir(subject_code = subject_code, project_name = project_name,
              mkdirs = 'preprocess_dir')
      
      self$project_name = project_name
      self$subject_code = subject_code
      private$settings_file = file.path(self$dirs$preprocess_dir, 'rave.yaml')
      private$log_file = file.path(self$dirs$preprocess_dir, 'log.csv')
      
      
      # load available info
      self$available_blocks = list.dirs(self$dirs$pre_subject_dir, full.names = F, recursive = F)
      # self$available_channels
      
      
      
      self$logger = RAVEHistory$new(
        path = self$dirs$preprocess_dir,
        name = 'rave.yaml', use_yaml = TRUE
      )
      self$cacher = RAVEHistory$new(
        path = self$dirs$preprocess_dir,
        name = 'rave.RData', use_yaml = FALSE
      )
      
      self$blocks = self$logger$get_or_save('blocks')
      self$channels = self$logger$get_or_save('channels')
      self$srate = self$logger$get_or_save('srate')
      self$badchan = self$logger$get_or_save('badchan')
      self$epichan = self$logger$get_or_save('epichan')
      self$exclchan = self$logger$get_or_save('exclchan')
      
      self$valid = TRUE
    },
    set_blocks = function(blocks, force = FALSE){
      is_changed = FALSE
      if(!force){
        blocks = blocks[blocks %in% self$available_blocks]
      }
      if(!base::setequal(self$blocks, blocks)){
        self$blocks = blocks
        is_changed = TRUE
      }
      return(is_changed)
    },
    set_channels = function(channels, name = 'channels'){
      is_changed = FALSE
      if(name == 'channels'){
        if(!setequal(self[[name]], channels)){
          self$channels = channels
          is_changed = TRUE
        }
        
      }else{
        channels = channels[channels %in% self$channels]
        if(!setequal(self[[name]], channels)){
          assign(name, channels, envir = self)
          is_changed = TRUE
        }
      }
      return(is_changed)
    },
    save = function(action = '', message = '', ...){
      defaults = data.frame(
        Index = 0,
        Date = strftime(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'),
        Action = 'Initialization',
        Message = sprintf('Subject folder created - %s', base::normalizePath(self$dirs$subject_dir)),
        stringsAsFactors = F
      )
      history = self$logger$get_or_save('log', defaults, save = TRUE, inherits = FALSE)
      # if(!is.data.frame(history)){
      #   history = defaults
      # }
      history = rbind(data.frame(
        Index = max(history$Index) + 1,
        Date = strftime(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'),
        Action = action,
        Message = message,
        stringsAsFactors = F
      ), history)
      
      self$logger$save(
        project_name = self$project_name,
        subject_code = self$subject_code,
        blocks = self$blocks,
        channels = self$channels,
        srate = self$srate,
        badchan = self$badchan,
        epichan = self$epichan,
        exclchan = self$exclchan,
        log = history,
        ...
      )
    }
  )
)
