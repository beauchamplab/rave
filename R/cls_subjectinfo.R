#' Definition of subject information
#'
#' @param project_name Subject project
#' @param subject_code Subject code (folder)
#' @param blocks All possible blocks
#' @param channels All possible channels (including bad, epi channels)
#' @param badchan Bad channels (will be ignored)
#' @param epichan Epilepsy channels (will be ignored)
#' @param exclchan Channels counted as valid but excluded from CAR
#' @export
# SubjectInfo <- R6::R6Class(
#   classname = 'SubjectInfo',
#   private = list(
#     errors = NULL,
#     add_error = function(e){
#       rave::logger(e)
#       private$errors = c(private$errors, e)
#     },
#     settings_file = "",
#     log_file = '',
#     meta = NULL,
#     reactiveFlag = NULL,
#     load_settings = function(){
#       f = private$settings_file
#       # if file not exists, create one
#       if(!file.exists(f)){
#         s = list(
#           project_name = self$project_name,
#           subject_code = self$subject_code,
#           blocks = self$blocks,
#           channels = self$channels,
#           badchan = self$badchan,
#           epichan = self$epichan,
#           exclchan = self$exclchan,
#           srate = self$srate
#         )
#         yaml::write_yaml(s, file = f, fileEncoding = 'UTF-8')
#         l = data.frame(
#           Index = 1,
#           Modified = str_c(Sys.time()),
#           Action = 'Initialize',
#           Value = '',
#           stringsAsFactors = F
#         )
#         write.csv(l, private$log_file, row.names = F)
#       }else{
#         s = yaml::read_yaml(f)
#         l = read.csv(private$log_file, stringsAsFactors = F)
#
#         # recover previous settings
#         self$blocks = s$blocks
#         self$channels = s$channels
#         self$srate = s$srate
#         self$badchan = s$badchan
#         self$epichan = s$epichan
#         self$exclchan = s$exclchan
#       }
#
#
#       private$meta$conf = s
#       private$meta$log = l
#     }
#   ),
#   public = list(
#     project_name = NULL,
#     subject_code = NULL,
#     blocks = NULL,
#     channels = NULL,
#     srate = NULL,
#     badchan = NULL,
#     epichan = NULL,
#     exclchan = NULL,
#     valid = FALSE,
#     dirs = NULL,
#     channel_info = NULL,
#
#     # Init
#     initialize = function(project_name, subject_code){
#       self$dirs = get_dir(subject_code = subject_code, project_name = project_name)
#       self$project_name = project_name
#       self$subject_code = subject_code
#       private$reactiveFlag = shiny::reactiveVal()
#
#       if(is.null(project_name) || is.null(subject_code) || subject_code == ''){
#         private$add_error('')
#         self$valid = FALSE
#       }else if(!dir.exists(self$dirs$pre_subject_dir)){  # check if subject is valid
#         private$add_error('Subject NOT Found!')
#         self$valid = FALSE
#       }else{
#         # make sure that preprocess_dir exists
#         get_dir(subject_code = subject_code, project_name = project_name,
#                 mkdirs = 'preprocess_dir')
#         private$settings_file = file.path(self$dirs$preprocess_dir, 'rave.yaml')
#         private$log_file = file.path(self$dirs$preprocess_dir, 'log.csv')
#         private$meta = new.env()
#         private$load_settings()
#
#         # load channel_info if exists
#         channel_info = load_meta('electrodes', self$project_name, self$subject_code)
#         if(is.data.frame(channel_info)){
#           channel_info = channel_info[,1:4]
#           names(channel_info) = c('Channel','EpilepsyChan', 'BadChan', 'ExcludedChan')
#           self$channel_info = channel_info
#         }
#         self$valid = TRUE
#       }
#     },
#     set_blocks = function(blocks){
#       block_folders = list.dirs(self$dirs$pre_subject_dir, full.names = F, recursive = F)
#       is_blocks = (blocks %in% block_folders)
#       if(length(blocks) == 0){
#         self$valid = FALSE
#       }else if(sum(!is_blocks) > 0){
#         invalid_blocks = blocks[!is_blocks]
#         invalid_blocks = str_c(invalid_blocks, collapse = ', ')
#         private$add_error(sprintf('Block(s) "%s" NOT Found!', invalid_blocks))
#         self$valid = FALSE
#       }else{
#         self$blocks = blocks
#       }
#     },
#     set_channels = function(channels, name = 'channels'){
#       if(length(channels) == 0 || !is.numeric(channels)){
#         if(name == 'channels'){
#           self$valid = FALSE
#         }else{
#           assign(name, NULL, envir = self)
#           switch (name,
#                   'epichan' = 'EpilepsyChan',
#                   'badchan' = 'BadChan',
#                   'exclchan' = 'ExcludedChan'
#           ) ->
#             colname
#
#           if(length(colname) == 1){
#             info = self$channel_info
#             if(!is.null(info) && nrow(info) > 0){
#               self$channel_info[, colname] = rep(FALSE, nrow(info))
#             }
#           }
#         }
#       }else{
#         assign(name, channels, envir = self)
#         if(name == 'channels'){
#           # init channel_info as a dataframe
#           if(is.null(self$channel_info)){
#             self$channel_info = data.frame(
#               Channel = channels,
#               EpilepsyChan = FALSE,
#               BadChan = FALSE,
#               ExcludedChan = FALSE
#             )
#           }else{
#             ci = self$channel_info
#             channels = channels[!channels %in% ci$Channel]
#             if(length(channels) > 0){
#               self$channel_info = rbind(
#                 self$channel_info,
#                 data.frame(
#                   Channel = channels,
#                   EpilepsyChan = FALSE,
#                   BadChan = FALSE,
#                   ExcludedChan = FALSE
#                 )
#               )
#               self$channel_info = self$channel_info[order(self$channel_info$Channel), ]
#             }
#           }
#         }else{
#           switch (name,
#                   'epichan' = 'EpilepsyChan',
#                   'badchan' = 'BadChan',
#                   'exclchan' = 'ExcludedChan'
#           ) ->
#             colname
#
#           if(length(colname) > 0){
#             ci = self$channel_info
#             if(is.data.frame(ci)){
#               chls = ci$Channel %in% channels
#               ci[,colname] = chls
#               self$channel_info = ci
#             }
#           }
#         }
#       }
#     },
#     logger = function(..., .list = NULL){
#       args = c(list(...), .list)
#       l = private$meta$log
#       ind = max(l$Index) + 1
#       private$meta$log = rbind(l, data.frame(
#         Index = ind,
#         Modified = str_c(Sys.time()),
#         Action = names(args),
#         Value = str_c(unlist(args)),
#         stringsAsFactors = F
#       ))
#     },
#     configure = function(..., .list = NULL){
#       args = c(list(...), .list)
#       for(name in names(args)){
#         val = args[[name]]
#         private$meta$conf[[name]] = val
#         val = str_c(val, collapse = ', ')
#         n = str_length(val)
#         if(n > 21){
#           val = str_c(str_sub(val, end = 14L), '...', str_sub(val, start = n - 3L))
#         }
#         args[[name]] = val
#       }
#       logger(.list = args)
#     },
#     save = function(){
#       for(name in c(
#         'project_name',
#         'subject_code',
#         'blocks',
#         'channels',
#         'srate',
#         'badchan',
#         'epichan',
#         'exclchan'
#       )){
#         if(name %in% ls(self)){
#           private$meta$conf[[name]] = get(name, envir = self)
#         }
#       }
#
#       # Save conf
#       yaml::write_yaml(private$meta$conf,
#                        file = private$settings_file, fileEncoding = 'UTF-8')
#
#       # save channel_info
#       if(is.data.frame(self$channel_info)){
#         save_meta(self$channel_info, 'electrodes', project_name = self$project_name, subject_code = self$subject_code)
#       }
#
#
#       # write log
#       write.csv(private$meta$log,
#                 private$log_file, row.names = F)
#
#       private$reactiveFlag(str_c(Sys.time()))
#     }
#   ),
#
#   active = list(
#     conf = function(){
#       return(private$meta$conf)
#     },
#     log = function(){
#       return(private$meta$log)
#     },
#     as.reactive = function(){
#       return(private$reactiveFlag)
#     }
#   )
# )





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
    initialize = function(project_name, subject_code){
      self$dirs = get_dir(subject_code = subject_code, project_name = project_name)
      if(!dir.exists(self$dirs$pre_subject_dir)){
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
        name = 'rave.yaml', use_yaml = T
      )
      self$cacher = rave:::RAVEHistory$new(
        path = self$dirs$preprocess_dir,
        name = 'rave.RData', use_yaml = F
      )

      self$blocks = self$logger$get_or_save('blocks')
      self$channels = self$logger$get_or_save('channels')
      self$srate = self$logger$get_or_save('srate')
      self$badchan = self$logger$get_or_save('badchan')
      self$epichan = self$logger$get_or_save('epichan')
      self$exclchan = self$logger$get_or_save('exclchan')

      self$valid = TRUE
    },
    set_blocks = function(blocks){
      blocks = blocks[blocks %in% self$available_blocks]
      self$blocks = blocks
    },
    set_channels = function(channels, name = 'channels'){
      if(name == 'channels'){
        self$channels = channels
      }else{
        channels = channels[channels %in% self$channels]
        assign(name, channels, envir = self)
      }
    },
    save = function(action = '', message = '', ...){
      defaults = data.frame(
        Index = 0,
        Date = strftime(Sys.time(), '%Y-%m-%d %H:%M:%S %Z'),
        Action = 'Initialization',
        Message = sprintf('Subject folder created - %s', tools::file_path_as_absolute(self$dirs$subject_dir)),
        stringsAsFactors = F
      )
      history = self$logger$get_or_save('log', defaults, save = T, inherits = FALSE)
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
