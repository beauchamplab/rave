# subject class

#' @export
Subject <- R6::R6Class(
  classname = 'Subject',
  private = list(
    loaded = NULL,
    subjectinfo = NULL
  ),
  public = list(
    meta = NULL,
    subject_id = NULL,
    subject_code = NULL,
    project_name = NULL,
    dirs = NULL,
    info = function(){
      cat('<Subject> [', self$subject_id, ']\n - Total electrodes: ', length(self$valid_electrodes), '\n', sep = '')
    },
    print = function(){
      pryr::address(self)
    },
    finalize = function(){
      rm(list = ls(private$loaded), envir = private$loaded)
    },
    initialize = function(project_name, subject_code, reference = NULL){
      subject_id = sprintf('%s/%s', project_name, subject_code)
      self$project_name = project_name
      self$subject_code = subject_code
      self$subject_id = subject_id

      self$meta = list()
      private$loaded = new.env()

      # load meta data
      self$dirs = get_dir(subject_id = subject_id)

      # meta_dir = self$dirs$meta_dir
      #
      # self$meta[['electrode']] = read.csv(file.path(meta_dir, 'electrodes.csv'), stringsAsFactors = F)
      # self$meta[['frequency']] = read.csv(file.path(meta_dir, 'frequencies.csv'), stringsAsFactors = F)
      es = load_meta('electrodes', project_name = project_name, subject_code = subject_code)
      if(is.character(reference)){
        ref = load_meta('references', project_name = project_name, subject_code = subject_code, meta_name = reference)
        if(is.data.frame(ref)){
          es = merge(es, ref, by = 'Electrode')
        }
      }
      self$meta[['electrode']] = es
      self$meta[['frequency']] = load_meta('frequencies', project_name = project_name, subject_code = subject_code)

      self$meta[['time_points']] = load_meta('time_points', project_name = project_name, subject_code = subject_code)

      tm = self$meta[['time_points']]$Time[1:2]
      self$meta[['sample_rate']] = sample_rate = 1 / (tm[2] - tm[1])


      self$meta[['time_excluded']] = load_meta('time_excluded', project_name = project_name, subject_code = subject_code)

      # load preprocess subject info
      private$subjectinfo = SubjectInfo2$new(project_name = project_name, subject_code = subject_code)

    },
    preprocess_info = function(key, default = NULL, customized = F){
      if(customized){
        res = private$subjectinfo$logger$get_or_save(key = key)
      }else{
        res = private$subjectinfo[[key]]
      }
      res %?<-% default
      return(res)
    },
    filter_all_electrodes = function(electrodes){
      electrodes = electrodes[electrodes %in% private$subjectinfo$channels]
      electrodes
    },
    filter_valid_electrodes = function(electrodes){
      electrodes[electrodes %in% self$valid_electrodes]
    },
    has_bad_time_point = function(block, electrode, start, end){
      (self$meta[['time_excluded']]) %>%
        subset(
          Block %in% block &
          Electrode %in% electrode &
          (start < End | end < Start)
        ) %>%
        nrow() ->
        res
      return(res > 0)
    }
  ),
  active = list(
    electrodes = function(){
      self$meta[['electrode']]
    },
    frequencies = function(){
      self$meta[['frequency']]
    },
    time_points = function(){
      self$meta[['time_points']]
    },
    time_excluded = function(){
      self$meta[['time_excluded']]
    },
    sample_rate = function(){
      self$meta[['sample_rate']]
    },
    valid_electrodes = function(){
      tbl = self$meta[['electrode']]
      if(is.data.frame(tbl) && 'Reference' %in% names(tbl)){
        re = tbl$Electrode[tbl$Reference != '']
      }else{
        re = private$subjectinfo$channels
      }
      re
    },
    id = function(){
      self$subject_id
    }
  )
)



#' @export
r_to_py.Subject <- function(obj, convert = FALSE){
  reticulate::r_to_py(list(
    subject_id = obj$id,
    electrodes = obj$electrodes,
    frequencies = obj$frequencies,
    sample_rate = obj$meta$sample_rate,
    valid_electrodes = obj$valid_electrodes,
    dirs = obj$dirs[c('rave_dir', 'meta_dir', 'cache_dir', 'suma_dir', 'suma_out_dir')]
  ), convert = convert)
}
