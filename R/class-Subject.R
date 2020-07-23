# documented 2019-11-22

#' @title R6 Class for 'RAVE' Subject 
#' @author Zhengjia Wang
#' @description contains subject meta information after preprocessing.
#' @examples 
#' \dontrun{
#' 
#' # Load subject, use `strict=FALSE` if not sure the existence of raw files 
#' subject <- Subject$new(project_name = 'demo', 'YAB', strict = FALSE)
#' 
#' # Filter 1:14 to see which numbers refer to the valid electrodes
#' subject$filter_valid_electrodes(1:14)
#' #> [1] 13 14
#' 
#' }
#' @export
Subject <- R6::R6Class(
  classname = 'Subject',
  private = list(
    loaded = NULL,
    subjectinfo = NULL
  ),
  public = list(
    
    #' @field meta environment stores subject meta data
    meta = NULL,
    
    #' @field subject_id character, subject ID, generated from project name 
    #' and subject code. For example, project name is \code{"congruency"} and
    #' subject code is \code{"YAB"}, then the \code{subject_id="congruency/YAB"}
    subject_id = NULL,
    
    #' @field subject_code identifier for subject
    subject_code = NULL,
    
    #' @field project_name project name
    project_name = NULL,
    
    #' @field dirs stores folder paths for subject data
    dirs = NULL,
    
    #' @field is_strict whether preprocess directory is checked when 
    #' initializing the instance
    is_strict = TRUE,
    
    #' @description print the information of the subject
    #' @return none
    info = function(){
      cat('<Subject> [', self$subject_id, ']\n - Total electrodes: ', length(self$valid_electrodes), '\n', sep = '')
    },
    
    #' @description override of default print method
    #' @param ... ignored
    #' @return default memory address of the environment
    print = function(...){
      self$info()
    },
    
    #' @description called when garbage collected
    finalize = function(){
      rm(list = ls(private$loaded), envir = private$loaded)
    },
    
    #' @description constructor
    #' @param project_name project name
    #' @param subject_code subject code
    #' @param reference what kind of reference is default for the subject, 
    #' default is "default", referring to \code{"reference_default.csv"} in 
    #' subject meta folder
    #' @param strict whether to check if the raw folder exists
    initialize = function(project_name, subject_code, reference = NULL, 
                          strict = TRUE){
      subject_id = sprintf('%s/%s', project_name, subject_code)
      self$project_name = project_name
      self$subject_code = subject_code
      self$subject_id = subject_id
      self$is_strict = strict
      
      self$meta = list()
      private$loaded = new.env()
      
      # load meta data
      self$dirs = get_dir(subject_code = subject_code, project_name = project_name)
      
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
      private$subjectinfo = SubjectInfo2$new(project_name = project_name,
                                             subject_code = subject_code, strict = strict)
    },
    
    #' @description Obtain preprocessing information.
    #' This methods is rarely directly called, I wrap up most commonly used 
    #' fields in other functions
    #' @param key the fields or items store in \code{SubjectInfo2}
    #' instance
    #' @param default default value if the key is not found
    #' @param customized indicates whether the key refers to additional items 
    #' or fields in \code{SubjectInfo2}. Default is false, meaning 
    #' the key is the fields.
    #' @return the preprocess information correspond to the key
    preprocess_info = function(key, default = NULL, customized = FALSE){
      if(customized){
        res = private$subjectinfo$logger$get_or_save(key = key)
      }else{
        res = private$subjectinfo[[key]]
      }
      res %?<-% default
      return(res)
    },
    
    #' @description filter, and returns existing electrodes
    #' @param electrodes integer vector
    #' @return the electrodes that the subject has, including bad, or invalid
    #' electrodes.
    filter_all_electrodes = function(electrodes){
      electrodes = electrodes[electrodes %in% private$subjectinfo$channels]
      electrodes
    },
    
    #' @description filter, and returns valid electrodes
    #' @param electrodes integer vector
    #' @return the valid electrodes. Invalid electrodes refers to bad 
    #' electrodes, or the end of bipolar reference. If \code{"Reference"} 
    #' column is blank in the reference file, then the electrode is invalid. 
    filter_valid_electrodes = function(electrodes){
      electrodes[electrodes %in% self$valid_electrodes]
    },
    
    
    #' @description (deprecated) check whether the selected time is excluded
    #' @param block block name
    #' @param electrode electrode number
    #' @param start start time
    #' @param end end time
    has_bad_time_point = function(block, electrode, start, end){
      nrow(
        subset(
          self$meta[['time_excluded']],
          Block %in% block &
            Electrode %in% electrode &
            (start < End | end < Start)
        )
      ) ->
        res
      return(res > 0)
    }
  ),
  active = list(
    
    #' @field electrodes electrode table (read-only)
    electrodes = function(){
      self$meta[['electrode']]
    },
    
    #' @field frequencies frequency table (read-only)
    frequencies = function(){
      self$meta[['frequency']]
    },
    
    #' @field time_points time-point table (read-only)
    time_points = function(){
      self$meta[['time_points']]
    },
    
    
    #' @field time_excluded (deprecated) excluded time-point table (read-only)
    time_excluded = function(){
      self$meta[['time_excluded']]
    },
    
    
    #' @field sample_rate time-point table (read-only, for compatibility issues)
    sample_rate = function(){
      self$meta[['sample_rate']]
    },
    
    #' @field volt_sample_rate voltage (trace) sampling rate in Hertz 
    #' (read-only)
    volt_sample_rate = function(){
      self$preprocess_info('srate')
    },
    
    #' @field power_sample_rate power (amplitude) sampling rate in Hertz
    #' (read-only)
    power_sample_rate = function(){
      self$meta[['sample_rate']]
    },
    
    #' @field phase_sample_rate phase sampling rate in Hertz (read-only)
    phase_sample_rate = function(){
      self$meta[['sample_rate']]
    },
    
    #' @field valid_electrodes all valid electrodes in current reference scheme 
    #' (read-only)
    valid_electrodes = function(){
      tbl = self$meta[['electrode']]
      if(is.data.frame(tbl) && 'Reference' %in% names(tbl)){
        re = tbl$Electrode[tbl$Reference != '']
      }else{
        re = private$subjectinfo$channels
      }
      re
    },
    
    #' @field id read-only version of subject ID
    id = function(){
      self$subject_id
    }
  )
)


#' Convert subject to python object
#' @param obj Subject class
#' @param convert pass to \code{r_to_py} in \code{reticulate} package
#' @export
r_to_py.Subject <- function(obj, convert = FALSE){
  if(requireNamespace('reticulate', quietly = TRUE)){
    reticulate::r_to_py(list(
      subject_id = obj$id,
      electrodes = obj$electrodes,
      frequencies = obj$frequencies,
      sample_rate = obj$meta$sample_rate,
      valid_electrodes = obj$valid_electrodes,
      dirs = obj$dirs[c('rave_dir', 'meta_dir', 'cache_dir', 'suma_dir', 'suma_out_dir')]
    ), convert = convert)
  }
}

# Conver subject to JSON format
# Preserved for jsonlite
# asJSON.Subject <- function(obj){
#   list(
#     subject_id = obj$id,
#     electrodes = obj$electrodes,
#     frequencies = obj$frequencies,
#     sample_rate = obj$meta$sample_rate,
#     valid_electrodes = obj$valid_electrodes
#     # dirs = obj$dirs[c('rave_dir', 'meta_dir', 'cache_dir', 'suma_dir', 'suma_out_dir')]
#   )
# }



#' @export
as.character.Subject <- function(x, ...){
  x$id
}

#' @title Make new subject object from character
#' @param subject characters in format \code{"project/subject"}
#' @param strict logical indication whether preprocess folder is needed
#' @param reference what reference file the subject is using
#' @export
as_subject <- function(subject, strict = TRUE, reference = 'default'){
  if(is.character(subject)){
    
    sub_dir = file.path(rave_options('data_dir'), subject, 'rave')
    if(!dir.exists(sub_dir)){
      stop('Subject ', subject, ' not found')
    }
    
    s = stringr::str_split_fixed(subject, '/', n = 2)
    s = unlist(s)
    subject = Subject$new(project_name = s[1], subject_code = s[2], strict = strict, reference = reference)
  }
  
  subject
}
