# subject class

#' @export
#' @exportClass Subject
Subject <- R6::R6Class(
  classname = 'Subject',
  private = list(
    loaded = NULL
  ),
  public = list(
    meta = NULL,
    subject_id = NULL,
    dirs = NULL,
    print = function(){
      cat('<Subject> [', self$subject_id, ']\n - Total electrodes: ', length(self$valid_electrodes), '\n', sep = '')

    },
    finalize = function(){
      rm(list = ls(private$loaded), envir = private$loaded)
    },
    initialize = function(subject_id){
      self$subject_id = subject_id
      self$meta = list()
      private$loaded = new.env()

      # load meta data
      self$dirs = get_dir(subject_id = subject_id)

      meta_dir = self$dirs$meta_dir

      self$meta[['electrode']] = read.csv(file.path(meta_dir, 'electrodes.csv'), stringsAsFactors = F)
      self$meta[['frequency']] = read.csv(file.path(meta_dir, 'frequencies.csv'), stringsAsFactors = F)
      self$meta[['time']] = read.csv(file.path(meta_dir, 'time_points.csv'), stringsAsFactors = F, colClasses = c('character', 'numeric'))
      tm = self$meta[['time']]$Time[1:2]
      self$meta[['sample_rate']] = 1 / (tm[2] - tm[1])
      # self$meta[['info']] = load_meta('info', subject_id = subject_id)
    },
    filter_valid_electrodes = function(electrodes){
      electrodes[electrodes %in% self$valid_electrodes]
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
      self$meta[['time']]
    },
    sample_rate = function(){
      self$meta[['sample_rate']]
    },
    valid_electrodes = function(){
      e = self$meta[['electrode']]
      sel = e$BadChan | e$EpilepsyChan
      e$Channel[!sel]
    },
    id = function(){
      self$subject_id
    }
  )
)
