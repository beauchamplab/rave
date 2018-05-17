# subject class

#' @export
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

      time_points = read.csv(file.path(meta_dir, 'time_points.csv'), stringsAsFactors = F, colClasses = c('character', 'numeric'))
      time_points$Valid = T

      tm = time_points$Time[1:2]
      self$meta[['sample_rate']] = sample_rate = 1 / (tm[2] - tm[1])

      time_excluded_path = file.path(meta_dir, 'time_excluded.csv')
      if(file.exists(time_excluded_path)){
        time_excluded = read.csv(file.path(meta_dir, 'time_excluded.csv'), stringsAsFactors = F, colClasses = c('character', 'numeric', 'numeric'))
        for(i in 1:nrow(time_excluded)){
          b = time_excluded$Block[i]
          s = time_excluded$Start[i]
          e = time_excluded$End[i]
          sel = time_points$Block == b & time_points$Time %within% c(s, e)
          if(sum(sel)){
            time_points$Valid[sel] = FALSE
          }
        }
      }
      self$meta[['time']] = time_points
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
    },
    project_name = function(){
      unlist(str_split(self$subject_id, '/'))[1]
    },
    subject_code = function(){
      unlist(str_split(self$subject_id, '/'))[2]
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
