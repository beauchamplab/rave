
#' @import rhdf5
#' @export
SingleElectrode <- R6::R6Class(
  'SingleElectrode',

  cloneable = FALSE,
  portable = FALSE,

  private = list(
  ),

  public = list(
    subject = NULL,
    electrode = NULL,
    power = NULL,
    phase = NULL,
    cut = NULL,
    cache_path = NULL,


    initialize = function(subject, electrode){
      # locate source file
      data_dir = rave_options('data_dir')
      subject_dir = file.path(data_dir, subject, 'rave')
      cache_path = file.path(subject_dir, 'cache', sprintf('chl_%d.h5', electrode))

      # assume the file is ready
      self$cache_path = tools::file_path_as_absolute(cache_path)

      # load power
      self$power = rhdf5::h5read(file = cache_path, name = 'power')

      # TODO: load phase

      # load meta
      # TODO load time cut
      # Here I use ad-hoc template


    }
  )
)

