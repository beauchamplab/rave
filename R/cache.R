#' Cache Subject Electrodes
#' @description Create cache for subjects
#' @param subject_id Folder name for subject
#' @param electrodes \code{NULL} by default, meaning caching all valid electrodes. Or you can specify a
#'   vector of integers indicating the number of electrodes to be cached (usually for broken cache files)
#' @param use_rhdf5 \code{FALSE} by default. HDF5 is still under test. Don't change it.
#' @export
cache_subject <- function(subject_id, electrodes = NULL, use_rhdf5 = FALSE){
  if(use_rhdf5){
    v = rave_opts$get_options('use_rhdf5')
    on.exit(rave_opts$set_options(use_rhdf5 = v))

    rave_opts$set_options(use_rhdf5 = 'TRUE')
  }


  subject <- rave:::prophet$get_subject(subject_id)
  prophet$reset()

  if(is.null(electrodes)){
    electrodes = subject$electrodes$Number
  }

  electrodes = electrodes[electrodes %in% subject$valid_electrodes]

  for(i in electrodes){
    print(i)
    e <- SingleElectrode$new(subject, i)
    rm(e)
  }

}

