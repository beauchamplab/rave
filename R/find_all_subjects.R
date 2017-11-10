#' @include options.R

find_all_subjects <- function(){
  subjects <- list.files(rave_opts$get_options('data_dir'))
  subjects
}
