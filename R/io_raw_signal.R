#' @description Import raw voltage signals from matlab file
#' @param subject_code subject code
#' @param block_num which block of signals will be looked into?
#' @param chl integer, channel number
#' @return raw signal
#' @import R.matlab
pre_import_matlab <- function(subject_code, project_name, block_num, chl, name = 'analogTraces'){
  dir = get_dir(subject_code = subject_code, project_name = project_name, block_num = block_num)
  file.path(
    dir$block_dir, sprintf(
      '%sDatafile%s_ch%d.mat', subject_code, block_num, chl
    )
  ) %>%
  R.matlab::readMat() ->
    dat

  x = as.vector(dat[[name]])

  return(x)
}


