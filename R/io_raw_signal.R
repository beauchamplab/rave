#' mport raw voltage signals from matlab file
#' @param subject_code subject code
#' @param block_num which block of signals will be looked into?
#' @param chl integer, channel number
#' @return raw signal
pre_import_matlab <- function(subject_code, project_name, block_num, chl, name = 'analogTraces'){
  dir = get_dir(subject_code = subject_code, project_name = project_name, block_num = block_num)

  fpath = file.path(
    dir$block_dir, sprintf(
      '%sDatafile%s_ch%d.mat', subject_code, block_num, chl
    )
  )

  if(!file.exists(fpath)){
    # file not exists, no strict mode, need to guess file name
    # use _chXX.mat as key word
    fs = list.files(dir$block_dir, sprintf('_ch%d.[mM][aA][tT]', chl), full.names = T, recursive = F)
    if(length(fs)){
      fpath = fs[1]
    }
  }
  R.matlab::readMat(fpath) ->
    dat

  x = as.vector(dat[[name]])

  return(x)
}


