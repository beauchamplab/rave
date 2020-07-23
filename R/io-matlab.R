
pre_import_matlab <- function(subject_code, project_name, block_num, chl, name = 'analogTraces'){
  dir = get_dir(subject_code = subject_code, project_name = project_name, block_num = block_num)
  
  fpath = file.path(dir$block_dir, sprintf('%sDatafile%s_ch%d.mat', subject_code, block_num, chl))
  
  if(!file.exists(fpath)){
    # file not exists, no strict mode, need to guess file name
    # use _chXX.mat as key word
    fs = list.files(dir$block_dir, sprintf('ch%d.[mM][aA][tT]', chl), full.names = TRUE, recursive = FALSE)
    if(length(fs)){
      fpath = fs[1]
    }
  }
  dat = read_mat(fpath)
  
  nms = names(dat)
  if(name %in% nms){
    x = dat[[name]]
  }else{
    guess = sapply(nms, function(nm){
      try({
        dim = dim(dat[[nm]])
        if(length(dim) <= 2){
          if(any(dim == 1)){
            return(c(1, length(dat[[nm]])))
          }
        }
      }, silent = TRUE)
      c(0, 0)
    })
    
    if( any(guess[1,] == 1) ){
      sel = guess[1,] == 1
      idx = which(guess[2,] == max(guess[2,][sel]))[[1]]
      x = dat[[nms[idx]]]
    }else{
      x = dat[[1]]
    }
  }
  
  
  return(as.vector(x))
}


