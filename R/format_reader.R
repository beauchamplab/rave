
import_from_csv = function(file_path, type, ...){
  header = type %in% c('trial')
  if(type == 'content'){
    stop('Reading ECOG signal data NOT IMPLEMENTED...')
  }
  read.csv(file_path, header = header, stringsAsFactors = F, ...)
}

# Read matlab file, must be <= -V6 mat format

import_from_mat = function(file_path, type, ...){
  if(!type %in% c('content', 'data')){
    stop('Currently only ECOG signal data is supported...')
  }

  logger('loading matlab file: ', file_path)
  dat = R.matlab::readMat(file_path)
  if(is.list(dat)){
    return(dat[[1]])
  }else{
    return(dat)
  }
}







