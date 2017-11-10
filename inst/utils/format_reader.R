# functions to import from raw files
# For more format, you can customize it.
# For e.g., filename has format 'XXX.txt'
# 1. Change ecogdata.XXX_label_format := 'txt'
# 2. append function `import_from_txt` here in this file with two params: file_path, and type
# `file_path`: example: ./data/XXX_Subject/ecog/freq.txt
# `type`: 'frequency'

# import_from_csv file
import_from_csv = function(file_path, type, ...){
  header = type %in% c('trial')
  if(type == 'content'){
    stop('Reading ECOG signal data NOT IMPLEMENTED...')
  }
  read.csv(file_path, header = header, stringsAsFactors = F, ...)
}

# Read matlab file, must be <= -V6 mat format
import_from_mat = function(file_path, type, ...){
  if(type != 'content'){
    stop('Currently only ECOG signal data is supported...')
  }
  
  utils$logger('loading matlab file: ', file_path)
  
  require(R.matlab)
  dat = readMat(file_path)
  if(is.list(dat)){
    return(dat[[1]])
  }else{
    return(dat)
  }
}







