#' @description Save objects to H5 file without trivial checkings
#' @import rhdf5
#' @import stringr
save_h5 <- function(x, file, name, chunk, level = 7,replace = FALSE, new_file = FALSE, ctype = NULL){

  # Create file if needed
  if(new_file){
    if(file.exists(file)){
      file.remove(file)
    }
  }

  if(!file.exists(file)){
    h5createFile(file)
  }

  # create group if needed
  info = h5ls(file)


  if(str_sub(name, end=1) != '/'){
    name = str_c('/', name)
  }

  group = as.vector(str_split(name, '/',simplify = T))[-1]
  gl = length(group)
  g = ''

  H5close()
  if(gl > 1){
    for(i in 1:(gl - 1)){
      flag = TRUE
      subg = group[i]
      g = str_c(g, '/', subg)
      sel = str_detect(info$otype, 'GROUP') & info$name == subg

      if(
        sum(sel) == 0 || !g %in% as.vector(sprintf('%s/%s', info$group[sel], info$name[sel]))
      ){
        suppressMessages(h5createGroup(file, g))
        logger('[HDF5] ', file, ' -> ', g, ' (Group) created.')
      }
    }
  }else{
    g = '/'
  }



  # Create dataset
  info = h5ls(file)
  dname = group[gl]

  data_exists = sum(
    # has the dataset
    (info$name == dname) &
      # in that group
      (info$group == g) &
      # is not a group
      str_detect(info$otype, 'DATASET')) > 0
  if(!data_exists){
    dim = dim(x)
    if(is.null(dim)){ dim = length(x) }

    ctype = get('ctype', envir = environment())
    if(is.null(ctype)){
      if(is.character(x)){
        ctype = 'character'
      }else if(is.numeric(x)){
        ctype = 'double'
      }else{
        logger('You MUST provide storage.mode if x is not numerics or characters. ',
               'Trying double type',
               level = 'WARNING')
        ctype = 'double'
      }
    }
    H5close()
    h5createDataset(file, dataset = name, dims = dim,
                    storage.mode = ctype, chunk = chunk, level = level)
    logger('[HDF5] ', file, ' -> ', name, ' (Dataset) created.')
  }

  H5close()

  if((data_exists & replace) || (!data_exists)){
    h5write(x, file, name)
    if(!data_exists){
      logger('[HDF5] ', file, ' -> ', name, ' (Data) saved.')
    }else{
      logger('[HDF5] ', file, ' -> ', name, ' (Data) replaced')
    }
  }

  H5close()
}







