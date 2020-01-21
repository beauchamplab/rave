# Support of NWB format
# nwb_file = normalizePath('~/rave_data/raw_dir/NWBDEMO/EC125_B22.nwb'); nwb_file


NWB_parser <- function(nwb_file, verbose = FALSE, progress = NULL){

  inc = function(...){
    if(!is.null(progress)){
      progress$inc(paste0(...))
    }else if(verbose){
      dipsaus::cat2(...)
    }
  }

  this_env = environment()



  # Get all fields
  parse = function(obj, parent_path = NULL){

    container = new.env(parent = emptyenv())

    tbl = obj$ls()
    obj_type = obj$get_obj_type(); obj_type = as.character(obj_type)

    if(obj_type == 'H5I_FILE'){
      on.exit({
        class(container) = c('rave_nwb_file', 'rave_nwb_container', 'environment')
      })
    }else if(obj_type == 'H5I_GROUP'){
      on.exit({
        class(container) = c('rave_nwb_group', 'rave_nwb_container', 'environment')
      })
    }

    n = nrow(tbl)
    if(!n){
      return(container)
    }

    # recursive view
    lapply(seq_len(n), function(ii){
      row = tbl[ii, ]
      pth = paste(c(parent_path, row$name), collapse = '/')
      name = row$name
      inc('Loading - ', pth)
      if(row$obj_type == 'H5I_GROUP'){
        # this is a group
        g = parse(obj[[name]], parent_path = c(parent_path, name))

        # get group attributes
        nattr = obj[[name]]$attr_get_number()
        if(nattr){
          for(jj in seq_len(nattr) - 1){
            attr_name = obj[[name]]$attr_name_by_idx(jj, '.')
            if(verbose){
              dipsaus::cat2('\t', attr_name)
            }

            attr_value = obj[[name]]$attr_open_by_idx(jj, '.')$read()
            attr(g, attr_name) = attr_value
          }
        }
        container[[name]] = nwb_post_process(g, name)

      }else if(row$obj_type == 'H5I_DATASET'){
        dat = rave::load_h5(file = nwb_file, name = pth, read_only = TRUE)

        # get group attributes
        nattr = obj[[name]]$attr_get_number()
        if(nattr){
          for(jj in seq_len(nattr) - 1){
            attr_name = obj[[name]]$attr_name_by_idx(jj, '.')
            if(verbose){
              dipsaus::cat2('\t', attr_name)
            }
            attr_value = obj[[name]]$attr_open_by_idx(jj, '.')$read()
            attr(dat, attr_name) = attr_value
          }
        }
        class(dat) = c('rave_nwb_data', class(dat))
        container[[name]] = nwb_post_process(dat, name)
      }
    })

    container

  }

  h5file = hdf5r::H5File$new(filename = nwb_file, mode = 'r')
  h5file$close_all()
  h5file = hdf5r::H5File$new(filename = nwb_file, mode = 'r')

  on.exit({
    h5file$close()
  })

  container = parse(h5file)
  attr(container, 'file_path') = nwb_file
  container
}




print.rave_nwb_data <- function(x, ...){
  if(length(x) < 1e3){
    print(x[])
  }else{
    NextMethod('print')
  }
  invisible(x)
}

print.rave_nwb_container = function(x, ...){
  cat = function(...){
    base::cat(..., sep = '', end = '\n')
  }
  cat('  In-RAM size: ', as.character(dipsaus::to_ram_size(pryr::object_size(x))))
  cat('')
  # ---- attributes
  def = attr(x, 'NWB_DEF')
  if(length(def)){
    cat("Definition:")
    print(def)
  }

  # ---- Fields
  cat("Fields:")
  smry = lapply(names(as.list(x, all.names = TRUE)), function(nm){
    sub = .subset2(x, nm)
    cls = class(sub)
    if('rave_nwb_group' %in% cls){
      data.frame(stringsAsFactors = FALSE,
                 Name = nm,
                 Type = 'Group',
                 Length = length(sub),
                 Dim = '')
    }else{
      dd = dim(sub)
      if(length(dd)){
        dd = paste(dd, collapse = ' x ')
      }else{
        dd = ''
      }
      data.frame(stringsAsFactors = FALSE,
                 Name = nm,
                 Type = 'Data',
                 Length = length(sub),
                 Dim = dd)
    }
  })

  print(do.call(rbind, smry))

  invisible(x)
}

print.rave_nwb_file = function(x, ...){
  file_path = attr(x, 'file_path')#.subset2(x, '...file_path')

  cat = function(...){
    base::cat(..., sep = '', end = '\n')
  }

  cat('File path: ', file_path)
  cat('  Format: NWB File')
  NextMethod('print')
}


print.rave_nwb_group = function(x, ...){
  cat = function(...){
    base::cat(..., sep = '', end = '\n')
  }

  cat('Format: NWB Group')
  NextMethod('print')
}


list_nwb <- function(x, level = 0, concat = ' > ', parent = NULL){

  if(!'rave_nwb_container' %in% class(x)){
    return(NULL)
  }

  nms = names(as.list(x, all.names = TRUE))
  sapply(nms, function(nm){
    if(!isFALSE(concat)){
      re = paste0(c(parent, nm), collapse = concat)
    }else{
      re = nm
    }



    if(level){
      re_sub = list_nwb(x[[nm]], concat = concat, level = level - 1, parent = c(parent, nm))
      re_sub = unlist(re_sub)
      re = c(re, re_sub)
    }
    re
  }, USE.NAMES = FALSE, simplify = FALSE) ->
    re

  unlist(re)
}



nwb_post_process <- function(container, name){
  # check attributes
  namespace = attr(container, 'namespace')
  neurodata_type = attr(container, 'neurodata_type')
  is_data = 'rave_nwb_data' %in% class(container)

  def = NULL
  if(!length(namespace) || !length(neurodata_type)){
    def = NWB_dataset_def(name, neurodata_type_def = NULL, namespace = 'core')
  } else{
    def = NWB_dataset_def(name, neurodata_type_def = neurodata_type, namespace = namespace)
    if(is.null(def) && namespace != 'core'){
      namespace = 'core'
      def = NWB_dataset_def(name, neurodata_type_def = neurodata_type, namespace = 'core')
    }
  }
  if(length(def)){
    attr(container, 'NWB_DEF') = def
  }

  return(container)

}


# list_nwb(x, level = Inf, concat = FALSE)


# nwbfile = NWB_parser(nwb_file)
# ns = NWB_namespace('ecog')
#
# def = NWB_dataset_def('subject', neurodata_type_def = NULL, namespace = 'ecog'); def
# ns$containers
# x = NWB_parser(nwb_file)
# x
# x$acquisition$button_press$data
#

