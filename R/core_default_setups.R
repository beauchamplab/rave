# functions to create module,data,rawdata dirs

#' Initialize data repository
#' @param first_time will create data repositories for you
#' @param reset reset to default data directory
#' @export
arrange_data_dir <- function(first_time = F, reset = F){
  if(first_time || reset){
    data_dir = '~/rave_data/data_dir'
    raw_dir = '~/rave_data/raw_dir'

    dir.create(data_dir, showWarnings = F, recursive = T)
    dir.create(raw_dir, showWarnings = F, recursive = T)

    # Russian rocket: RE-DO whenever fails
    file.copy(system.file('data/data_dir', package = 'rave'), '~/rave_data/', recursive = T, overwrite = T)
    file.copy(system.file('data/raw_dir', package = 'rave'), '~/rave_data/', recursive = T, overwrite = T)
  }

  if(reset){
    rave_options(
      data_dir = data_dir,
      raw_data_dir = raw_dir
    )
  }

  data_dir = rave_options('data_dir')
  raw_dir = rave_options('raw_data_dir')

  if(!dir.exists(data_dir) || !dir.exists(raw_dir)){
    logger('Cannot find data directory for RAVE. Please make sure that these folder exists', level = 'ERROR')
    logger(data_dir, level = 'ERROR')
    logger(raw_dir, level = 'ERROR')
    logger('Check existence of these folders, or reset default data repository by typing arrange_data_dir(reset = T)', level = 'ERROR')
    return(F)
  }else{
    rave_options(data_dir = base::normalizePath(data_dir))
    rave_options(raw_data_dir = base::normalizePath(raw_dir))

    # Test speed
    if(!isTRUE(rave_options('disable_startup_speed_check'))){
      speed = test_hdspeed(quiet = T)
      rave_options(drive_speed = speed)
    }

    return(T)
  }

}

#' Update (optional), and check validity of modules
#' @param refresh check and updates file
#' @param reset same as first_time, check module updates, ignored
#' @param quiet no overwrite messages
#' @export
arrange_modules <- function(refresh = FALSE, reset = FALSE, quiet = FALSE){

  col_names = c('ID', 'Name', 'Group', 'Package', 'Active', 'Notes')
  fpath = rave_options('module_lookup_file')
  dirname = dirname(fpath)
  if(!dir.exists(dirname)){
    dir.create(dirname, showWarnings = FALSE, recursive = TRUE)
  }
  if(!file.exists(fpath)){
    logger('First time? looking for modules', quiet = quiet)
  }

  old_table = NULL


  if(!reset && file.exists(fpath)){
    tbl = read.csv(fpath, stringsAsFactors = F)
    nms = names(tbl)
    if(!all(col_names %in% nms)){
      refresh = TRUE
    }else{
      tbl = tbl[, col_names]
      old_table = tbl
    }
  }

  if(refresh || reset){
    logger('Trying to locate all possible modules...', quiet = quiet)

    # detect all modules
    tbl = detect_modules(as_module = FALSE)
    tbl = as.data.frame(tbl, stringsAsFactors = F)

    # if(nrow(tbl) == 0){
    #   stop("No module detected. Please install\n\tdevtools::install_github('beauchamplab/ravebuiltins', upgrade = 'never')\nand then run \n\trave::arrange_modules(refresh = TRUE)")
    # }
    #

    if(!nrow(tbl)){
      logger('No modules can be found. Installing builtin modules using \n\tremotes::install_github("beauchamplab/ravebuiltins")', level = 'ERROR')
      devtools::install_github("beauchamplab/ravebuiltins", upgrade = 'never')

      tbl = detect_modules(as_module = FALSE)
      tbl = as.data.frame(tbl, stringsAsFactors = F)
    }
    names(tbl) = col_names[1:4]



    # join
    if(is.data.frame(old_table) && !reset){
      tbl = merge(tbl, old_table, by = names(tbl), all.x = TRUE, no.dups = T)
      tbl$Active[is.na(tbl$Active)] = TRUE
      tbl$Notes[is.na(tbl$Notes)] = ''
    }else{
      tbl$Active = TRUE
      tbl$Notes = ''
    }

    # try for each module, validate
    for(ii in seq_len(nrow(tbl))){
      row = tbl[ii, ]
      if(row$Active){
        module_id = row$ID
        package_name = row$Package
        logger('Validating [', module_id, '] (Package - ', package_name, ')', level = 'INFO', quiet = quiet)
        m = get_module(package_name, module_id)

        if(!any(c('ModuleEnvir', 'R6') %in% class(m))){
          tbl[ii, 'Active'] = FALSE
          tbl[ii, 'Notes'] = 'Cannot initialize'
        }
      }
    }

    # write to fpath
    safe_write_csv(tbl, fpath, row.names = FALSE)
  }

  tbl$Notes[is.na(tbl$Notes)] = ''

  tbl
}


