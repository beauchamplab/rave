# functions to create module,data,rawdata dirs

#' Initialize data repository
#' @param first_time will create data repositories for you
#' @param reset reset to default data directory
#' @export
arrange_data_dir <- function(first_time = FALSE, reset = FALSE){
  if(first_time || reset){
    data_dir = '~/rave_data/data_dir'
    raw_dir = '~/rave_data/raw_dir'
    
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Russian rocket: RE-DO whenever fails
    file.copy(system.file('data/data_dir', package = 'rave'), '~/rave_data/', recursive = TRUE, overwrite = TRUE)
    file.copy(system.file('data/raw_dir', package = 'rave'), '~/rave_data/', recursive = TRUE, overwrite = TRUE)
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
    catgl('Cannot find data directory for RAVE. Please make sure that these folder exists', level = 'ERROR')
    catgl(data_dir, level = 'ERROR')
    catgl(raw_dir, level = 'ERROR')
    catgl('Check existence of these folders, or reset default data repository by typing arrange_data_dir(reset = TRUE)', level = 'ERROR')
    return(FALSE)
  }else{
    rave_options(data_dir = base::normalizePath(data_dir))
    rave_options(raw_data_dir = base::normalizePath(raw_dir))
    
    # Test speed
    if(!isTRUE(rave_options('disable_startup_speed_check'))){
      speed = test_hdspeed(quiet = TRUE)
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
    catgl('First time? looking for modules', quiet = quiet)
    refresh = TRUE
  }
  
  old_table = NULL
  
  
  if(!reset && file.exists(fpath)){
    tbl = utils::read.csv(fpath, stringsAsFactors = F)
    nms = names(tbl)
    if(!all(col_names %in% nms)){
      refresh = TRUE
    }else{
      tbl = tbl[, col_names]
      old_table = tbl
    }
  }
  
  if(refresh || reset){
    catgl('Trying to locate all possible modules...', quiet = quiet)
    
    # detect all modules
    tbl = detect_modules(as_module = FALSE)
    tbl = as.data.frame(tbl, stringsAsFactors = F)
    
    # if(nrow(tbl) == 0){
    #   stop("No module detected. Please install\n\tdevtools::install_github('beauchamplab/ravebuiltins', upgrade = 'never')\nand then run \n\trave::arrange_modules(refresh = TRUE)")
    # }
    #
    
    if(!nrow(tbl)){
      catgl('No modules can be found. Installing builtin modules using \n\tdevtools::install_github("beauchamplab/ravebuiltins@dev")', level = 'ERROR')
      devtools::install_github("beauchamplab/ravebuiltins@dev", upgrade = 'never')
      
      tbl = detect_modules(as_module = FALSE)
      tbl = as.data.frame(tbl, stringsAsFactors = F)
    }
    names(tbl) = col_names[1:4]
    
    
    
    # join
    if(is.data.frame(old_table) && !reset){
      tbl = merge(tbl, old_table, by = names(tbl), all.x = TRUE, no.dups = TRUE)
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
        catgl('Validating [', module_id, '] (Package - ', package_name, ')', level = 'INFO', quiet = quiet)
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





#' Find all Available RAVE Projects
#' @return A vector of RAVE project names in characters
#' @export
get_projects <- function(){
  data_dir = rave_options('data_dir')
  pj = list.dirs(data_dir, full.names = F, recursive = F)
  pj[stringr::str_detect(pj, '^[a-zA-Z0-9]') & pj != 'rave']
}

#' Get all subjects within project
#' @param project_name project
#' @param check_subfolders logical, check whether folder `rave` exists in subject folder, default true
#' @param check_rawdata logical, whether raw subject folder exists, default false
#' @export
get_subjects <- function(project_name, check_subfolders = TRUE, check_rawdata = FALSE){
  data_dir = rave_options('data_dir')
  sub = list.dirs(file.path(data_dir, project_name), full.names = F, recursive = F)
  sub = sub[stringr::str_detect(sub, '^[a-zA-Z0-9]') & sub!= 'rave']
  if( check_subfolders ){
    sub = sub[dir.exists(file.path(data_dir, project_name, sub, 'rave'))]
  }
  if( check_rawdata ){
    raw_dir = rave_options('raw_data_dir')
    sub = sub[dir.exists(file.path(raw_dir, sub))]
  }
  sub
}

#' Get Directories in `RAVE`
#' @param subject_code subject code; can be ignored when \code{subject_id} is provided
#' @param project_name project name; can be ignored when \code{subject_id} is provided
#' @param block_num block name (optional)
#' @param mkdirs internally used
#' @param subject_id subject ID; can be omitted if \code{subject_code} and 
#' \code{project_name} are provided
#' @param relative whether to return relative path or absolute to root directory
#' @export
get_dir <- function(subject_code, project_name, block_num, mkdirs = NULL, subject_id, relative = FALSE){
  re = list()
  
  re$data_dir = rave_options('data_dir')
  re$raw_data_dir = rave_options('raw_data_dir')
  
  if(relative){
    re$data_dir = '.'
  }
  
  if(!(missing(subject_code) || missing(project_name))){
    re$subject_name = paste0(project_name, '/', subject_code)
  }else if(!missing(subject_id)){
    re$subject_name = subject_id
    project_name = stringr::str_split(subject_id, '/')[[1]][[1]]
  }
  if(!is.null(re$subject_name)){
    
    re$subject_dir = (file.path(re$data_dir, re$subject_name))
    re$preprocess_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'preprocess'))
    # re$pre_visual_dir = (file.path(re$data_dir, re$subject_name, 'preprocess', 'visualizations'))
    re$rave_dir = (file.path(re$data_dir, re$subject_name, 'rave'))
    re$project_dir = (file.path(re$data_dir, project_name))
    re$meta_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'meta'))
    re$cache_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'data'))
    re$channel_dir = re$cache_dir
    re$reference_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'data', 'reference'))
    re$suma_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'suma'))
    re$suma_out_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'suma'))
    
    re$copied_raw_dir = copied_raw_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'raw'))
    
    re$module_data_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'module_data'))
    
    if(!missing(subject_code)){
      if(!dir.exists(copied_raw_dir)){
        re$pre_subject_dir = (file.path(re$raw_data_dir, subject_code))
      }else{
        re$pre_subject_dir = copied_raw_dir
      }
      
      if(!missing(block_num)){
        re$block_dir = (file.path(re$raw_data_dir, subject_code, block_num))
      }
    }
  }
  
  for(dname in mkdirs){
    if(dname %in% names(re)){
      dir = re[[dname]]
      if(!dir.exists(dir)){
        dir_create(dir)
      }
      if(dname %in% 'preprocess_dir'){
        dir_create(file.path(dir, 'spectrum'))
        dir_create(file.path(dir, 'voltage'))
      }
      if(dname %in% c('channel_dir', 'cache_dir')){
        dir_create(file.path(dir, 'power'))
        dir_create(file.path(dir, 'phase'))
        dir_create(file.path(dir, 'voltage'))
        dir_create(file.path(dir, 'reference'))
      }
    }
  }
  
  return(re)
}


