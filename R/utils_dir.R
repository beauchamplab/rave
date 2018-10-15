#' util functions to get subject dir
#' Internal use
NULL


# New dir hierachy
# data_dir > projectdir > subjectdir > rave > {raw, preprocessing, rave, meta, suma}

#' @export
get_projects <- function(){
  data_dir = rave_options('data_dir')
  pj = list.dirs(data_dir, full.names = F, recursive = F)
  pj[str_detect(pj, '^[a-zA-Z0-9]') & pj != 'rave']
}

#' @export
get_subjects <- function(project_name){
  data_dir = rave_options('data_dir')
  sub = list.dirs(file.path(data_dir, project_name), full.names = F, recursive = F)
  sub[str_detect(sub, '^[a-zA-Z0-9]') & sub!= 'rave']
}


#' @export
get_dir <- function(subject_code, project_name, block_num, mkdirs = NULL, subject_id, relative = F){
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
  }
  if(!is.null(re$subject_name)){

    re$subject_dir = (file.path(re$data_dir, re$subject_name))
    re$preprocess_dir = (file.path(re$data_dir, re$subject_name, 'rave', 'preprocess'))
    # re$pre_visual_dir = (file.path(re$data_dir, re$subject_name, 'preprocess', 'visualizations'))
    re$rave_dir = (file.path(re$data_dir, re$subject_name, 'rave'))
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
        dir.create(dir, recursive = T)
      }
      if(dname %in% 'preprocess_dir'){
        dir.create(file.path(dir, 'spectrum'), recursive = T, showWarnings = F)
        dir.create(file.path(dir, 'voltage'), recursive = T, showWarnings = F)
      }
      if(dname %in% c('channel_dir', 'cache_dir')){
        dir.create(file.path(dir, 'power'), recursive = T, showWarnings = F)
        dir.create(file.path(dir, 'phase'), recursive = T, showWarnings = F)
        dir.create(file.path(dir, 'voltage'), recursive = T, showWarnings = F)
        dir.create(file.path(dir, 'reference'), recursive = T, showWarnings = F)
      }
    }
  }

  return(re)
}
