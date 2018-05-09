#' util functions to get subject dir
#' Internal use
NULL

#' @export
get_dir <- function(subject_code, project_name, block_num, mkdirs = NULL, subject_id){
  re = list()

  re$data_dir = rave_options('data_dir')
  re$raw_data_dir = rave_options('raw_data_dir')

  if(!(missing(subject_code) || missing(project_name))){
    re$subject_name = paste0(subject_code, '_', project_name)
  }else if(!missing(subject_id)){
    re$subject_name = subject_id
  }
  if(!is.null(re$subject_name)){

    re$subject_dir = file.path(re$data_dir, re$subject_name)
    re$preprocess_dir = file.path(re$data_dir, re$subject_name, 'preprocess')
    re$pre_visual_dir = file.path(re$data_dir, re$subject_name, 'preprocess', 'visualizations')
    re$rave_dir = file.path(re$data_dir, re$subject_name, 'rave')
    re$meta_dir = file.path(re$data_dir, re$subject_name, 'rave', 'meta')
    re$cache_dir = file.path(re$data_dir, re$subject_name, 'rave', 'cache')
    re$suma_dir = file.path(re$data_dir, re$subject_name, 'suma')
    re$suma_out_dir = file.path(re$data_dir, re$subject_name, 'suma', 'rave')

    if(!missing(subject_code)){
      re$pre_subject_dir = file.path(re$raw_data_dir, subject_code)

      if(!missing(block_num)){
        re$block_dir = file.path(re$raw_data_dir, subject_code, block_num)
      }
    }
  }

  for(dname in mkdirs){
    if(dname %in% names(re)){
      dir = re[[dname]]
      if(!dir.exists(dir)){
        dir.create(dir, recursive = T)
      }
    }
  }

  return(re)
}
