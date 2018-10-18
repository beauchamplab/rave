# functions for group analysis

#' @export
module_analysis_names <- function(module_id, project_name){
  # if missing project_name, get from current repository
  if(missing(project_name)){
    data_env = getDefaultDataRepository()
    on.exit(rm(data_env))
    project_name = data_env$subject$project_name
  }

  # abuse function get_dir
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir

  if(!dir.exists(lookup_dir)){
    dir.create(lookup_dir, recursive = T, showWarnings = F)
  }

  if(!missing(module_id)){
    module_id = stringr::str_to_upper(module_id)
    pattern = sprintf('^%s\\-([a-zA-Z0-9_]+).[cC][sS][vV]$', module_id)
  }else{
    pattern = '\\-([a-zA-Z0-9_]+).[cC][sS][vV]$'
  }
  filenames = list.files(path = lookup_dir, pattern = pattern)
  analysis_names = NULL
  if(length(filenames)){
    analysis_names = stringr::str_match(filenames, pattern)[,2]
  }
  analysis_names
}


#' @export
subject_tmpfile <- function(module_id, fun_name = '', project_name, subject_code, pattern = 'file_'){
  if(missing(project_name)){
    data_env = getDefaultDataRepository()
    on.exit(rm(data_env))
    project_name = data_env$subject$project_name
    subject_code = data_env$subject$subject_code
  }

  assertthat::assert_that(!is.blank(project_name) && !is.blank(subject_code), msg = 'subject_code or project_name is blank while creating subject tmpfile.')

  tmpdir = get_dir(subject_code = subject_code, project_name = project_name)$module_data_dir
  tmpdir = file.path(tmpdir, module_id, fun_name)
  if(!dir.exists(tmpdir)){
    dir.create(tmpdir, recursive = T, showWarnings = F)
  }
  tempfile(tmpdir = tmpdir, pattern = pattern)
}


#' @export
module_analysis_table <- function(project_name, module_id, analysis_name, check_valid = FALSE){
  # if missing project_name, get from current repository
  if(missing(project_name)){
    data_env = getDefaultDataRepository()
    on.exit(rm(data_env))
    project_name = data_env$subject$project_name
  }
  module_id = stringr::str_to_upper(module_id)
  analysis_name = stringr::str_to_upper(analysis_name)
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir
  lookup_file = file.path(lookup_dir, sprintf('%s-%s.csv', module_id, analysis_name))
  if(file.exists(lookup_file)){
    tbl = read.csv(lookup_file, stringsAsFactors = F)
    if(check_valid){
      # Check if all path is valid
      # Only check valid ones
      lapply(tbl$file, function(path){
        find_path(path)
      }) ->
        paths
      is_valid = vapply(paths, function(p){!is.null(p)}, FUN.VALUE = TRUE)
      use_safe = F
      if(any(!is_valid)){
        use_safe = TRUE
      }
      tbl$valid = is_valid
      tbl$file[is_valid] = unlist(paths[is_valid])
      tbl = tbl[tbl$valid, ]
      # write back to file
      if(use_safe){
        safe_write_csv(tbl, lookup_file, row.names = F)
      }else{
        write.csv(tbl, lookup_file, row.names = F)
      }
    }else{
      tbl = tbl[tbl$valid, ]
    }

  }else{
    tbl = data.frame()
  }
  return(tbl)
}

#' @export
find_path <- function(path, root_dir){
  if(file.exists(path)){
    return(path)
  }
  root_dir %?<-% rave_options('data_dir')
  path = unlist(stringr::str_split(path, '(/)|(\\\\)|(\\~)'))
  path = path[path != '']

  for(ii in 1:length(path)){
    tmp_path = do.call(file.path, as.list(c(root_dir, path[ii:length(path)])))
    if(file.exists(tmp_path)){
      return(tmp_path)
    }
  }

  # No path found
  return(NULL)
}

#' @export
module_analysis_save <- function(project_name, subject_code, module_id, analysis_name, file, meta = NULL){
  # if missing project_name, get from current repository
  if(missing(project_name) || missing(subject_code)){
    data_env = getDefaultDataRepository()
    on.exit(rm(data_env))
    project_name = data_env$subject$project_name
    subject_code = data_env$subject$subject_code
  }

  # abuse function get_dir
  lookup_dir = get_dir('_export_lookup', project_name = project_name)$subject_dir

  if(!dir.exists(lookup_dir)){
    dir.create(lookup_dir, recursive = T, showWarnings = F)
  }

  module_id = stringr::str_to_upper(module_id)
  analysis_name = stringr::str_extract_all(analysis_name, '[a-zA-Z0-9_]', simplify = T)
  analysis_name = paste(analysis_name, collapse = '')
  analysis_name = stringr::str_to_upper(analysis_name)

  lfile = file.path(lookup_dir, sprintf('%s-%s.csv', module_id, analysis_name))

  if(file.exists(lfile)){
    old_data = read.csv(lfile)
  }else{
    old_data = data.frame()
  }

  valid = FALSE
  if(file.exists(file)){
    logger('File detected, export meta info')
    file = normalizePath(file)
    valid = TRUE
  }else{
    logger('File not detected, meta info will be ignored')
    return()
  }

  v = data.frame(
    project_name = project_name,
    subject_code = subject_code,
    module_id = module_id,
    analysis_name = analysis_name,
    file = file,
    timestamp = as.numeric(Sys.time()),
    valid = valid
  )

  if(is.data.frame(meta) && nrow(meta) == 1){
    nv = cbind(v, meta)
  }else{
    nv = v
  }
  tryCatch({
    new_data = rbind(nv, old_data)
    write.csv(new_data, file = lfile, row.names = F)
  }, error = function(e){
    if(setequal(names(v), names(old_data))){
      # get rid of meta data
      new_data = rbind(v, old_data)
      write.csv(new_data, file = lfile, row.names = F)
    }else{
      safe_write_csv(v, lfile, row.names = F)
    }
  })
}
