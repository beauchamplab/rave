# utils

init_reavtive_globals <- function(){
  return(
    reactiveValues(
      auto_calculation = TRUE
    )
  )
}

#' @export
load_packages <- function(packages, repos = "https://cloud.r-project.org/"){
  if(length(packages) == 0 || packages == ''){
    return(invisible(NULL))
  }
  for(p in packages){
    if(!requireNamespace(p, quietly = T)){
      install.packages(p, repos = repos)
    }
    do.call('require', args = list(
      package = p,
      character.only = T,
      quietly = T
    ))
  }
}

#' @export
inner_environments <- function(results){
  for(result in results){
    if(is.environment(result)){
      return(result)
    }
    env <- environment(result)
    if(!is.null(env)){
      return(env)
    }
  }
  stop('Cannot find environment for SHINY_EXECUTE')
}


#' @export
parse_text = function(statement, env = parent.frame(), default = NULL){
  tryCatch({
    do.call('eval', args = list(
      parse(text = statement),
      envir = env
    )) ->
      res
    return(res)
  }, error = function(e){
    traceback(e)
    return(default)
  })

}



get_session_id = function(session = shiny::getDefaultReactiveDomain(), default = 'DEFAULT'){
  if(is.null(session)){
    return(default)
  }
  if(!exists('session_id', envir = session$userData)){
    return(default)
  }

  session_id = get('session_id', envir = session$userData)

  if(length(session_id) == 0){
    return(default)
  }

  return(session_id)

}






# Save things as HDF5 format
#' @export
save_hdf5 = function(X, name, group = '', file_name = strftime(Sys.time(), 'params_%Y%m%d_%H%M%S.h5'), create_new = TRUE, ...){
  temp_dir = rave_opts$get_options('temp_dir')
  if(!file.exists(temp_dir) || !file.info(temp_dir)$isdir){
    dir.create(temp_dir, recursive = TRUE)
  }

  tmp_path = file.path(temp_dir, file_name)

  if(create_new){
    rhdf5::h5createFile(tmp_path)
  }
  name = stringr::str_c(group, '/', name)
  name = stringr::str_replace_all(name, '[/]+', '/')

  group = stringr::str_replace_all(sprintf('/%s', group), '[/]+', '/')


  if(!group %in% rhdf5::h5ls(tmp_path)$group){
    rhdf5::h5createGroup(file = tmp_path, group = group)
  }

  if(length(list(...)) > 0){
    rhdf5::h5createDataset(tmp_path, name, dims = dim(X), ...)
  }
  rhdf5::h5write(X, tmp_path, name)

  return(tools::file_path_as_absolute(tmp_path))
}


#' @export
temp_file = function(filename = uuid::UUIDgenerate(), create_new = TRUE){
  filedir = rave_opts$get_options('temp_dir')
  if(!file.exists(filedir) || !file.info(filedir)$isdir){
    dir.create(filedir, recursive = TRUE)
  }

  f = file.path(filedir, filename)

  if(create_new){
    if(file.exists(f)){
      unlink(f)
    }
    file.create(f)
    f = tools::file_path_as_absolute(f)
  }else{
    f = file.path(
      tools::file_path_as_absolute(filedir),
      filename
    )
  }




  return(f)

}





