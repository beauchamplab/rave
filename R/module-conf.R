#' Get RAVE module package name
#' @noRd
get_package_name <- function(env = parent.frame()){
  get('.__rave_package__.', envir = env, inherits = TRUE)
}

#' Get RAVE module root directory
#' 1. If in RStudio, get current working directory/project dir
#' 2. If not in RStudio, or the project is not rave module directory, return
#' the one installed in the system library
#' 
#' requires package name
#' 
#' @noRd
get_root_dir <- function(){
  ctx = rave_context(disallowed_context = 'default')
  if(verify_rstudio_version()){
    d = rstudioapi::getActiveProject()
  }else{
    d = NULL
  }
  pkgname = ctx$package
  
  if(length(d) == 1 && grepl(paste0('/', pkgname, '$'), d)){
    return(d)
  }else{
    # package user
    return(system.file('', package = pkgname))
  }
}


#' Safe Way to Access Module Package Files Using Relative Path
#' @param ... relative path to the file
#' @param mustWork whether the file must exists
#' @param is_directory whether required file is a directory
#' @return If you are developing the package, \code{get_path} returns the 
#' absolute file path, otherwise it uses \code{\link[base]{system.file}} to 
#' get the file from package library.
#' @export
get_path <- function(..., mustWork = FALSE, is_directory = FALSE){
  
  ctx = rave_context(disallowed_context = 'default')
  
  package = ctx$package
  project_dir = get_root_dir()
  
  path = file.path(project_dir, ...)
  
  path_bak = path
  if(file.exists(path)){
    return(normalizePath(path))
  }
  path = strsplit(path, '/|\\\\')[[1]]
  for(ii in 1:(length(path)-1)){
    tmp = paste(path[-(1:ii)], collapse = '/')
    tmp = system.file(tmp, package = package, mustWork = FALSE)
    if(tmp != ''){
      return(normalizePath(tmp))
    }
  }
  
  if(mustWork){
    catgl('Cannot find path: ', path_bak, ' (try to locate it manually)',
         level = 'ERROR')
    
    path = select_path(is_directory)
    
    if(!is.null(path)){
      path = normalizePath(path)
      catgl('Found it! - ', path, level = 'INFO')
      return(path)
    }
  }
}


#' Get package settings file
#' Running context: `debug`
#' Inner context: `debug`
#' linked to: `get_path`
#' 
#' @return list of settings
#' 
#' @noRd
load_rave_yaml <- function(){
  rave_context(disallowed_context = 'default')
  path = get_path('inst', 'rave.yaml', mustWork = TRUE)
  conf = as.list(raveio::load_yaml(path))
  conf
}


#' Parse DESCRIPTION file and make sure all packages are installed
#' Running context: `debug`
#' Inner context: `debug`
#' linked to: `get_root_dir`
#' 
#' @return list of settings
#' 
#' @noRd
load_pkg_description <- function(check_dependencies = c('cran', 'Remotes'), 
                                  force_update_remote = FALSE){
  rave_context(disallowed_context = 'default')
  path = get_path('DESCRIPTION', mustWork = TRUE)
  desc = as.list(raveio::load_yaml(path))
  
  if('cran' %in% check_dependencies){
    devtools::install_dev_deps(get_root_dir(), dependencies = TRUE)
  }
  
  if(!is.null(desc$Imports)){
    pkgs = devtools::parse_deps(desc$Imports)
    desc$Imports = pkgs$name
  }
  
  if(!is.null(desc$Suggests)){
    pkgs = devtools::parse_deps(desc$Suggests)
    desc$Suggests = pkgs$name
  }
  
  if(!is.null(desc$Remotes)){
    desc$Remotes = stringr::str_split(desc$Remotes, ',')[[1]]
    desc$Remotes = stringr::str_trim(desc$Remotes)
    pkgs = stringr::str_match(
      desc$Remotes,
      pattern = '(?:(^[a-zA-Z0-9_]*)::|)([\\w]*)/([a-zA-Z0-9_]*)(?:(@[\\w]+)|)$'
    )
    pkgs[is.na(pkgs[,2]),2] = 'github'
    pkgs[is.na(pkgs[,5]),5] = ''
    desc$Remotes = pkgs[,4]
    if(!force_update_remote){
      # Check if packages are installed
      not_installed = dipsaus::check_installed_packages(pkgs[,4], auto_install = F)
      pkgs = pkgs[pkgs[,4] %in% not_installed,, drop=FALSE]
    }
    if('Remotes' %in% check_dependencies && nrow(pkgs)){
      # install
      cmds = paste0('devtools::install_', pkgs[,2], '("',  pkgs[,3], '/',  pkgs[,4], pkgs[,5], '")')
      for(cmd in cmds){
        eval(parse(text = cmd))
      }
    }
  }
  
  
  desc
}

#' Get Module Label given ID
#' Running context: `debug`
#' Inner context: `debug`
#' linked to: `load_rave_yaml`
#' 
#' @return label of module
#' 
#' @noRd
get_module_label <- function(module_id){
  rave_context(disallowed_context = 'default')
  conf = load_rave_yaml()
  module_label = lapply(conf$modules, function(comp){
    if(comp$module_id == module_id){
      return(comp$module_label)
    }
    return(NULL)
  })
  module_label = unlist(module_label)
  module_label %?<-% 'Unknown Module'
  module_label = module_label[1]
  module_label
}


#' @title Load Demo Subject According to Package Configuration File
#' 
#' @param subject_code optional, subject code
#' @param project_name optional, project name
#' @param ... further passed to \code{\link[rave]{rave_prepare}}
#' @param download_url optional, web link to subject archive 
#' @param force_reload_subject logical, whether to force reload subject even 
#' if another subject is loaded
#' 
#' @details When debugging the 'RAVE' modules, it loads demo subject for
#' debugging from according to settings file \code{"inst/rave.yaml"}. 
#' 
#' This function only function properly in \code{'rave_module_debug'} mode.
#' This means by default it raises errors. In other mode, for example 
#' \code{'rave_running'}, it does nothing.
#' @return None
#' 
#' @name mount_demo_subject
NULL

.mount_demo_subject <- function(subject_code, project_name,
                                force_reload_subject = FALSE,...,download_url){
  rave_context(disallowed_context = 'default', 
               error_msg = 'only used to debug the modules.')
  if(!force_reload_subject && any_subject_loaded()){
    if(!'rave_data' %in% search()){
      attachDefaultDataRepository()
    }
    return(invisible())
  }
  force_reload_subject = force_reload_subject || !any_subject_loaded()
  env = new.env()
  conf = load_rave_yaml()
  
  conf$dev_subject$electrodes = dipsaus::parse_svec(conf$dev_subject$electrodes)
  conf$dev_subject$time_range = c(conf$dev_subject$time_range$pre, conf$dev_subject$time_range$post)
  
  list2env(conf$dev_subject, envir = env)
  
  if(missing(project_name) || missing(subject_code)){
    subject_code = conf$dev_subject$subject_code
    project_name = conf$dev_subject$project_name
    download_url = conf$dev_subject$download_url
  }
  
  
  
  # If subject_code and project_name are not missing
  if(!subject_code %in% get_subjects(project_name)){
    download_url %?<-% 'Not given :/'
    ans = ask_question(
      title = 'This action requires downloading subject.',
      message = paste0(
        'Project Name: ', project_name, '\n',
        'Subject Code: ', subject_code, '\n',
        'Remote URL: \n\t', download_url
      )
    )
    if(ans){
      # download subject
      download_subject_data(download_url, override_project = project_name, override_subject = subject_code)
    }else{
      stop('Action aborted because no [', project_name, '/', subject_code, '] found.')
    }
  }
  
  # subject exists, load it
  env$subject_code = subject_code
  env$project_name = project_name
  list2env(list(...), envir = env)
  
  # Create subject instance
  subject = Subject$new(project_name = env$project_name,
                        subject_code = env$subject_code,
                        reference = env$reference)
  
  catgl('Loading subject. Please wait...', level = 'INFO')
  
  rave_prepare(
    subject = subject, electrodes = env$electrodes, epoch = env$epoch,
    time_range = env$time_range, reference = env$reference, frequency_range = env$frequency_range,
    data_types = env$data_types, load_brain = env$load_brain, attach = T)
  
  return(invisible())
}
#' @rdname mount_demo_subject
#' @export
mount_demo_subject <- rave_context_generics('mount_demo_subject', .mount_demo_subject)

#' @rdname mount_demo_subject
#' @export
mount_demo_subject.rave_module_debug <- .mount_demo_subject

#' @rdname mount_demo_subject
#' @export
mount_demo_subject.rave_running <- do_nothing

#' @rdname mount_demo_subject
#' @export
mount_demo_subject.rave_running_local <- do_nothing


.rave_dev_env <- function(){
  rave_context(context = 'rave_module_debug', disallowed_context = 'default')
  desc = load_pkg_description(NULL)
  pkgs = c(desc$Imports, desc$Suggests, desc$Remotes)
  installed = dipsaus::package_installed(pkgs, all = TRUE)
  if(!installed){
    load_pkg_description()
  }
  for(p in pkgs){
    do.call('library', list(p, character.only = TRUE))
  }
}
rave_dev_env <- rave_context_generics('rave_dev_env', .rave_dev_env)
rave_dev_env.rave_module_debug <- .rave_dev_env
rave_dev_env.rave_running <- do_nothing
rave_dev_env.rave_running_local <- do_nothing


#' Function to load RAVE module package with UI tools
#' @description called internally by \code{\link[rave]{init_module}} or 
#' other module packages
#' @param env environment to load tools
#' @param parse_context parsing context
#' @export
load_rave_module_package <- function(
  env, parse_context = c("rave_module_debug", "rave_running", "rave_running_local")){
  
  parse_context = match.arg(parse_context)
  
  rave_context(disallowed_context = 'default')
  rave_context(context = parse_context, tenv = env)
  
  toolbox_files = file.path(
    system.file('template/inst/tools/', package = 'rave'), 
    c('input_widgets.R', 'output_widgets.R')
  )
  
  .fs_dir = get_path('inst/tools')
  if( .fs_dir != '' && dir.exists(.fs_dir) ){
    toolbox_files = c(toolbox_files, list.files(.fs_dir, pattern = '\\.R$', full.names = TRUE))
  }
  
  .fs = switch (
    parse_context,
    'rave_module_debug' = system.file('module_addons/local.R', package = 'rave'),
    'rave_running_local' = system.file('module_addons/local.R', package = 'rave'),
    'rave_running' = system.file('module_addons/compile.R', package = 'rave'),
    {
      stop('parse_context ', sQuote(parse_context), ' is invalid')
    }
  )
  toolbox_files = c(.fs, toolbox_files)
  
  rave_context(context = parse_context, tenv = env)
  
  # Load toolboxes
  lapply(toolbox_files, function(f){
    source(f, local = env)
  })
  
  rave_context(context = parse_context, tenv = env)
  invisible(env)
}



#' @title Reload 'RAVE' module package without restarting 'RStudio'
#' @description For debugging module packages. In all other contexts it will
#' raise error.
#' @param expose whether to expose development tools to the global environment;
#' default is no
#' @param clear_env whether to clear the global environment before reloading;
#' default is no
#' @name reload_module_package
NULL
.reload_module_package <- function(expose = FALSE, clear_env = FALSE){
  rave_context(disallowed_context = c('default'))
  local = !expose
  if(clear_env){
    rm(list = ls(all.names = TRUE, envir = globalenv()), envir = globalenv())
  }
  pkg_dir = get_root_dir()
  save_all()
  devtools::document(pkg_dir)
  devtools::load_all(pkg_dir, reset = TRUE, export_all = TRUE)
  
  if(!local){
    env = globalenv()
  }else{
    env = new.env(parent = globalenv())
  }
  
  load_rave_module_package(env = env, parse_context = 'rave_module_debug')
}
#' @rdname reload_module_package
#' @export
reload_module_package <- rave_context_generics('reload_module_package', .reload_module_package)
#' @export
reload_module_package.rave_module_debug <- .reload_module_package
#' @export
reload_module_package.rave_running <- do_nothing
#' @export
reload_module_package.rave_running_local <- do_nothing




