
dev_f <- function(
  module_id, reactive = TRUE,
  root_path = '.', force_reload_subject = FALSE,
  project_name, subject_code, electrodes = NULL, epoch, time_range = c(1,2),
  reference = 'default', frequency_range = NULL, data_types = NULL,
  load_brain = TRUE, download_url = NULL){

  dev_call_expr = match.call()

  # Setups for debugs
  rave::`%?<-%`(root_path, '.')
  rave::`%?<-%`(reactive, TRUE)
  rave::`%?<-%`(force_reload_subject, FALSE)

  # Current package name
  package = '${{PACKAGE}}'

  # Get package root. This is very important as we need to work
  # as if we are under the package root
  root_path = normalizePath(root_path, mustWork = TRUE)

  get_path = function(path, mustWork = F){
    if(file.exists(path)){
      return(normalizePath(path))
    }
    path = strsplit(path, '/|\\\\')[[1]]
    for(ii in 1:(length(path)-1)){
      tmp = paste(path[-(1:ii)], collapse = '/')
      tmp = system.file(tmp, package = package, mustWork = F)
      if(tmp != ''){
        return(tmp)
      }
    }
    if(mustWork){
      stop('Cannot find path: ', path)
    }
  }




  # ------------------ Step 1: load package utils and prepare subjects ------------------
  path = file.path(root_path, 'inst', 'utils', 'package_utils.R')
  path = get_path(path, mustWork = T)
  source(path, local = T)
  utils_pkg = package_utils(package = package)

  # sub 1: load config file
  path = file.path(root_path, 'inst', 'rave.yaml')
  path = get_path(path, mustWork = T)
  config = utils_pkg$load_rave_yaml(yaml_path = path)

  # sub 2: load dependencies
  if(is.list(config$dependencies)){
    utils_pkg$check_load_packages(cran = config$dependencies$cran, github = config$dependencies$github)
  }


  # sub 3: load subjects
  # check data repos
  force_reload_subject = force_reload_subject || !rave:::any_subject_loaded()
  if(force_reload_subject){
    if(missing(project_name)){
      tryCatch({
        dev_subject = config$dev_subject
        dev_subject$electrodes = rave::parse_selections(dev_subject$electrodes)
        dev_subject$time_range = c(dev_subject$time_range$pre, dev_subject$time_range$post)
        do.call(utils_pkg$load_demo_subject, dev_subject)
      }, error = function(e){
        print(traceback(e))
        rutabaga::cat2('Error occurred. Check package configuration "dev_subject"', level = 'ERROR')
        rutabaga::cat2('Try to use demo subject', level = 'INFO')
        utils_pkg$load_demo_subject()
      })
    }else{
      rutabaga::cat2('Override default subject...', level = 'INFO')
      tryCatch({
        utils_pkg$load_demo_subject(
          project_name = project_name, subject_code = subject_code,
          electrodes = electrodes, epoch = epoch, time_range = time_range,
          reference = reference, frequency_range = frequency_range,
          data_types = data_types, load_brain = load_brain,
          download_url = download_url)
      }, error = function(e){
        print(traceback(e))
        rutabaga::cat2('Error occurred. Please check help("rave_prepare") for argument details', level = 'FATAL')
      })

    }
  }

  rutabaga::cat2('------------------ Subject loaded ------------------', level = 'DEBUG')

  # sub 4: load modules
  if(length(config$modules)){
    modules = lapply(config$modules, function(m){
      path = file.path(root_path, 'inst', m$config_name)
      path = get_path(path, mustWork = T)
      cfg = utils_pkg$load_rave_yaml(yaml_path = path)
      c(m, cfg)
    })

    module_ids = sapply(config$modules, function(m){ m$module_id })
    names(modules) = module_ids
  }else{
    modules = NULL
    module_id = ''
  }



  # ------------------ Step 2: Register input output tools ------------------
  path = file.path(root_path, 'inst', 'utils', 'module_utils.R')
  path = get_path(path, mustWork = T)
  source(path, local = T)

  dev_call_expr
  dev_call_expr[[1]] = rlang::quo_squash(rlang::quo(`:::`(!!!package, dev_f)))
  command = capture.output(print(dev_call_expr))

  dev_module = function(module_id){

    if(!rave::any_subject_loaded()){
      rave::attachDefaultDataRepository()
    }

    config = modules[[module_id]]

    utils_pkg = module_utils(
      package = package,
      root_path = root_path,
      config = config,
      restart_command = command
    )

    toolbox_env = utils_pkg$get_toolbox(reactive = reactive)

    # toolbox_env$reactive
    toolbox_env$attach_toolbox()
  }

  if(!missing(module_id)){
    dev_module(module_id)
  }else{
    dev_module(module_id[1])
  }


  return(invisible(dev_module))
}


#' Function to create dev environment for RAVE package developers
#' @export
dev_env_${{PACKAGE}} <- dev_f


