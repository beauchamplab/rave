# create local experiment environment

#' @export
attach_virtualenv = function(subject_id, electrodes, module_path = NULL, packages = '', is_univariate = TRUE, ...){

  tryCatch({
    detach(.runtime_env)
  }, error = function(e){})
  # init subject data
  .subject = prophet$get_subject(subject_id)
  .subject$data_environment$load(electrodes = electrodes)
  .subject$data_environment$bind_electrodes(electrodes = electrodes, debug = TRUE)

  data_repository$set_data(.subject$data_environment)
  # init module data
  if(!is.null(module_path)){
    .module = load_module(module_id = '.test_module', source_path = module_path, category = 'Test', label = 'Test Module',
      is_univariate = is_univariate, suma_enabled = FALSE, packages = packages)
    data_repository$add_module(module = .module)


    .runtime_env = .module$runtime_env
    .runtime_env$get_local_var <- .module$get_local_var
    .runtime_env$get_SUMA <- .module$get_SUMA
    .runtime_env$set_cache <- .module$set_cache
    .runtime_env$get_cache <- .module$get_cache
  }else{
    .runtime_env = new.env()
  }


  data_env <- data_repository$get_data()
  .runtime_env$data_env <- new.env()
  .runtime_env$data_env$cumsum <- data_env$cumsum
  .runtime_env$data_env$data <- data_env$data
  .runtime_env$data_env$electrodes <- data_env$electrodes
  .runtime_env$data_env$subject <- new.env()
  .runtime_env$data_env$subject$id <- .subject$id
  .runtime_env$data_env$subject$electrodes <- .subject$electrodes
  .runtime_env$data_env$subject$frequencies <- .subject$frequencies
  .runtime_env$data_env$subject$trials <- .subject$trials
  .runtime_env$data_env$subject$time_points <- .subject$time_points

  .runtime_env$get_global_var <- data_repository$get_global_var

  attach(.runtime_env)
  message('Virtual environment created:\nHere are variables you might want to use for development\n')
  print(ls(.runtime_env))
  message("Here's what you can find in data_env: \n")
  print(ls(.runtime_env$data_env))
  message('For example, data_env$data contains ecog tensor data\n')
  message('  while data_env$subject contains subject info\n')
  message('--- Type detach_virtualenv() to quit this environment. Enjoy :)')
}

#' @export
detach_virtualenv = function(){
  detach(.runtime_env)
}


