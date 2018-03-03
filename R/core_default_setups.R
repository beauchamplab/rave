# functions to create module,data,rawdata dirs

#' @export
arrange_data_dir <- function(is_dev = FALSE){
  data_dir = rave::rave_opts$get_options('data_dir')
  raw_dir = rave::rave_opts$get_options('raw_data_dir')
  is_changed = FALSE

  if(!dir.exists(data_dir) || is_dev){
    data_dir = '~/rave_data/data_dir'
    dir.create(data_dir, showWarnings = F, recursive = T)

    # move example data to it
    message("--------------------")
    message("Original data directory NOT found! Trying to generate default data repository")
    message("Original data repository - [", rave::rave_opts$get_options('data_dir'), '] (invalid/not exist)')
    message("New data repository - [", data_dir, ']')

    # Russian rocket: RE-DO whenever fails
    file.copy(system.file('data/data_dir', package = 'rave'), '~/rave_data/', recursive = T, overwrite = T)

    is_changed = T
  }
  rave::rave_opts$set_options(data_dir = tools::file_path_as_absolute(data_dir))


  if(!dir.exists(raw_dir) || is_dev){
    raw_dir = '~/rave_data/raw_dir'
    dir.create(raw_dir, showWarnings = F, recursive = T)

    # move example data to it
    message("--------------------")
    message("Original raw-data directory NOT found! Trying to generate default raw-data repository")
    message("Original raw-data repository - [", rave::rave_opts$get_options('raw_data_dir'), '] (invalid/not exist)')
    message("New raw-data repository - [", raw_dir, ']')

    # Russian rocket: RE-DO whenever fails
    file.copy(system.file('data/raw_dir', package = 'rave'), '~/rave_data/', recursive = T, overwrite = T)

    is_changed = T
  }
  rave::rave_opts$set_options(raw_data_dir = tools::file_path_as_absolute(raw_dir))

  rave::rave_opts$save_settings()
  return(is_changed)
}

#' @export
arrange_modules <- function(
  look_up_file = rave::rave_opts$get_options('module_lookup_file'),
  target_dir = NULL,
  is_new = FALSE
){
  is_changed = FALSE
  # check conditions
  if(is.null(look_up_file) || !file.exists(look_up_file)){
    is_new = TRUE
    is_changed = TRUE
  }

  if(is_new){
    look_up_file = '~/rave_modules/modules.csv'
  }

  tryCatch({
    read.csv(look_up_file, stringsAsFactors = F)
  }, error = function(e){
    NULL
  }) ->
    old_modules
  if(is.null(target_dir)){
    target_dir = dirname(look_up_file)
  }

  # makesure target_dir exists
  dir.create(target_dir, recursive = T, showWarnings = F)

  # define destination files
  # write to look_up_file
  new_modules = read.csv(system.file('modules.csv', package = 'rave'), stringsAsFactors = F)
  new_modules.copy = new_modules
  if(is.data.frame(old_modules) && nrow(old_modules)){
    tryCatch({
      sel = !old_modules$ModuleID %in% new_modules$ModuleID
      if(sum(sel)){
        if(!setequal(names(new_modules), names(old_modules))){
          n_names = setdiff(names(new_modules), names(old_modules))
          old_modules[, n_names] = NA
        }
        new_modules = rbind(new_modules, old_modules[sel, names(new_modules)])
      }
      new_modules = new_modules[complete.cases(new_modules[, c("ModuleID","Name","ScriptPath")]), ]
      new_modules$Active[is.na(new_modules$Active)] = FALSE
      new_modules[is.na(new_modules)] = ""
      new_modules$Active = new_modules$Active & sapply(new_modules$ScriptPath, function(p){tryCatch({
        file.exists(p)
      }, error = function(e){FALSE})})
      new_modules
    }, error = function(e){
      warning('Failed to migrate old modules to [', look_up_file, ']')
      return(new_modules.copy)
    }) ->
      new_modules
  }
  write.csv(new_modules, look_up_file)
  look_up_file = tools::file_path_as_absolute(look_up_file)
  rave::rave_opts$set_options(module_lookup_file = look_up_file, module_root_dir = target_dir)

  message("--------------------")
  message('Active modules: \n', paste0(' - ', new_modules$Name[new_modules$Active], collapse = '\n'),
          '\nPlease edit [', look_up_file, ']')

  # migrate new modules and overwrite
  new_repo = system.file('modules', package = 'rave')
  file.copy(new_repo, target_dir, overwrite = T, recursive = T)

  rave_opts$save_settings()

  return(is_changed)
}


