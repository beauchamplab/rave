# functions to create module,data,rawdata dirs

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
    rave::rave_options(
      data_dir = data_dir,
      raw_data_dir = raw_dir
    )
  }

  data_dir = rave::rave_options('data_dir')
  raw_dir = rave::rave_options('raw_data_dir')

  if(!dir.exists(data_dir) || !dir.exists(raw_dir)){
    logger('Cannot find data directory for RAVE. Please make sure that these folder exists', level = 'ERROR')
    logger(data_dir, level = 'ERROR')
    logger(raw_dir, level = 'ERROR')
    logger('Check existence of these folders, or reset default data repository by typing arrange_data_dir(reset = T)', level = 'ERROR')
    return(F)
  }else{
    rave_options(data_dir = tools::file_path_as_absolute(data_dir))
    rave_options(raw_data_dir = tools::file_path_as_absolute(raw_dir))
    return(T)
  }

}

#' @export
arrange_modules <- function(
  first_time = FALSE, reset = F
){

  look_up_file = rave::rave_options('module_lookup_file')
  target_dir = rave::rave_options('module_root_dir')

  dir.create(target_dir, recursive = T, showWarnings = F)
  new_modules = read.csv(system.file('modules.csv', package = 'rave'), stringsAsFactors = F)

  if(first_time || reset || !dir.exists(target_dir) || !file.exists(look_up_file)){
    # unlike data dir, we need to update this every time!
    try({
      columns = names(new_modules)
      if(file.exists(look_up_file)){
        old_modules = read.csv(look_up_file, stringsAsFactors = F)
      }
      new = merge(new_modules, old_modules, by = 'ModuleID', all = T, sort = F, suffixes = c('', '_old'))

      # check ModuleID
      new = new[!is.na(new$ModuleID),]

      lapply(seq_len(nrow(new)), function(ii){
        row = new[ii,]

        for(col in columns){
          if(is.na(row[[col]])){
            row[[col]] = row[[paste0(col, '_old')]]
          }
        }
        if(is.na(row$Active_old)){ row$Active = TRUE }else{
          row$Active = row$Active & row$Active_old
        }
        if(is_invalid(row$ScriptPath, .invalids = c('null', 'na', 'blank')) || !file.exists(row$ScriptPath)){
          row$Active = F
        }
        if(is.na(row$Version) || !is.character(row$Version)){
          row$Version = '0'
        }

        if(!reset){
          if(length(row$Version_old) == 1 &&
             !is.na(row$Version_old) &&
             is.character(row$Version_old) &&
             utils::compareVersion(row$Version, row$Version_old) < 0
          ){
            # New packages are not new! DO not change module file, this guy is a developer!
            row$Version = row$Version_old
            row$PackageID = row$PackageID_old
            row$GroupName = row$GroupName_old
            row$Name = row$Name_old
            row$ScriptPath = row$ScriptPath_old
            row$Author = row$Author_old
            row$Packages = row$Packages_old
          }
        }
        row[, columns]
      }) ->
        modules

      new = do.call(rbind, modules)

      lapply(modules, function(m){
        sel = m$ModuleID == new$ModuleID
        if(sum(sel) > 1){
          vers = as.numeric_version(new$Version[sel])
          m_ver = as.numeric_version(m$Version)
          if(m_ver != max(vers)){
            return(NULL)
          }
        }
        return(m)
      }) ->
        modules

      modules = rave:::dropNulls(modules)

      modules = do.call(rbind, modules)
      new_modules = modules[!duplicated(modules[,c('ModuleID', 'Version')]), ]

    }, silent = T)

    safe_write_csv(new_modules, look_up_file)

    # Move modules to module_root_dir
    # migrate new modules and overwrite
    file.copy(system.file('modules', package = 'rave'), target_dir, overwrite = T, recursive = T)
    file.copy(system.file('packages.txt', package = 'rave'), target_dir, overwrite = T)

  }

  rave_options(module_root_dir = tools::file_path_as_absolute(target_dir))
  rave_options(module_lookup_file = tools::file_path_as_absolute(look_up_file))

  logger('\nActive modules: \n', paste0(' - ', new_modules$Name[new_modules$Active], '(', new_modules$ModuleID[new_modules$Active], ')', collapse = '\n'),
          '\nAccording to [', look_up_file, ']', level = 'INFO')

  return(first_time || reset)
}


