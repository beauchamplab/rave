

#' Function to check if data repository has data
#' @export
any_subject_loaded <- function(){
  # Right now this function only has simple checks

  has_data = FALSE

  rave_data = getDefaultDataRepository()
  if(all(c("data_check", "module_tools",
           "preload_info", "subject") %in% names(rave_data))){
    has_data = TRUE
  }

  rm(rave_data)
  return(has_data)
}


#' Function to find modules in packages
#' @param package package name to search for modules
#' @param module_id (optional) module ID if the package contains multiple modules
#' @export
get_module <- function(package, module_id){
  yml = system.file('rave.yaml', package = package)
  if(yml == ''){
    logger('Package ', package, ' contains no RAVE module.', level = 'ERROR')
    return(invisible())
  }else{
    conf = yaml::read_yaml(yml)
    ids = sapply(conf$modules, '[[', 'module_id')

    if(missing(module_id)){
      module_id = ids
    }else if(any(!module_id %in% ids)){
      logger('Cannot find module ', paste(module_id[!module_id %in% ids], collapse = ', '), ' in package ', package, ' - Terminate.', level = 'ERROR')
      return(invisible())
    }
  }

  # Load dev environment
  .fs = list.files(system.file('tools', package = package), pattern = '\\.R$', full.names = T)
  env = new.env()
  with(env, {
    for(.f in .fs){
      source(.f, local = T)
    }
  })

  if(length(module_id) == 1){
    module = env$to_module(module_id = module_id, sidebar_width = 3L)
    return(module)
  }else{
    modules = lapply(module_id, function(mid){
      tryCatch({
        env$to_module(module_id = mid, sidebar_width = 3L)
      }, error = function(e){
        NULL
      })

    })

    modules = dropNulls(modules)
    if(length(modules) == 0){
      modules = NULL
    }else if(length(modules) == 1){
      modules = modules[[1]]
    }
  }
}


#' Check all packages to for new RAVE module packages
#' @param packages array of packages to search for, default is all packages
#' @param as_module logical, try to return module instances or just a list of modules
#' @export
detect_modules <- function(packages, as_module = TRUE){

  lib_path = .libPaths()
  all_packages = unlist(sapply(lib_path, function(lp){
    list.dirs(lp, recursive = FALSE, full.names = FALSE)
  }, simplify = F))
  all_packages = unique(all_packages)

  yaml_path = sapply(all_packages, function(p){
    system.file('rave.yaml', package = p)
  })

  packages %?<-% all_packages

  sel = (yaml_path != '' & all_packages %in% packages)

  if(!sum(sel)){
    return(NULL)
  }

  all_packages = all_packages[sel]
  yaml_path = yaml_path[sel]

  m_info = cbind(all_packages, yaml_path)

  # load yaml
  m_data = lapply(seq_len(nrow(m_info)), function(ii){
    x = m_info[ii, ]
    pname = x[1]
    ypath = x[2]

    tryCatch({
      conf = yaml::read_yaml(ypath)
      do.call('rbind', lapply(conf$modules, function(m){
        module_id = m$module_id
        label_name = m$module_label
        label_name %?<-% sprintf('No Label (%s)', module_id)
        group_name = m$group_name
        group_name %?<-% '______'
        c(module_id, label_name, group_name, pname)
      }))

    }, error = function(e){
      NULL
    })
  })

  m_data = dropNulls(m_data)

  if(!length(m_data)){
    return(NULL)
  }

  m_data = do.call('rbind', m_data)

  if(as_module){
    gnames = unique(m_data[,3])
    modules = sapply(gnames, function(gname){
      sel = m_data[,3] == gname

      m = lapply(which(sel), function(ii){
        x = m_data[ii,]
        tryCatch({
          find_module(package = x[4], module_id = x[1])
        }, error = function(e){
          NULL
        })
      })

      m = dropNulls(m)
      if(!length(m)) m = NULL
      m
    }, simplify = F, USE.NAMES = T)

    modules = dropNulls(modules)
    if(!length(modules)){
      return(NULL)
    }

    return(modules)
  }else{
    return(m_data)
  }

}
