

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
get_module <- function(package, module_id, local = FALSE){

  if(local){
    if(missing(module_id)){
      logger('You are running module locally. Please specify module ID.')
      return(invisible())
    }else{
      return(debug_module(package = package, module_id = module_id))
    }
  }

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
  .fs = list.files(system.file('template/inst/tools', package = 'rave'), pattern = '\\.R$', full.names = T)
  env = new.env()
  with(env, {
    for(.f in .fs){
      source(.f, local = T)
    }
  })
  env$.packageName = package

  if(length(module_id) == 1){
    module = env$to_module(module_id = module_id, sidebar_width = 3L)

    return(module)
  }else{
    modules = lapply(module_id, function(mid){
      tryCatch({
        env$to_module(module_id = mid, sidebar_width = 3L)
      }, error = function(e){
        logger('An error occurred during parsing module ', mid, ' (', package, '). Please check source code if you are module developer. [Ignored]', level = 'WARNING')
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


debug_module <- function(package = package, module_id = module_id, reload = FALSE){
  .fs = list.files(system.file('template/inst/tools', package = 'rave'), pattern = '\\.R$', full.names = T)
  rave_dev_load <- function(){
    # Get package name
    env = new.env()
    with(env, {
      for(.f in .fs){
        source(.f, local = T)
      }
    })
    env$.packageName = package
    return(env)
  }
  # Reload first
  if(reload){
    env = rave_dev_load()
    env$reload_this_package(expose = FALSE, clear_env = FALSE)
  }


  env = rave_dev_load()

  # Need to load subject first
  has_subject = any_subject_loaded()

  if(!has_subject){
    logger('Error: No subject found! Please load subject first', level = 'ERROR')
    return(invisible())
  }

  if(has_subject && !'rave_data' %in% search()){
    attachDefaultDataRepository()
  }

  assign('aaa', env, envir = globalenv())
  param_env = env$init_module(module_id = module_id)

  runtime_env = new.env(parent = param_env)

  envs = env$get_comp_env(module_id = module_id)
  has_content = env$get_content(content = envs$content, env = envs$tmp_env)
  inputs = lapply(envs$input_env, function(comp){
    if(is(comp, 'comp_input')){
      return(comp$inputId)
    }else{
      NULL
    }
  })
  inputs = unlist(inputs); names(inputs) = NULL

  args = as.list(param_env)[inputs]

  main_quos = env$get_main_function(module_id)

  outputIds = lapply(envs$output_env, function(comp){
    if(is(comp, 'comp_output')){
      return(comp$outputId)
    }else{
      NULL
    }
  })
  outputIds = unlist(outputIds)


  FUN = function(){}

  environment(FUN) = runtime_env

  sel = names(main_quos) %in% c('async')
  normal_quos = main_quos[!sel]
  async_quo = main_quos[sel]
  async = length(async_quo)

  body(FUN) = rlang::quo_squash(rlang::quo({
    !!!normal_quos

    results = environment()
    ..env = list()

    ..env$results = new.env()

    ..tmp = new.env()

    ..tmp[['..async']] = FALSE

    if(!!async){
      ..tmp[['..async']] = TRUE
      ..tmp[['..async_quo']] = rlang::quo_squash(async_quo[[1]])
      ..tmp[['..async_var']] = NULL
      ..tmp[['..packages']] = str_match(search(), '^package:(.+)$')[,2]
      ..tmp[['..packages']] = unique(..tmp[['..packages']][!is.na(..tmp[['..packages']])])
      ..tmp[['..rave_future_obj']] = future::future({
        rave::eval_dirty(..async_quo)#, env = async_env)
        if(is.null(..async_var)){
          return(environment())
        }else{
          re = sapply(..async_var, get0, simplify = F, USE.NAMES = T)
          return(list2env(re))
        }
      }, packages = ..tmp[['..packages']], evaluator = future::multiprocess,
      envir = ..tmp, gc = T)
    }


    ..env$results$get_value = function(key, ifNotFound = NULL){
      get0(key, envir = results, ifnotfound = ifNotFound)
    }
    ..env$results$async_value = function(key){
      if(!..tmp[['..async']]){
        stop('This module has no async part.')
      }else{
        if(future::resolved(..tmp[['..rave_future_obj']])){
          env = ..tmp[['..rave_future_env']]
          if(!is.environment(env)){
            env = ..tmp[['..rave_future_env']] = future::value(..tmp[['..rave_future_obj']])
          }
          get0(key, envir = env)
        }
      }

    }

    ..re = sapply(!!outputIds, function(nm){
      ..f = get0(nm, envir = results, inherits = TRUE, ifnotfound = NULL)
      if(!is.function(..f)){
        return(function(...){
          cat2('Function ', nm, ' is not available.', level = 'ERROR')
        })
      }else{
        fm = formals(..f)

        if(!length(fm)){
          # Case 1: fm is NULL, meaning this is temp function or customized output
          ..f
        }else{
          # Case 2: ..f is a package function
          fm = fm[-1]
          nms = names(fm)
          has_dots = '...' %in% nms
          nms = nms[!nms %in% c('', '...')]

          f = function(){
            args = sapply(nms, get0, inherits = FALSE, simplify = F, USE.NAMES = T)
            if(has_dots){
              args = c(list(..env$results), args, list(...))
            }else{
              args = c(list(..env$results), args)
            }

            do.call(..f, args)
          }
          formals(f) = fm
          f
        }
      }

      # eval(call("function", as.pairlist(fm), rhs), env, env)
      # call("function", as.pairlist(fm), rhs)
    }, simplify = F, USE.NAMES = T)

    return(c(..env, ..re))
  }))
  formals(FUN) = args

  return(FUN)

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
          get_module(package = x[4], module_id = x[1])
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
