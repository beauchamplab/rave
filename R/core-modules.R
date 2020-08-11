to_package_name <- function(module_id){
  pkgName = paste('RAVE', module_id)
  pkgName = stringr::str_replace_all(pkgName, '[\\W_]', '')
  return(pkgName)
}

#' Load RAVE Modules
#' @param legacy for internal debug use
#' @export
load_modules <- function(legacy = FALSE){
  
  if(!legacy){
    if(!dipsaus::package_installed('ravebuiltins')){
      if( requireNamespace('devtools') ){
        tryCatch({
          devtools::install_github('beauchamplab/ravebuiltins', upgrade = 'never', force = FALSE)
        }, error = function(e){
          stop('Fail to install RAVE builtin modules. Please install it manually via the following command!\n\tdevtools::install_github("beauchamplab/ravebuiltins")')
        })
      }
    }
    
    m = detect_modules('ravebuiltins', rave_running = TRUE)
    return(m)
  }
  
  modules = utils::read.csv(rave_options('module_lookup_file'), stringsAsFactors = FALSE)
  
  #1. filter out all deactived packages
  modules = modules[modules$Active, ]
  
  stopifnot2(nrow(modules) > 0, msg = paste0(
    'Is there any module specified in ', rave_options('module_lookup_file'), ' ?'
  ))
  
  #2. check if all compiled packages exists
  pkg_ids = unique(modules$PackageID)
  pkg_ids = pkg_ids[!is.na(pkg_ids) & !is.blank(pkg_ids)]
  
  gt_md = function(x){
    m = as.list(x)
    if(is_invalid(m$PackageID, .invalids = c('null', 'na', 'blank'))){
      module = ModuleEnvir$new(
        module_id = m$ModuleID,
        label_name = m$Name,
        script_path = m$ScriptPath,
        version = m$Version,
        author = m$Author,
        packages = m$Packages
      )
    }else{
      pkg = to_package_name(m$PackageID)
      module = do.call('::', list(pkg, 'rave_module'))(module_id = m$ModuleID, launch = F)
    }
    return(module)
  }
  
  #3 Build module list
  groups = modules$GroupName
  sel = !is.na(groups) & !is.blank(groups)
  if(sum(sel)){
    sapply(unique(groups[sel]), function(gid){
      g = sel & groups == gid
      apply(modules[g, ], 1, gt_md)
    }, simplify = F, USE.NAMES = T) ->
      module_inst
  }else{
    module_inst = list()
  }
  
  if(sum(!sel)){
    sapply(modules$ModuleID[!sel], function(mid){
      g = (!sel) & modules$ModuleID == mid
      apply(modules[g, ], 1, gt_md)
    }, simplify = F, USE.NAMES = F) ->
      module_alone
  }else{
    module_alone = list()
  }
  
  module_inst[['______']] = module_alone
  
  module_inst
}



#' Check all packages to for new RAVE module packages
#' @param packages array of packages to search for, default is all packages
#' @param as_module logical, try to return module instances or just a list of modules
#' @param ... ignored for compatibility purpose
#' @export
detect_modules <- function(packages, as_module = TRUE, ...){
  
  lib_path = .libPaths()
  
  if(missing(packages)){
    packages = unlist(sapply(lib_path, function(lp){
      list.dirs(lp, recursive = FALSE, full.names = FALSE)
    }, simplify = FALSE))
    packages = unique(packages)
  }
  
  yaml_path = sapply(packages, function(p){
    system.file('rave.yaml', package = p)
  })
  
  sel = yaml_path != ''
  
  if(!sum(sel)){
    return(NULL)
  }
  
  packages = packages[sel]
  yaml_path = yaml_path[sel]
  
  m_info = cbind(packages, yaml_path)
  
  # load yaml
  m_data = lapply(seq_len(nrow(m_info)), function(ii){
    x = m_info[ii, ]
    pname = x[1]
    ypath = x[2]
    
    tryCatch({
      conf = raveio::load_yaml(ypath)
      do.call('rbind', lapply(conf$modules, function(m){
        module_id = m$module_id
        label_name = m$module_label
        label_name %?<-% sprintf('No Label (%s)', module_id)
        group_name = m$group_name
        group_name %?<-% '______'
        order = m$order
        order %?<-% Inf
        c(module_id, label_name, group_name, pname, order)
      }))
      
    }, error = function(e){
      NULL
    })
  })
  
  m_data = dipsaus::drop_nulls(m_data)
  
  if(!length(m_data)){
    return(NULL)
  }
  
  m_data = do.call('rbind', m_data)
  m_data = m_data[order(as.numeric(m_data[, 5])), 1:4, drop = FALSE]
  
  if(as_module){
    gnames = unique(m_data[,3])
    modules = sapply(gnames, function(gname){
      sel = m_data[,3] == gname
      
      m = lapply(which(sel), function(ii){
        x = m_data[ii,]
        
        tryCatch({
          get_module(package = x[4], module_id = x[1])
        }, error = function(e){
          catgl(e, level = 'WARNING')
          catgl('Error found! Please check dependencies. Will not import module ', x[1], level = 'INFO')
        })
      })
      
      m = dipsaus::drop_nulls(m)
      if(!length(m)) m = NULL
      m
    }, simplify = F, USE.NAMES = T)
    
    modules = dipsaus::drop_nulls(modules)
    if(!length(modules)){
      return(NULL)
    }
    
    return(modules)
  }else{
    return(m_data)
  }
  
}


#' Function to find modules in packages
#' @param package package name to search for modules
#' @param module_id (optional) module ID if the package contains multiple modules
#' @param local run module locally?
#' @param ... ignored for compatibility purpose
#' @export
get_module <- function(package, module_id, local = FALSE, ...){
  require(rave)
  # rave_context()
  
  .__rave_context__. = 'rave_module_debug'
  .__rave_package__. = package
  
  
  if(local){
    if(missing(module_id)){
      catgl('You are running module locally. Please specify module ID.', level = 'ERROR')
      return(invisible())
    }else{
      # FIXME
      .__rave_context__. = 'rave_running_local'
      .__rave_module__. = module_id
      return(module_as_function(package = package, module_id = module_id, reload = FALSE))
    }
  }
  
  # If you see context error, uncomment this line and expose the error
  # load_rave_yaml()
  conf = tryCatch({
    load_rave_yaml()
  }, error = function(e){
    catgl('Package ', package, ' has no RAVE modules.', level = 'WARNING')
  })
  if(!length(conf)){
    return(invisible())
  }
  ids = sapply(conf$modules, '[[', 'module_id')
  
  if(missing(module_id)){
    module_id = ids
  }else if(any(!module_id %in% ids)){
    catgl('Cannot find module ', paste(module_id[!module_id %in% ids], collapse = ', '), ' in package ', package, ' - Terminate.', level = 'ERROR')
    return(invisible())
  }
  
  if(length(module_id) == 1){
    catgl('Compile module ', module_id)
    
    .__rave_context__. = 'rave_running_local'
    .__rave_module__. = module_id
    
    module = to_module(module_id = module_id, sidebar_width = 3L, 
                       parse_context = 'rave_running_local')
    catgl('Compiled module ', module_id, '; path: - ',
         module$script_path)
    
    return(module)
  }else{
    modules = lapply(module_id, function(mid){
      .__rave_context__. = 'rave_running_local'
      .__rave_package__. = package
      .__rave_module__. = module_id
      
      tryCatch({
        to_module(module_id = mid, sidebar_width = 3L, parse_context = 'rave_running_local')
      }, error = function(e){
        catgl('An error occurred during parsing module ', mid, ' (', package, '). Please check source code if you are module developer. [Ignored]', level = 'WARNING')
        NULL
      })
      
    })
    
    modules = dipsaus::drop_nulls(modules)
    if(length(modules) == 0){
      modules = NULL
    }else if(length(modules) == 1){
      modules = modules[[1]]
    }
  }
}










module_as_function <- function(package = package, module_id = module_id, reload = FALSE, ...){
  
  .__rave_context__. = 'rave_running_local'
  .__rave_package__. = package
  .__rave_module__. = module_id
  # .__rave_module_instance__.
  
  if(reload){
    reload_module_package(expose = FALSE, clear_env = FALSE)
  }
  
  # Load UI
  
  # comps = parse_components(module_id = module_id, parse_context = 'rave_running_local')
  
  has_subject = any_subject_loaded()
  
  if(!has_subject){
    catgl('Error: No subject found! Please load subject first', level = 'ERROR')
    return(invisible())
  }
  
  if(has_subject && !'rave_data' %in% search()){
    attachDefaultDataRepository()
  }
  
  # assign('aaa', env, envir = globalenv())
  param_env = init_module(module_id = module_id, debug = FALSE, parse_context = 'rave_running_local')
  
  
  runtime_env = new.env(parent = param_env)
  
  envs = get_comp_env(module_id = module_id, parse_context = 'rave_running_local')
  get_content(content = envs$content, env = envs$tmp_env)
  inputs = lapply(envs$input_env, function(comp){
    if(inherits(comp, 'comp_input')){
      return(comp$inputId)
    }else{
      NULL
    }
  })
  inputs = unlist(inputs); names(inputs) = NULL
  
  args = as.list(param_env)[inputs]
  
  main_quos = get_main_function(module_id)
  
  outputIds = lapply(envs$output_env, function(comp){
    if(inherits(comp, 'comp_output')){
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
  if(async){
    async_quo = async_quo[[1]]
  }else{
    async_quo = {}
  }
  
  async_vars = main_quos$async_vars
  
  body(FUN) = rlang::quo_squash(rlang::quo({
    !!!normal_quos
    
    results = environment()
    ..env = list()
    
    ..env$results = new.env()
    
    ..tmp = new.env()
    
    ..tmp[['..async']] = FALSE
    
    if(!!async){
      ..tmp[['..async']] = TRUE
      pkgs = stringr::str_match(search(), '^package:(.+)$')[,2]
      pkgs = unique(pkgs[!is.na(pkgs)])
      ..tmp[['..rave_future_obj']] = future::future({
        dipsaus::eval_dirty(quote({!!async_quo}))#, env = async_env)
        async_vars = !!async_vars
        if(is.null(async_vars)){
          return(as.list(environment()))
        }else{
          re = sapply(async_vars, get0, simplify = F, USE.NAMES = T)
          return(re)
        }
      }, packages = pkgs, evaluator = future::multiprocess,
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
          if(!(is.environment(env) || is.list(env))){
            env = ..tmp[['..rave_future_env']] = future::value(..tmp[['..rave_future_obj']])
          }
          env[['key']]
        }
      }
      
    }
    
    ..env$results$get_variables <- function(level = 2, env = results){
      res <- names(env)
      if( level > 0 ){
        res <- c(res, ..env$results$get_variables(level - 1, env = parent.env(env)))
      } else {
        warning('results$get_variables is for debug use only')
      }
      unique(res)
    }
    
    ..re = sapply(!!outputIds, function(nm){
      ..f = get0(nm, envir = results, inherits = TRUE, ifnotfound = NULL)
      if(!is.function(..f)){
        return(function(...){
          catgl('Function ', nm, ' is not available.', level = 'ERROR')
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
          
          f = function(...){
            args = sapply(nms, function(..nm..){
              eval(rlang::sym(..nm..))
            }, simplify = F, USE.NAMES = T)
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
