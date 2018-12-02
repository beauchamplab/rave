# package = 'XXX'
# root_path = '.'
# restart_command
# config

module_utils <- function(module_id, module_label, config_file, package, root_path){
  `__env__` = environment()
  penv = loadNamespace(package)
  module_config = NULL
  
  reload_package = function(){
    'Re-compile the package and install the package.
  Parameters:
      path: path to package dir
      command: R command after restarting session
  '
    rutabaga::cat2('Reloading package...')
    ee = devtools::load_all(root_path, reset = TRUE, export_all = TRUE, export_imports = FALSE)
    `__env__`$penv = ee$env
  }
  
  # Function to obtain path, relative to root dir
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
  
  
  # Load module.yaml
  load_module_yaml <- function(){
    
    path = file.path(root_path, 'inst', config_file)
    path = get_path(path, mustWork = T) 
    config = yaml::read_yaml(path)
    config$module_id = module_id
    config$module_label = module_label
    
    `__env__`$module_config = config
    invisible(config)
  }
  load_module_yaml()
  
  # Converts configurations into RAVE input components
  modularize_inputs <- function(is_reactive = FALSE){
    config = module_config
    input_ids = names(config$inputs)
    lapply(input_ids, function(inputId){
      comp = config$inputs[[inputId]]
      comp$inputId = inputId
      comp$label %?<-% '(Missing label)'
      init_quo = do.call(penv$to_input_component, comp)
      
      # get update quos
      if(
        !is.null(comp$init) &&
        length(comp$init) == 1 &&
        is.character(comp$init) &&
        is.function(penv[[comp$init]])
      ){
        update_quo = rlang::quo(local(!!body(penv[[comp$init]])))
      }else{
        update_quo = NULL
      }
      list(
        inputId = comp$inputId,
        init_quo = init_quo,
        update_quo = update_quo
      )
    }) ->
      inputs
    
    input_quos = lapply(inputs, function(comp){
      # extract expressions
      rlang::quo_squash(comp$init_quo)
    })
    
    # Generate rave_inputs
    rave_input_quo = rlang::quo({
      rave_inputs(
        !!!input_quos,
        .input_panels = !!config$input_layouts
      )
    })
    
    # Generate rave_updates
    # names
    var_names = c('', input_ids)
    
    
    checks = unlist(config$data_checks)
    if(length(checks) && is_reactive){
      check_expr = rlang::quo({
        rave_checks(data = !!checks)
      })
    }else{
      check_expr = quote({})
    }
    
    if(is.function(penv[[config$init_function]])){
      init_expr = rlang::quo(!!body(penv[[config$init_function]]))
    }else{
      init_expr = quote({})
    }
    
    update_quos = c(
      rlang::quo({
        !!check_expr
        !!init_expr
      }),
      lapply(inputs, function(comp){ comp$update_quo })
    )
    
    names(update_quos) = var_names
    update_quos = dropNulls(update_quos)
    update_quos = sapply(update_quos, rlang::quo_squash, USE.NAMES = T, simplify = F)
    
    
    rave_updates_quo = rlang::quo({
      rave_updates(!!!update_quos)
    })
    
    return(list(
      rave_input_quo = rave_input_quo,
      rave_updates_quo = rave_updates_quo
    ))
  }
  
  #' Converts configurations into RAVE output components
  modularize_outputs <- function(is_reactive = FALSE){
    config = module_config
    # Get input elements
    varnames = names(config$inputs)
    
    body = body(penv[[config$main_function]])
    args = formals(penv[[config$main_function]])
    
    # generate rave_execute
    rave_execute_quo = rlang::quo({
      rave_execute({
        # get params
        ._env = ..runtime_env
        ._args = sapply(!!varnames, get, envir = ._env, inherits = TRUE, simplify = F, USE.NAMES = T)
        
        # make function
        ._f = function(){!!!body}
        formals(._f) = !!args
        
        result = do.call(._f, ._args)
        
        result
      })
    })
    
    
    # generate output quos
    output_titles = sapply(config$outputs, '[[', 'title')
    output_ids = names(config$outputs)
    
    output_quos = lapply(output_ids, function(outputId){
      comp = config$outputs[[outputId]]
      
      # create a fake function takes no arg
      fake_fname = paste0('._tmp_output_', outputId)
      
      fake_quo = rlang::quo(
        assign(!!fake_fname, function(){
          
          # Check if function exists
          fname = !!outputId
          f = NULL
          if(exists(fname, inherits = T)){
            f = get0(fname)
          }
          
          if(!is.function(f)){
            message = paste0('Cannot find function ', fname)
            cond = structure(list(message = message),
                             class = c("shiny.silent.error", "validation", "error", "condition"))
            stop(cond)
          }
          
          if(!exists('result') || !is.list(result)){
            result = list()
          }
          
          f(result)
        })
      )
      
      comp$outputId = fake_fname
      
      out_quo = do.call(penv$to_output_component, comp)
      
      list(
        fake_quo = fake_quo,
        out_quo = out_quo
      )
    })
    
    # extract fake calls
    fake_quos = lapply(output_quos, '[[', 'fake_quo')
    output_quos = lapply(output_quos, '[[', 'out_quo')
    
    names(output_quos) = output_titles
    
    # convert layouts for outputs
    width = config$output_layouts[['width']]
    width %?<-% 12L
    config$output_layouts[['width']] = NULL
    output_layouts = sapply(config$output_layouts, function(comp){
      if(is.list(comp)){
        sapply(comp, function(outputId){
          paste0('._tmp_output_', outputId)
        }, simplify = F, USE.NAMES = T)
      }
    }, simplify = F, USE.NAMES = T)
    output_layouts[['width']] = width
    
    rave_output_quo = rlang::quo({
      !!!fake_quos
      
      rave_outputs(!!!output_quos, .output_tabsets = !!output_layouts)
    })
    
    # Obtain reactive expressions
    if(is.function(penv[[config$reactive_function]])){
      rave_reactive_quo = rlang::quo(!!body(penv[[config$reactive_function]]))
    }else{
      rave_reactive_quo = quote({})
    }
    
    return(list(
      rave_reactive_quo = rave_reactive_quo,
      rave_execute_quo = rave_execute_quo,
      rave_output_quo = rave_output_quo
    ))
  }
  
  #' Converts configurations into RAVE components and launch GUI
  modularize = function(is_reactive = FALSE){
    c(
      modularize_inputs(is_reactive = is_reactive),
      modularize_outputs(is_reactive = is_reactive)
    )
    # content = paste(unlist(lapply(quos, rlang::quo_text)), collapse = '\n')
    # 
    # # write to a temp file
    # tf = tempfile(fileext = '.R')
    # 
    # writeLines(text = content, con = tf, sep = '\n')
    # 
    # m = ModuleEnvir$new(
    #   module_id = config$module_id, label_name = config$module_label, parent_env = penv,
    #   packages = package, script_path = tf)
    # 
    # init_app(m, test.mode = T)
  }
  
  # Function to detect if any file is changed
  last_digest = NULL
  monitor_file_changed = function(path = c('R', 'inst')){
    path = file.path(root_path, path)
    info = fileSnapshot(path, file.info = FALSE, md5sum = TRUE, recursive = TRUE)
    digest = digest::digest(info$info)
    if(!length(last_digest)){
      `__env__`$last_digest = digest
    }
    if(digest != last_digest){
      `__env__`$last_digest = digest
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  
  # ------------------------------------------------------------------------
  #
  # How should I reload a module?
  # Step1: reload package     - reload_package
  # Step2: load module yaml   - load_module_yaml
  # Step3: Refresh all quos   - modularize
  # Step4: Lauch app          - test_module
  # 
  # For interactive editing, we use monitor_file_changed to see if any file is changed
  # if so, reload 
  #
  # -------------------------------------------------------------------------
  
  test_module = function(test.mode = T, launch = TRUE, temp_file = tempfile(fileext = '.R')){
    # reload package first
    reload_package()
    
    # Reload module
    load_module_yaml()
    
    # Modulize
    quos = modularize(is_reactive = TRUE)
    
    # write to a temp file
    content = paste(unlist(lapply(quos, rlang::quo_text)), collapse = '\n')
    
    writeLines(text = content, con = temp_file, sep = '\n')
    
    m = ModuleEnvir$new(
      module_id = module_id, label_name = module_label, parent_env = penv,
      packages = package, script_path = temp_file)
    
    if(launch){
      init_app(m, test.mode = test.mode, theme = 'green')
    }else{
      return(m)
    }
    
  }
  
  dev_module = function(reactive = FALSE){
    # reload package first
    reload_package()
    
    # Reload module
    load_module_yaml()
    
    tool = get_toolbox(reactive = reactive)
    tool$attach_toolbox()
  }
  
  
  # A function to provide dev toolboxes
  get_toolbox = function(reactive = FALSE){
    # Create toolbox environment, which will be attached to search path
    toolbox = new.env(parent = emptyenv())
    fn_env = new.env()
    
    # A local environment storing inputs and outputs
    localenv = new.env(parent = emptyenv())
    localenv$reactive_inputs = list()
    localenv$reactive_outputs = list()
    
    if('rave_toolbox' %in% search()){
      detach('rave_toolbox', character.only = T)
    }
    
    # ------------------------ function environment ------------------------
    # Function to attach toolbox
    attach_toolbox = function(){
      sel = search() == 'rave_data'
      if(any(sel)){
        pos = which(sel) + 1
      }else{
        pos = 2L
      }
      if('rave_toolbox' %in% search()){
        detach('rave_toolbox', character.only = T)
      }
      attach(toolbox, name = 'rave_toolbox', warn.conflicts = FALSE, pos = pos)
    }
    
    
    fn_env$reload_package = reload_package
    
    
    # ------------------------ Toolbox ------------------------
    
    path = get_path(file.path(root_path, 'inst', 'utils', 'dev_default.R'), mustWork = TRUE)
    
    # Load dev functions
    local({
      source(path, local = TRUE)
    }, envir = fn_env)
    
    if(reactive){
      # load reactive dev functions
      local({
        source(get_path(file.path(root_path, 'inst', 'utils', 'dev_reactives.R'), mustWork = TRUE), local = TRUE)
        source(get_path(file.path(root_path, 'inst', 'utils', 'dev_extras.R'), mustWork = TRUE), local = TRUE)
      }, envir = fn_env)
    }
    
    # migrate to toolbox
    list2env(as.list(fn_env), toolbox)
    
    fn_env$attach_toolbox = attach_toolbox
    
    return(fn_env)
    # 
    #   preview = function(){
    #     'Launch GUI from RAVE
    #   '
    #     modularize(config)
    #   }
    
  }
  
  
  return(environment())
}
