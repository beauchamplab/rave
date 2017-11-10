# default module file
SHINY_DESCRIPTION = shiny::HTML('No Description<br />')

SHINY_VALIDATE = function(params){
  return(c())
}

ENABLE_SUMA = FALSE

UNIVARIATE_MODE = 'selected_electrode'


SHINY_OUTPUT = list(
  Default = list()
)

params = list()

INIT_TIME = Sys.time()

NOT_RUN = function(...){}


SHINY_RENDER = function(...){
  list(...)
}

SHINY_EXPORT = function(params){
  tmpdir <- file.path(
    config$module_export,
    MODULE_INFO$label,
    format(Sys.time(), '%Y%m%d_%H%M%S')
  )
  if(!file.exists(tmpdir)){
    utils$create_directory(tmpdir, recursive = T)
  }

  # First, export environment

  export_envir = utils$create_data_env()
  export_envir$subject = subject
  export_envir$electrode = electrode
  export_envir$module_id = MODULE_INFO$id
  export_module = export_envir$init_module_env(
    module_id = MODULE_INFO$id
  )
  export_module$params = params
  rm(list = c('ALL_MODULE_LIST', 'get_module_env',
              'get_or_shinirize', 'get_shinirized',
              'init_module_env', 'shinirize_module', 'SHINIRIZED'),
     envir = export_envir)
  rm(list = c('ENABLE_SUMA', 'INIT_TIME', 'MULTIVARIATE_MODE', 'NOT_RUN',
              'script_path', 'MODULE_INFO', 'SHINY_DESCRIPTION', 'SHINY_EXPORT'),
     envir = export_module)

  export_envir$set_data_env(subject = subject, electrode = electrode,
                            data = utils$get_current('data'), summary = utils$get_current('summary'))
  export_envir$MODULE = export_module
  init_env = function(){
    if('export_envir' %in% search()){
      detach('export_envir')
    }
    if('MODULE' %in% search()){
      detach('MODULE')
    }
    attach(export_envir)
    attach(MODULE)
  }
  end_env = function(){
    detach(export_envir)
    detach(MODULE)
  }
  expr = c(
    'source("scripts/env.R")\n',
    sprintf("load('%s')\n\n", file.path(tmpdir, 'cache.RData')),
    'init_env()\n\n',
    'subject = ', str_c(deparse(subject), collapse = '\n\t'), '\n\n',
    'electrode = ', str_c(deparse(electrode), collapse = '\n\t'), '\n\n',
    'params = ', str_c(deparse(params), collapse = '\n\t'), '\n\n',
    'exec = ', str_c(deparse(SHINY_EXECUTE), collapse = '\n\t'), '\n\n',
    'result = exec(params);\n\n',
    '# Now, to get results, try result[[section_name]][[output_name]].\n',
    'func = result[[1]];\n',
    'func()\n\n',
    '# Clean up:\n',
    'end_env()'
  )

  if(interactive()){
    showNotification(p(
      'Exporting module in background.', br(),
      'See local directory:', br(),
      tmpdir
    ), type = 'message', duration = 10)
  }
  flag = futures$run({
    cache_file = file.path(tmpdir, 'cache.RData')
    script_file = file.path(tmpdir, 'script.R')
    save(export_envir, init_env, end_env, file = cache_file)
    writeLines(expr, script_file, sep = '')
  }, globals = c('tmpdir', 'export_envir', 'init_env', 'end_env', 'expr'))
}
