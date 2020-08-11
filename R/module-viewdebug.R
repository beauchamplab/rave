#' Parse 'RAVE' Module and Returns Parsed Content in Environments
#' @param module_id module ID
#' @param sidebar_width input sidebar width
#' @param parse_context parse context, default is \code{"rave_running_local"}
#' @export
to_module <- function(module_id, sidebar_width = 3, parse_context = c(
  'rave_running_local', 'rave_running'
)){
  parse_context = match.arg(parse_context)
  ctx = rave_context(disallowed_context = 'default')
  pkg_name = ctx$package
  .__rave_module__. = module_id
  catgl('parsing module ', module_id, ' in the following context: ', parse_context, level = 'INFO')
  
  quos = parse_components(module_id, parse_context)
  
  tempdir = file.path(tempdir(), 'rave_modules', module_id)
  dir.create(tempdir, showWarnings = F, recursive = T)
  tmpfile = tempfile(pattern = module_id, tmpdir = tempdir)
  
  asis = quos$script_env$asis
  asis %?<-% FALSE
  
  src = sapply(quos$script_env$source, function(f){
    
    if(rlang::is_quosure(f)){
      expr = paste(deparse(rlang::quo_squash(f)), collapse = '\n')
      return(expr)
    }else if(f != '' && file.exists(f)){
      
      if(!asis){
        # This file is valid R script,
        fname = unlist(stringr::str_split(stringr::str_trim(f), '/|\\\\'))
        fname = utils::tail(fname, 1)
        
        fpath = file.path(tempdir, fname)
        file.copy(f, fpath, overwrite = TRUE)
        
        catgl("Copying ", f, ' >> ', fpath)
        
        # if the file ends with .R, source it
        if(stringr::str_detect(fname, pattern = '\\.[Rr]$')){
          quo = rlang::quo(source(!!fname, local = TRUE))
          expr = paste(deparse(rlang::quo_squash(quo)), collapse = '\n')
          return(expr)
        }
      }else{
        if(stringr::str_detect(f, pattern = '\\.[Rr]$')){
          quo = rlang::quo(source(!!f, local = TRUE))
          expr = paste(deparse(rlang::quo_squash(quo)), collapse = '\n')
          return(expr)
        }
      }
      
      
    }else{
      catgl("Cannot find path to ", f, level = 'ERROR')
    }
    return(NULL)
  })
  names(src) = NULL
  
  exec = rlang::quo(rave_execute(!!!get_main_function(module_id)))
  
  funs = sapply(names(quos$output_functions), function(nm){
    f = quos$output_functions[[nm]]
    
    s = paste(deparse(rlang::quo_squash(f)), collapse = '\n')
    s = paste(nm, '<-', s)
    s
  }, simplify = T, USE.NAMES = F)
  
  s = unlist(c(
    src,
    deparse(rlang::quo_squash(quos$rave_inputs_quo)),
    deparse(rlang::quo_squash(quos$rave_update_quo)),
    deparse(rlang::quo_squash(quos$rave_output_quo)),
    deparse(rlang::quo_squash(exec)),
    funs
  ))
  
  writeLines(s, tmpfile)
  m = rave::ModuleEnvir$new(module_id = module_id, label_name = get_module_label(module_id),
                            script_path = tmpfile, parent_env = asNamespace(pkg_name))
  m$from_package = TRUE
  m$package_name = pkg_name
  m$sidebar_width = sidebar_width
  m
}

#' Debug-use only, reload package, mount demo subject, and launch shiny app
#' @param module_id module ID to debug
#' @param sidebar_width input width, from 1 to 11, default is 3
#' @param launch.browser whether to launch browser, default is true, other
#' options are \code{rstudioapi::viewer} or false.
#' @param reload whether to reload package first. default is true,
#' @param ... passed to \code{\link[rave]{init_app}}
#' @export
view_layout <- function(module_id, sidebar_width = 3, 
                        launch.browser = TRUE, reload = TRUE, ...){
  rave_context('rave_module_debug')
  .__rave_module__. = module_id
  if(reload){
    reload_module_package(expose = FALSE, clear_env = FALSE)
  }
  
  mount_demo_subject()
  
  m = to_module(module_id = module_id, sidebar_width = sidebar_width, parse_context = 'rave_running_local')
  
  init_app(m, launch.browser = launch.browser, disable_sidebar = TRUE, 
           simplify_header = TRUE, ...)
}

