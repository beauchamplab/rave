
context("Dependencies")


test_that('Checking installed script dependencies', {
  
  skip_on_cran()
  
  fs = list.files(system.file('', package = 'rave'), pattern = '\\.[rR]$', full.names = TRUE, recursive = TRUE)
  pkgs = lapply(fs, function(f){
    s = readLines(f)
    s = stringr::str_trim(s)
    s = stringr::str_remove_all(s, '#.*$')
    unique(unlist(stringr::str_extract_all(s, pattern = '[a-zA-Z0-9.]+[:]{2,3}+[a-zA-Z0-9._]+')))
  })
  
  
  fns = unique(unlist(pkgs))
  fns = fns[!fns %in% c('devtools::install_')]
  fns = stringr::str_split_fixed(fns, '[:]+', 2)
  
  fns <- fns[!fns[,1] %in% c("pryr", "hdf5r"), ]
  
  apply(fns, 1, function(x){
    expect(
      exists(x[[2]], envir = asNamespace(x[[1]]), inherits = TRUE),
      failure_message = rave:::gl('Cannot find {x[[2]]} in package {x[[1]]}')
    )
  })
})


extract_calls <- function(expr){
  expr = as.list(expr)
  if(length(expr) <= 1){
    return(NULL)
  }
  caller = expr[[1]]
  is_call = is.call(caller)
  if( !is_call ){
    pm = tryCatch({
      is.primitive(eval(caller))
    }, error = function(e){FALSE})
    if( pm ){
      is_call = TRUE
    }
  }
  if( !is_call ){
    return(c(caller, unlist(lapply(expr[-1], extract_calls))))
  }else{
    return(unlist(lapply(expr[-1], extract_calls)))
  }
  
}

# test_that('R6 class - Module', {
#   
#   skip_on_cran()
#   rave_env = new.env(parent = loadNamespace('rave'))
#   
#   m <- get_module('ravebuiltins', 'power_explorer')
#   
#   instance = ModuleEnvir$new(
#     module_id = m$ModuleID,
#     label_name = m$Name,
#     script_path = m$ScriptPath,
#     version = m$Version,
#     author = m$Author,
#     packages = m$Packages
#   )
#   
#   rave_env$self = c(cls_generator$public_methods, cls_generator$public_fields, cls_generator$active)
#   rave_env$private = c(cls_generator$private_methods, cls_generator$private_fields)
#   fs = c(rave_env$self, rave_env$private)
#   
#   lapply(names(fs), function(nm){
#     f = fs[[nm]]
#     if(is.function(f)){
#       fns = extract_calls(body(f))
#       
#       
#     }
#   })
#   
# })

