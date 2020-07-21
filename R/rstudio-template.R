# Function to create a RAVE module template
create_template <- function(path, ...){
  this_call = match.call()
  catgl(deparse(this_call))
  args = list(...)
  
  # step 1: get package name
  # path = normalizePath(path)
  
  catgl('Creating RAVE Module -', path)
  
  PACKAGE = utils::tail(strsplit(path, '\\\\|/')[[1]],1)
  MODULEID = args[['module_id']]
  
  # ensure path exists
  dir_create(path)
  dir_create(file.path(path, 'inst', 'tools'))
  dir_create(file.path(path, 'inst', 'modules', MODULEID))
  dir_create(file.path(path, 'R'))
  dir_create(file.path(path, 'vignettes'))
  
  
  
  # check MODULEID, must starts with 'a-zA-z' and only contains 'a-zA-Z0-9_'
  MODULEID = gsub('[^a-zA-Z_]', '', MODULEID)
  MODULEID = gsub('[_]{2,}', '_', MODULEID)
  if(MODULEID == ''){
    MODULEID = 'module_id'
  }
  catgl('First Module ID -', MODULEID)
  
  MODULELABEL = args[['module_label']]
  MODULELABEL = gsub('(^[\\ ]*)|([\\ ]$)', '', MODULELABEL)
  if(MODULELABEL == ''){
    MODULELABEL = 'Missing Label'
  }
  catgl('First Module Label -', MODULELABEL)
  
  # migrate template
  template_dir = system.file('template', package = 'rave')
  # template_dir = './inst/template'
  fs = list.files(template_dir, recursive = T, pattern = '^[a-zA-Z]', all.files = F, full.names = F)
  
  for(f in fs){
    s = readLines(file.path(template_dir, f))
    s = paste(s, collapse = '\n')
    s = raveio::glue(s, .open = "${{", .close = "}}")
    f = stringr::str_replace(f, 'first_example', MODULEID)
    writeLines(s, con = file.path(path, f))
  }
  
  # Write .Rbuildignore
  s = c("^.*\\.Rproj$", "^\\.Rproj\\.user$", "^hello\\.R$", "^readme\\.md$",  "adhoc/")
  writeLines(s, con = file.path(path, '.Rbuildignore'))
  
  # Rename template.Rproj
  
}





# rstudioapi::createProjectTemplate(binding = 'create_template', package = '.', title = 'RAVE Module', open_files = c('R/inputs.R', 'R/outputs.R'), )
