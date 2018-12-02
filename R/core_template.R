#' Function to create a RAVE module template
create_template <- function(path, ...){
  args = list(...)

  # step 1: get package name
  # path = normalizePath(path)

  rutabaga::cat2('Creating RAVE Module -', path)

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, 'inst', 'utils'), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, 'R'), recursive = TRUE, showWarnings = FALSE)

  PACKAGE = tail(strsplit(path, '\\\\|/')[[1]],1)
  MODULEID = args[['module_id']]

  # check MODULEID, must starts with 'a-zA-z' and only contains 'a-zA-Z0-9_'
  MODULEID = gsub('[^a-zA-Z_]', '', MODULEID)
  MODULEID = gsub('[_]{2,}', '_', MODULEID)
  if(MODULEID == ''){
    MODULEID = 'module_id'
  }
  rutabaga::cat2('First Module ID -', MODULEID)

  MODULELABEL = args[['module_label']]
  MODULELABEL = gsub('(^[\\ ]*)|([\\ ]$)', '', MODULELABEL)
  if(MODULELABEL == ''){
    MODULELABEL = 'Missing Label'
  }
  rutabaga::cat2('First Module Label -', MODULELABEL)

  # migrate template
  template_dir = system.file('template', package = 'rave')
  # template_dir = './inst/template'
  fs = list.files(template_dir, recursive = T, pattern = '^[a-zA-Z]', all.files = F, full.names = F)

  fs = c('.Rbuildignore', fs)

  for(f in fs){
    s = readLines(file.path(template_dir, f))
    s = rave::fprintf(s)
    writeLines(s, con = file.path(path, f))
  }


}





# rstudioapi::createProjectTemplate(binding = 'create_template', package = '.', title = 'RAVE Module', open_files = c('R/inputs.R', 'R/outputs.R'), )
