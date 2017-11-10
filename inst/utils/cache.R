
# Get cache object or create/replace a new one
# The cache path will be:
#    ./[cache_root_dir]/[func_name]/[path_dirs[1]]/[path_dirs[2]]/.../[cache_name].R
# path_dirs is a list of arg names that is inside of ...
#
get_or_create_cache = function(
  func_name,  # Function name
  cache_root_dir,
  envir = parent.frame(),
  cache_name = 'default', # Default cache name: ..../default.RData
  refresh = FALSE, # if refresh is True, do not load cache, create or replace a new one
  path_dirs = NULL, # A list of names that should be used to create sub path, example c('subject'),
  ... # additional args to be passed to func, is path_dirs is not null, for e.g., path_dirs = c('subject'), then ... must contains: `subject = 'XXXXXX'`
){
  
  args = list(...)
  sub_dir = ifelse(is.null(path_dirs), '', sapply(path_dirs, function(p){args[[p]]}))
  
  cache_path = file.path(
    cache_root_dir, func_name, sub_dir, str_c(cache_name, '.RData')
  )
  utils$logger('Load or create cache from: ', cache_path)
  
  # Create a new blank environment 
  tmp_env = new.env()
  with(tmp_env, {
    got_cache = FALSE
    if(!refresh && file.exists(cache_path)){
      load(cache_path)
      
      if(exists('CACHED_OBJ')){
        got_cache = TRUE
      }
    }
  })
  
  if(!tmp_env$got_cache){
    utils$logger('Cache data')
    # create/update cache!
    func = get(func_name, envir = envir)
    tmp_env$CACHED_OBJ = func(...)
    
    if(!file.exists(cache_path)){
      utils$create_directory(cache_path, recursive = T)
    }
    save(CACHED_OBJ, file = cache_path, envir = tmp_env)
  }
  
  return(tmp_env$CACHED_OBJ)
}


