##########################################
# File operations
# 

##### get_fname
# Get file/dir name
# 
# * full_path: file path
# * keep_extension: want to keep extension?
# 

path_sep = '/'

get_fname = function(full_path, keep_extension = FALSE) {
    remove_tail = function(x) x[-length(x)]

    name <- full_path %>%
        str_split("/|\\\\") %>%
        unlist %>%
        tail(1)

    if (keep_extension)
        return(name)

    name %>%
        str_split("\\.") %>%
        unlist %>%
        remove_tail %>%
        # because we split on '.' above, collapse back
        paste(collapse='.')
}

##### get_path_dir
# 
# get current directory, 
# 
# * pname: a path, if pname is a dir, return itself, if it's a file, return directory
# 
# 
get_path_dir = function(pname){
  pname %>% 
    str_split("/|\\\\") %>% 
    unlist %>% 
    tail(1) %>% 
    str_detect('\\.') ->
    is_file
  
  if(is_file){
    pname %>% 
      str_split("/|\\\\") %>% 
      unlist ->
      tmp
    tmp[-length(tmp)] %>% 
      str_c(collapse = path_sep) ->
      pname
  }
  
  pname
}


#### create_directory
# 
# Create directory if not exists
# 
# * dname: file/dir path
# 
create_directory <- function(dname, showWarnings = TRUE, recursive = TRUE) {
    dname = get_path_dir(dname)
    if(!dir.exists(dname)){
        dir.create(dname, showWarnings = showWarnings, recursive = recursive)
    }
}

############ format_path
# 
# Format path according to your os
# 
# * path: file/dir path
# * fsep: file separation, `\` on windows, `/` on mac and linux
# 
# 
format_path = function(path, fsep = path_sep){
  path %>% 
    str_replace_all('/|\\\\', str_replace_all(fsep, '\\\\', '\\\\\\\\'))
}

###### get_parent_dir
# 
# get parent directory
# 
# * pname: a file or directory
# 
get_parent_dir = function(pname){
  
  pname %>% 
    get_path_dir() %>% 
    str_split('/|\\\\') %>% 
    unlist %>% 
    lag() %>% 
    na.omit() %>% 
    str_c(collapse = path_sep)
}

