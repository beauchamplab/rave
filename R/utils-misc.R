#' Literally does nothing
#' @param ... nothing
#' @export
do_nothing <- function(...){
  
}

# Try to find path if not exists
find_path <- function(path, root_dir){
  if(file.exists(path)){
    return(path)
  }
  root_dir %?<-% rave_options('data_dir')
  path = unlist(stringr::str_split(path, '(/)|(\\\\)|(\\~)'))
  path = path[path != '']
  
  for(ii in 1:length(path)){
    tmp_path = do.call(file.path, as.list(c(root_dir, path[ii:length(path)])))
    if(file.exists(tmp_path)){
      return(tmp_path)
    }
  }
  
  # No path found
  return(NULL)
}


dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    dipsaus::cat2('Cannot create directory at ', shQuote(x), level = 'FATAL')
  }
  invisible(normalizePath(x))
}

to_datauri <- function(file, mime = ''){
  info = file.info(file)
  ss = jsonlite::base64_enc(input = readBin(file, what = 'raw', n = info$size))
  ss = sprintf('data:%s;base64,%s', mime, ss)
  ss
}


is.blank <- function(x){
  x == ''
}

is_invalid <- function(x, any = F, .invalids = c('null', 'na')){
  if('null' %in% .invalids){
    if(is.null(x) || !length(x)){
      return(TRUE)
    }
  }
  for(func in paste0('is.', .invalids)){
    res = do.call(func, args = list(x))
    if(length(res) > 1){
      if(any){
        res = any(res)
      }else{
        res = all(res)
      }
    }
    if(res){
      return(TRUE)
    }
  }
  return(FALSE)
}


get_val <- function(x, key = NULL, ..., .invalids = c('null', 'na')){
  
  if(is.null(key)){
    val = x
  }else{
    val = x[[key]]
  }
  if(is_invalid(val, .invalids = .invalids)){
    args = list(...)
    len = length(args)
    if(len){
      if(len == 1){
        val = args[[1]]
      }else{
        val = args
      }
    }
  }
  return(val)
}

zero_length <- function(..., any = T, na.rm = F){
  parent_env = parent.frame()
  args = as.list(match.call())[-1]
  len = length(args)
  if('any' %in% names(args)){
    len = len - 1
    args = args[1:len]
  }
  reNull = function(...){return(NULL)}
  for(i in 1:len){
    tryCatch({
      obj = eval(args[[i]], envir = parent_env)
      if(na.rm == TRUE || na.rm == i){
        obj = obj[!is.na(obj)]
      }
      obj
    }, error = reNull) ->
      obj
    if(any && length(obj) == 0){
      return(TRUE)
    }
    if(!any && length(obj) > 0){
      return(FALSE)
    }
  }
  return(!any)
}


try_normalizePath <- function(path, sep = c('/', '\\\\')){
  if(file.exists(path)){
    path = normalizePath(path)
    attr(path, 'is_absolute') = TRUE
    return(path)
  }else{
    # if dirname = itself
    dirname = dirname(path)
    if(dirname == path){
      attr(path, 'is_absolute') = FALSE
      return(path)
    }else{
      pre = Recall(dirname, sep = sep)
      
      p = unlist(stringr::str_split(path, pattern = paste(sprintf('(%s)', sep), collapse = '|')))
      fname = utils::tail(p, 1)
      
      path = file.path(pre, fname, fsep = '/')
      attr(path, 'is_absolute') = attr(pre, 'is_absolute')
      return(path)
    }
  }
}


#' Function to test local disk speed
#' @param file_size in bytes, default is 10 MB
#' @param quiet should verbose messages be suppressed?
test_hdspeed <- function(file_size = 1e7, quiet = FALSE){
  data_dir = rave_options('data_dir')
  
  if(!dir.exists(data_dir)){
    catgl('RAVE data directory is missing, please make sure the following directory exists: {data_dir}', level = 'ERROR')
    return(c(NA, NA))
  }
  
  # create tempdir for testing
  test_dir = file.path(data_dir, '.rave_hd_test', paste(sample(LETTERS, 8), collapse = ''))
  dir_create(test_dir)
  
  progress = progress(title = 'Testing read/write speed', max = 2, quiet = quiet)
  on.exit({
    unlink(test_dir, recursive = T)
    progress$close()
  })
  
  progress$inc('Write to disk...')
  
  # generate 10M file, tested
  file = tempfile(tmpdir = test_dir)
  dat = paste0(sample(LETTERS, file_size - 1, replace = T), collapse = '')
  upload = system.time(writeLines(dat, file, useBytes = T))
  
  progress$inc('Read from disk...')
  download = system.time({dat_c = readLines(file)})
  
  if(exists('dat_c') && dat_c != dat){
    catgl('Uploaded data is broken...', level = 'WARNING')
  }
  
  ratio = file.info(file)$size / 1000000
  
  speed = c(upload[3], download[3]) / ratio
  names(speed) = NULL
  return(speed)
}



time_diff <- function(start, end){
  delta = unclass(end-start)
  list(
    delta = as.numeric(delta),
    units = attr(delta, 'units')
  )
}


#' Get RAM usage
#' @param modules which module(s)
#' @param data_envir default uses \code{\link[rave]{getDefaultDataRepository}}
get_mem_usage <- function(modules, data_envir){
  if(missing(data_envir)){
    data_envir = getDefaultDataRepository()
  }
  if(missing(modules)){
    modules = NULL
  }else{
    modules = unlist(modules)
  }
  session = getDefaultReactiveDomain()
  on.exit({rm(data_envir, modules, session)})
  
  
  # get total memory used
  total_mem = mem_used()
  data_usage = object_size(data_envir)
  if(length(modules)){
    lapply(modules, function(m){
      
      module_ram = object_size(m)
      
      exec_env = m$get_or_new_exec_env(session = session)
      
      elem_ram = sapply(as.list(exec_env$runtime_env), function(o){
        tryCatch({
          object_size(o)
        }, error = function(e){
          0
        })
      })
      
      usage = 0
      if(length(elem_ram)){
        usage = sum(elem_ram[elem_ram < module_ram])
      }
      
      list(
        Name = m$label_name,
        usage = usage
      )
    }) ->
      module_usage
    names(module_usage) = NULL
    
    module_total = sum(sapply(module_usage, '[[', 'usage'))
    
  }else{
    module_usage = list()
    module_total = 0
  }
  
  
  misc_usage = total_mem - data_usage - module_total
  misc_usage = max(misc_usage, 0)
  list(
    total_mem = total_mem,
    data_usage = data_usage,
    module_usage = module_usage,
    other_usage = misc_usage
  )
}


#' Function to clear all elements within environment
#'
#' @param env environment to clean
#' @param ... ignored
#'
#' @examples
#' \dontrun{
#' env = new.env()
#' env$a = 1
#' print(as.list(env))
#'
#' clear_env(env)
#' print(as.list(env))
#' }
#' @export
clear_env <- function(env, ...){
  if(is.environment(env)){
    if(environmentIsLocked(env)){
      return(invisible())
    }
    nms = names(env)
    if(isNamespace(env)){
      nms = nms[!nms %in% c(".__NAMESPACE__.", ".__S3MethodsTable__.")]
    }
    rm(list = nms, envir = env)
  }
  return(invisible())
}

rand_string <- function (length = 10) {
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), 
        collapse = "")
}

#' RAVE Failure Message
#' @param message error message, character
#' @param level level of error message; can be chosen from \code{"INFO"},
#' \code{"WARNING"}, or \code{"ERROR"}
#' @param call call expression
#' @param .stop stop or just return the condition
#' @return Error condition or stop
#' @export
rave_failure <- function(message, level = 'ERROR', call = NULL, .stop = TRUE){
  class = c("rave-error", "shiny.custom.error", "error", "condition")
  level = stringr::str_to_upper(level)
  if( level %in% c('INFO', 'WARNING')){
    class = c(sprintf('rave-%s', stringr::str_to_lower(level)), class)
  }
  err = structure(list(message = as.character(message), call = call, level = level), 
                  class = class)
  if(.stop){
    stop(err)
  }
  return(err)
}

