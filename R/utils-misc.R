

dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    catgl('Cannot create directory at ', shQuote(x), level = 'FATAL')
  }
  invisible(normalizePath(x))
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


#' @title Get Value or Default
#' @param x a list, or environment, or just any R object
#' @param key the name to obtain from \code{x}. Default is \code{NULL}
#' @param ... if the value is invalid, the default value to return
#' @param .invalids what counts as invalid? Default is \code{NULL} and 
#' \code{NA}, represented by \code{"null"} and \code{"na"}
#' @export
get_val <- function(x, key = NULL, ..., .invalids = c('null', 'na')){
  
  if(is.null(key)){
    val = x
  }else{
    val = x[[key]]
  }
  if(is_invalid(val, .invalids = .invalids)){
    if(...length() > 1){
      return(list(...))
    } else{
      return(...elt(1))
    }
  }
  return(val)
}

zero_length <- function(..., any = TRUE, na.rm = FALSE){
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
#' @param session shiny session instance
get_mem_usage <- function(modules = list(), 
                          data_envir = getDefaultDataRepository(),
                          session = getDefaultReactiveDomain()){
  modules = unlist(modules)
  # session = getDefaultReactiveDomain()
  on.exit({rm(modules)})
  
  
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

