
#' Cache R Objects with Different levels
#' @param key any R object, a named list would be the best.
#' @param val value to cache, if key exists, then value will not be evaluated nor saved
#' @param name,inputId character name of the dataset or input
#' @param replace if true, force replace the cached object with current one
#' @param persist logical, whether persist on the hard-disk, only used when 
#' \code{global=FALSE}, the persisted data will be used by each modules
#' @param test,read_only whether not to save the value if cache is not found
#' @param global whether to cache the variable in global environment. If true,
#' then the variable will be accessible from other instances and modules.
#' @param levels levels when clear the cache
#' @param temporary whether to use temporary map to cache, used internally.
#' @param ... ignored
#' @param session shiny session instance
#' 
#' @return Cached value, or \code{val}. If cache and \code{val} are both 
#' missing, then return \code{NULL}.
#' @examples
#' 
#' # global can be set to false within RAVE modules
#' print(cache('a', 1, name = 'data', global = TRUE)) # returns 1
#' print(cache('a', 2, name = 'data', global = TRUE)) # still returns 1
#' 
#' # clear cache (for global=TRUE)
#' clear_cache(levels = 1:3)
#' print(cache('a', 2, name = 'data', global = TRUE)) # Now returns 2
#' 
#' # Not run `Sys.sleep` because a is cached
#' print(cache('a', 2, name = 'data', global = TRUE))
#' print(cache('a', {Sys.sleep(10); 1}, name = 'data', global = TRUE))
#' 
#' # get data without key
#' cache(name = 'data', global = TRUE)
#' 
#' # clear cache that is global-only
#' clear_cache(levels = 2)
#' 
#' # Test (test=TRUE) if cache exists, if not, return value but no save
#' cache(name = 'abracadabra', val = 'no cache', global = TRUE, test = TRUE)
#' cache(name = 'abracadabra', global = TRUE)
#' 
#' # cache module inputs
#' \dontrun{
#' # Need to run in package module environment
#' cache_input('abracadabra', 'no-magic', read_only = TRUE)  
#' }
#' 
#' 
#' @name rave-cache
NULL


.cache_global_container <- local({
  map = NULL
  tmp_map = NULL
  function(temorary = FALSE){
    if(is.null(map)){
      map <<- dipsaus::session_map()
      map$has_locker = FALSE
      RaveFinalizer$new(function(...){
        map$destroy()
      })
      
      tmp_map <<- dipsaus::session_map()
      tmp_map$has_locker = FALSE
      RaveFinalizer$new(function(...){
        tmp_map$destroy()
      })
    }
    if( temorary ){
      tmp_map
    }else{
      map
    }
    
  }
})

.cache_global <- function(key, val, name, replace=FALSE, has_key = TRUE, 
                          test = FALSE, temporary = FALSE, ...){
  map = .cache_global_container(temporary)
  tres = FALSE
  if( has_key ){
    tres = map$has(name, signature = key)
  }else{
    tres = map$has(name)
  }
  if( test ){
    return(tres)
  }
  
  if( replace || !tres ){
    map$set(name, val, signature = key)
  }
  
  return(map$get(name))
  
}

.cache <- function(key, val, name, replace=FALSE, global=FALSE, 
                   persist=FALSE, test = FALSE, temporary = FALSE, ...){
  
  ctx = rave_context()
  instance = ctx$instance
  
  has_key = TRUE
  if(missing(val)){
    val = NULL
    replace = FALSE
  }
  if(missing(key)){
    key = NULL
    replace = FALSE
    has_key = FALSE
  }
  if(test){
    # has_key = FALSE
    replace = FALSE
  }
  
  if(!global && is.null(instance)){
    debug('Module is not activated, temporary cached {name} to global container. This warning only appears when developing modules.', level = 'WARNING')
    global = TRUE
  }
  
  if( global ){
    if( test ){
      # Variable is cached in global environment, check whether cache exists
      tres_gl = .cache_global(key = key, name = name, replace = FALSE, 
                              has_key = has_key, test = TRUE, 
                              temporary = temporary)
      # if exist in global cache, then obtain the value
      if(tres_gl){
        return(.cache_global(name = name, replace = FALSE, 
                             has_key = FALSE, test = FALSE, 
                             temporary = temporary))
      }
      
      # No cache found, return value
      return( val )
    }
    
    return(.cache_global(key = key, val = val, name = name, 
                         replace = replace, has_key = has_key, 
                         test = FALSE, temporary = temporary))
  }
  
  # global is false
  if( !replace ){
    if( has_key ){
      tres = instance$cache_env$has(name, signature = key)
    }else{
      tres = instance$cache_env$has(name)
    }
    if( tres ){
      return(instance$cache_env$get(name))
    }
    if( has_key ){
      tres = instance$module_env$cache_env$has(name, signature = key)
    }else{
      tres = instance$module_env$cache_env$has(name)
    }
    if( tres ){
      return(instance$module_env$cache_env$get(name))
    }
  }
  
  if( !has_key || test ){ return(val) }
  
  if( !persist ){
    instance$cache_env$set(name, val, key)
  }else{
    instance$cache_env$remove(name)
    instance$module_env$cache_env$set(name, val, key)
  }
  return( val )
  
}

#' @rdname rave-cache
#' @export
cache <- rave_context_generics('cache', .cache)

#' @rdname rave-cache
#' @export
cache.rave_running = .cache

#' @rdname rave-cache
#' @export
cache.rave_running_local <- function(..., global = TRUE){
  .cache(..., global = TRUE, temporary = TRUE)
}

#' @rdname rave-cache
#' @export
cache.default <- cache.rave_running_local


#' @rdname rave-cache
#' @export
cache_input <- function(inputId, val = NULL, read_only = TRUE, ..., 
                        session = getDefaultReactiveDomain()){
  ctx = rave_context(disallowed_context = 'default')
  module_id = ctx$module_id
  rave_id = add_to_session(session)
  
  key = list(
    type = '.rave-input-Dipterix',
    special = inputId %in% c('..onced'),
    module_id = module_id,
    rave_id = rave_id,
    msg = 'I need to add something that is hard to guess - Dipterix'
  )
  test = FALSE
  if(read_only){
    # Only check if name exists
    test = TRUE
  }else{
    nm = shiny::NS(module_id)(inputId)
    cache(key, val, nm, replace = !read_only, global = TRUE)
  }
  
  re = cache(key, val, inputId, replace = !read_only, global = FALSE, 
        persist = FALSE, test = test)
  debug('Cache {inputId} - {paste(deparse(re), collapse = "")}')
  re
}

#' @rdname rave-cache
#' @export
clear_cache <- function(levels = 1){
  ctx = rave_context()
  if(1 %in% levels){
    if(length(ctx$instance)){
      names = ctx$instance$cache_env$keys()
      input_ids = ctx$instance$input_ids
      names = names[!names %in% input_ids]
      ctx$instance$cache_env$remove(names)
    }
  }
  
  if(2 %in% levels){
    map = .cache_global_container()
    map$reset()
  }
  
  if(3 %in% levels){
    if(length(ctx$instance)){
      ctx$instance$module_env$cache_env$reset()
    }
  }
  tmp_map = .cache_global_container(TRUE)
  tmp_map$reset()
}
