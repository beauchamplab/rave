.cache_globals <- local({
  root_path = file.path('~/rave_modules/cache/globals')
  common_path = file.path(root_path, 'common')
  
  initialized = FALSE
  
  common_container = NULL
  
  # Cache has 8 levels
  #    Shared across                          Different in              acs_inst acs_mod persist   container            use case
  # 1. R session, shiny session, and modules                            T        T       T         common_container     rave_options
  # 2. shiny session, and modules             R session                 T        T       F         common_container     R options
  # 3. R session and modules                  shiny session             T        F       F         common_container     module constants, session-based
  # 4. R session and shiny session            modules                   T        F       T         common_container     module constants, persist
  # 5. shiny session only                     R session, modules        F        F       T         module_container     module global cache
  # 6. R session only                         shiny session, modules    NA
  # 7. modules only                           R session, shiny sessions NA
  # 8.                                        everything                F        F       F         module_container     intermediate results (baseline)
  
  # Default: current session, current module
  # global: shared across sessions: key - 
  
  check_init = function( module_id ){
    if(!initialized){
      initialized <<- TRUE
      common_container <<- dipsaus::PersistContainer$new(
        backend = dipsaus::text_map, path = common_path)
    }
    if(!missing(module_id)){
      module_container = dipsaus::PersistContainer$new(
        backend = dipsaus::text_map, path = file.path(root_path, module_id))
      return(module_container)
    }
  }
  
  cache = function(key, val, name, module_id, replace = FALSE,
                   across_instance = TRUE, across_module = TRUE, persist = FALSE){
    
    check_init()
    if( !across_instance ){
      module_container = check_init( module_id = module_id )
    }
    
    if( across_instance && across_module ){
      re = common_container$cache(key = name, signature = key, value = val, replace = replace, persist = persist)
      return(invisible(re))
    }
    
    if( across_instance && !across_module ){
      ns = shiny::NS(module_id)
      re = common_container$cache(key = ns(name), signature = key, value = val, replace = replace, persist = persist)
      return(invisible(re))
    }
    
    # across_instance is FALSE
    re <- module_container$cache(key = name, signature = key, value = val, replace = replace, persist = persist)
    return(invisible(re))
  }
  
  clear = function(levels = 1, module_id){
    if( 1 %in% levels && !missing(module_id) ){
      module_container = dipsaus::PersistContainer$new(
        backend = dipsaus::text_map, path = file.path(root_path, module_id))
      module_container$reset(all = (2 %in% levels))
    }
    if( 3 %in% levels ){
      check_init()
      common_container$reset(all = (4 %in% levels))
    }
  }
  
  remove = function(name, module_id){
    check_init()
    common_container$remove(keys = name, all = TRUE)
    if(!missing(module_id)){
      ns = shiny::NS(module_id)
      common_container$remove(keys = ns(name), all = TRUE)
      module_container = check_init( module_id = module_id )
      module_container$remove(keys = name, all = TRUE)
    }
  }
  
  has = function(name, module_id, key){
    
    mis_sig = missing(key)
    if( mis_sig ){
      check_init()
      re = common_container$has(key = name)
    }else{
      module_container = check_init( module_id = module_id)
      re = common_container$has(key = name, signature = key, sig_encoded = FALSE)
    }
    
    if( re ){
      attr(re, 'across_instance') = TRUE
      attr(re, 'across_module') = TRUE
      return(re)
    }
    
    if( missing(module_id) ){
      return(FALSE)
    }
    
    ns = shiny::NS(module_id)
    if( mis_sig ){
      re = common_container$has(key = ns(name))
    }else{
      re = common_container$has(key = ns(name), signature = key, sig_encoded = FALSE)
    }
    if( re ){
      attr(re, 'across_instance') = TRUE
      attr(re, 'across_module') = FALSE
      return(re)
    }
    
    if( mis_sig ){
      re = module_container$has(key = name)
    }else{
      re = module_container$has(key = name, signature = key, sig_encoded = FALSE)
    }
    if( re ){
      attr(re, 'across_instance') = FALSE
      attr(re, 'across_module') = FALSE
      return(re)
    }
    return(FALSE)
  }
  
  list(
    cache = cache,
    clear = clear,
    remove = remove,
    has = has,
    containers = function(){common_container}
  )
})


#' Cache R Objects with Different levels
#' @param key any R object, a named list would be the best.
#' @param val value to cache, if key exists, then value will not be evaluated nor saved
#' @param name name of the dataset
#' @param replace if true, force replace the cached object with current one
#' @param across_instance logical, whether can be accessed across shiny session
#' @param across_module logical, whether can be accessed across 'RAVE' modules
#' @param persist logical, whether persist on the hard-disk
#' @param module_id character, used if object not shared across modules
#' @param levels levels when clear the cache
#' @examples
#' 
#' print(cache('a', 1, name = 'data')) # returns 1
#' print(cache('a', 2, name = 'data')) # still returns 1
#' 
#' # clear cache
#' clear_cache(levels = 3)
#' print(cache('a', 2, name = 'data')) # Now returns 2
#' 
#' # Not run because a is cached
#' print(cache('a', 2, name = 'data'))
#' print(cache('a', {Sys.sleep(10); 1}, name = 'data'))
#' 
#' # Persist on hard-disk
#' cache('aa', 1:1000000, name = 'junk', replace = TRUE, persist = TRUE)
#' has_cache('junk')                   # Has data called 'junk'
#' has_cache('junk', key = 'aaa')      # but no data called 'junk' matching the key
#' @name rave-cache
NULL

#' @rdname rave-cache
#' @export
cache <- .cache_globals$cache


#' @rdname rave-cache
#' @export
clear_cache <- .cache_globals$clear

#' @rdname rave-cache
#' @export
remove_cache <- .cache_globals$remove

#' @rdname rave-cache
#' @export
has_cache <- .cache_globals$has


