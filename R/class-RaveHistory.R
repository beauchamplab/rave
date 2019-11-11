#' R6 class for small object caching
RAVEHistory <- R6::R6Class(
  classname = 'RAVEHistory',
  private = list(
    save_path = NULL,
    env = NULL,
    use_yaml = F
  ),
  public = list(
    
    initialize = function(path = '~/', name = '.rave.history.yaml', use_yaml = F){
      private$save_path = file.path(path, name)
      private$use_yaml = use_yaml
      if(!file.exists(private$save_path)){
        dir.create(path, recursive = T, showWarnings = F)
        private$env = new.env(hash = T)
        self$save()
      }else{
        self$load()
      }
    },
    save = function(...){
      private$env$.save_time = Sys.time()
      args = list(...)
      for(nm in names(args)){
        if(nm != ''){
          assign(nm, args[[nm]], envir = private$env)
        }
      }
      if(private$use_yaml){
        dat = as.list(private$env, all.names = T)
        
        yaml::write_yaml(dat,
                         file = private$save_path)
      }else{
        saveRDS(private$env, file = private$save_path)
      }
      
    },
    load = function(){
      if(private$use_yaml){
        re = yaml::read_yaml(file = private$save_path)
        if(!is.environment(private$env)){
          private$env = new.env()
        }
        for(nm in names(re)){
          assign(nm, re[[nm]], envir = private$env)
        }
      }else{
        private$env = readRDS(file = private$save_path)
        if(!is.environment(private$env)){
          private$env = new.env()
        }
      }
      
    },
    get_or_save = function(key, val = NULL, save = TRUE, inherits = FALSE){
      if(exists(key, envir = private$env, inherits = inherits)){
        return(get(key, envir = private$env, inherits = inherits))
      }else if (!is.null(val)){
        if(save){
          args = list(`.ignored` = val)
          names(args) = key
          do.call(self$save, args = args)
        }
        return(val)
      }
      return(NULL)
    },
    clear = function(){
      private$env = new.env()
      self$save()
    }
  )
)

rave_hist <- RAVEHistory$new(use_yaml = TRUE)

#' Obtain last saved value - disk cache
#' @param key key name
#' @param default if not found, return default
#' @param save replace cache
#' @param group character, donnot use "main_app" and "main_app2" since they are reserved
#' @export
last_entry <- function(key, default, save = F, group = 'customized'){
  stopifnot2(is.character(key), msg = 'Key must be a string')
  dict = rave_hist$get_or_save(key = group, val = list(), save = F)
  val = dict[[key]]
  val %?<-% default
  
  str_v = paste(val, collapse = ' ')
  str_v = stringr::str_sub(str_v, end = min(stringr::str_length(str_v), 20))
  catgl("[ {group} ] Last entry of { paste(key, collapse = '') }: { str_v }")
  if(save){
    dict[[key]] = default
    arg = list(dict); names(arg) = group
    do.call(rave_hist$save, arg)
    return(invisible(val))
  }else{
    return(val)
  }
}



