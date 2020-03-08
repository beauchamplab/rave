
RAVEHistory <- R6::R6Class(
  classname = 'RAVEHistory',
  private = list(
    save_path = NULL,
    env = NULL,
    use_yaml = FALSE
  ),
  public = list(
    
    initialize = function(path = '~/', name = '.rave.history.yaml', 
                          use_yaml = FALSE){
      private$save_path = file.path(path, name)
      private$use_yaml = use_yaml
      if(!file.exists(private$save_path)){
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
        private$env = fastmap::fastmap() #new.env(hash = TRUE)
        self$save()
      }else{
        self$load()
      }
    },
    save = function(...){
      private$env$set('.save_time', Sys.time())
      args = list(...)
      for(nm in names(args)){
        if(nm != ''){
          private$env$set(nm, args[[nm]])
        }
      }
      if(private$use_yaml){
        yaml::write_yaml(private$env$as_list(),
                         file = private$save_path)
      }else{
        saveRDS(private$env, file = private$save_path)
      }
      
    },
    load = function(){
      if(private$use_yaml){
        re = yaml::read_yaml(file = private$save_path)
        if(length(private$env) < 10){
          private$env = fastmap::fastmap()
        }
        
        for(nm in names(re)){
          private$env$set(nm, re[[nm]])
        }
      }else{
        private$env = readRDS(file = private$save_path)
        if(length(private$env) < 10){
          private$env = fastmap::fastmap()
        }
      }
      
    },
    get_or_save = function(key, val = NULL, save = TRUE, inherits = FALSE){
      if(private$env$has(key)){
        return(private$env$get(key = key))
      }else if (!is.null(val)){
        if(save){
          do.call(self$save, args = structure(list(val), names = key))
        }
        return(val)
      }
      return(NULL)
    },
    clear = function(){
      private$env$reset()
      self$save()
    }
  )
)

rave_hist <- local({
  h <- NULL
  function(){
    if(is.null(h)){
      h <<- RAVEHistory$new(use_yaml = TRUE)
    }
    h
  }
})


last_entry <- function(key, default, save = FALSE, group = 'customized'){
  stopifnot2(is.character(key), msg = 'Key must be a string')
  dict = rave_hist()$get_or_save(key = group, val = list(), save = FALSE)
  val = dict[[key]]
  val %?<-% default
  
  str_v = paste(val, collapse = ' ')
  str_v = stringr::str_sub(str_v, end = min(stringr::str_length(str_v), 20))
  catgl("[ {group} ] Last entry of { paste(key, collapse = '') }: { str_v }")
  if(save){
    dict[[key]] = default
    arg = list(dict); names(arg) = group
    do.call(rave_hist()$save, arg)
    return(invisible(val))
  }else{
    return(val)
  }
}



