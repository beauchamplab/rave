# saving rave history

RAVEHistory <- R6::R6Class(
  classname = 'RAVEHistory',
  private = list(
    save_path = NULL,
    env = NULL,
    use_yaml = F
  ),
  public = list(

    initialize = function(path = '~/', name = '.rave.object.history', use_yaml = F){
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
      try(
        logger('new save')
      )
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




