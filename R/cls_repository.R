#' @include utils.R
NULL


#' DataRepository, inner-class definition for singleton \code{data_repository}.
#' @field get_data() get data environment that stores electrode data.
#'   Returns a \code{MultiElectrodes} instance
#' @field set_data(multi_electrodes) set data environment,
#'   multi_electrodes must be \code{MultiElectrodes} class
#' @field add_module(module) add module to the repository,
#'   module must be \code{ModuleEnv} class
#' @field get_module(id) get \code{ModuleEnv} instance by its id.
#' @field reset() remove all data imported as well as modules stored in this repository.
#' @seealso \code{\link{MultiElectrodes}}, \code{\link{ModuleEnv}}
#' @details DO NOT use this class directly, use the singleton \code{data_repository} instead.
#' This documentation only provides definitions for generating data_repository.
#' @keywords DataRepository, data_repository, R6Class
DataRepository <- R6::R6Class(
  'DataRepository',

  cloneable = FALSE,
  portable = FALSE,

  private = list(
    # Stores module instances
    modules = list(
      DEFAULT = list()
    ),

    # Stores global params for cross-module use
    globals = list(
      DEFAULT = list()
    ),

    # stores session IDs
    session_ids = c(),

    data = NULL
  ),

  public = list(

    synchronize = function(session = shiny::getDefaultReactiveDomain()){
      sid = get_session_id(session)

      if(sid %in% private$session_ids){

        # sync local variables
        for(session_id in names(private$modules)){
          if(session_id != sid){

            private$globals[[session_id]] = private$globals[[sid]]

            for(module_id in names(private$modules[[session_id]])){
              private$modules[[session_id]][[module_id]]$set_local_var(.list = private$modules[[sid]][[module_id]]$clone_params())
            }
          }
        }
      }
    },

    remove_session = function(session_id){
      private$session_ids = private$session_ids[private$session_ids != session_id]
      private$globals[[session_id]] = NULL
      private$modules[[session_id]] = NULL
    },

    reset = function(){
      for(m in private$modules){
        rm(m)
      }
      private$modules = list(
        DEFAULT = list()
      )
      private$globals = list(
        DEFAULT = list()
      )
      private$session_ids = c()
      private$data = NULL

    },

    has_data = function(){
      if(is.null(private$data)){
        return(FALSE)
      }else{
        return(TRUE)
      }
    },
    set_data = function(multi_electrodes){

      if(!'MultiElectrodes' %in% class(multi_electrodes)){
        if(is.null(multi_electrodes)){
          private$data <- NULL
        }else{
          logger('Pass a MultiElectrodes object please... :/', level = 'WARNING')
        }
      }else{
        private$data = multi_electrodes
      }
    },
    get_data = function(){
      private$data
    },

    add_module = function(module, session = shiny::getDefaultReactiveDomain()){
      sid = get_session_id(session)
      if(! sid %in% private$session_ids){
        private$session_ids = c(private$session_ids, sid)
      }
      if(!is.list(private$modules[[sid]])){
        private$modules[[sid]] = private$modules[['DEFAULT']]
      }
      private$modules[[sid]][[module$id]] = module
    },
    get_module = function(module_id, session = shiny::getDefaultReactiveDomain()){
      sid = get_session_id(session)
      private$modules[[sid]][[module_id]]
    },
    set_global_var = function(..., .list = NULL, session = shiny::getDefaultReactiveDomain()){
      sid = get_session_id(session)
      if(!is.list(private$globals[[sid]])){
        private$globals[[sid]] = private$globals[['DEFAULT']]
      }

      args = list(...)
      for(k in names(.list)){
        args[[k]] = .list[[k]]
      }
      for(n in names(args)){
        private$globals[[sid]][[n]] <- args[[n]]
        logger('[DEBUG] Global value - ', k, ' is set to: ', args[[k]])
      }
    },
    get_global_var = function(key, default = NULL, session = shiny::getDefaultReactiveDomain()){
      sid = get_session_id(session)
      val <- private$globals[[sid]][[key]]
      if(is.null(val)){
        val <- default
      }
      return(val)
    }
  )
)

#' Singleton of internal class DataRepository
#' @description
#' \code{data_repository} is a singleton instance creating an environment to store electrode data and modules.
#' Usually you won't be able to use this object directly
#' @inherit DataRepository
#' @usage
#' data_repository$set_data(multi_electrodes)
#' data_repository$get_data()
#' data_repository$set_global_var(key = value)
#' data_repository$get_global_var(key, default = NULL)
#' data_repository$reset()
#' @seealso \code{\link{DataRepository}}
#' @export
data_repository <- DataRepository$new()


#' @export
ModuleEnv <- R6::R6Class(
  'ModuleEnv',
  parent_env = data_repository,

  cloneable = TRUE,
  portable = FALSE,  # important, or your code will be convert to expressions

  private = list(

    # Stores global cache, if params changes, this will be cleared
    cache = list(),

    params = list()
  ),

  public = list(
    id = NULL,
    label = "",
    category = '',
    source_path = '',
    is_univariate = FALSE,
    suma_enabled = FALSE,
    runtime_env = NULL,

    results = list(),
    validate = NULL,
    activated = FALSE,
    packages = c(),
    info = NULL,

    finalize = function() {
      logger(self$id, ' is removed')
    },

    initialize = function(
      module_id, label, category, source_path, packages = '', suma_enabled = FALSE,
      is_univariate = TRUE, session = shiny::getDefaultReactiveDomain(), ...
    ){
      # Check packages
      for(p in packages){
        if(stringr::str_trim(p) != '' && !requireNamespace(p)){
          logger('Package ', p, ' NOT installed. Install it now!')
          install.packages(p, repos = "https://cloud.r-project.org/")
        }
      }

      self$packages = packages

      self$id = module_id
      self$label = label
      self$category = category
      self$source_path = source_path
      self$is_univariate = is_univariate
      self$runtime_env = new.env(parent = self)
      self$suma_enabled = suma_enabled
      self$info = list(...)

      # load script
      self$load()

      # register, link to a data_repository
      data_repository$add_module(self, session = session)
    },

    load = function(){
      with(self$runtime_env, {
        print(self$source_path)
        source(system.file('default.R', package = 'rave'), local = T)
        source(self$source_path, local = T)
      })
    },

    reload = function(){
      self$load()
      private$cache = list()
      self$activated = FALSE
    },

    eval = function(input, reactive_input){
      input <- shiny::isolate(input)
      params <- sapply(self$runtime_env$SHINY_INPUT, function(comp){
        id <- comp$id
        re <- list(input[[id]])
        names(re) <- id
        re
      })
      self$validate <- tryCatch({self$runtime_env$SHINY_VALIDATE(params)}, error = function(e){return(NULL)})

      if(length(self$validate) > 0){
        return(NULL)
      }
      self$results <- self$runtime_env$SHINY_EXECUTE(params, reactive_input = reactive_input)

      invisible(self$results)
    },

    get_SUMA = function(input, electrodes = NULL){
      # modified at 2017-07-17: using parallel

      subject = prophet$get_subject(prophet$last_subject)
      .data_env = data_repository$get_data()
      on.exit({
        self$runtime_env$data_env = .data_env
      })


      loaded_electrodes = .data_env$electrodes
      params <- shiny::isolate(input)
      params[['.IS_SUMA']] <- TRUE


      if(is.null(electrodes)){
        electrodes = subject$valid_electrodes
      }else{
        electrodes = electrodes[electrodes %in% subject$valid_electrodes]
      }


      # schedule
      if(self$is_univariate){
        nc <- as.numeric(rave_opts$get_options('suma_parallel_cores'))
        nr <- ceiling(length(electrodes) / nc)
        esm <- matrix(rep(NA, nc*nr), nrow = nc)
        esm[1:length(electrodes)] <- electrodes

        # make clusters
        # cl = parallel::makeForkCluster(nc)
        # alply = function(...){parallel::clusterApply(cl, ...)}
        alply = plyr::alply
        alply = function(X, M, FUN){
          workers = c()
          if(M == 2){
            X = t(X)
          }
          for(i in 1:nrow(X)){
            x = X[i, ]
            w = Worker$new()
            workers = c(workers, w)
            w$run(FUN(x))
          }

          m = Manager$new(workers, auto_process = FALSE, process = function(){
            lapply(1:length(workers), function(i){workers[[i]]$result})
          })
          print(m)
          while(m$check() != 'resolved'){
            print(m$finished)
            Sys.sleep(0.5)
          }
          return(m$process())
        }

        alply(esm, 1, function(current_electrodes){
          current_electrodes = current_electrodes[!is.na(current_electrodes)]
          if(length(current_electrodes) == 0){
            return(NULL)
          }
          elec_param_name = self$runtime_env$UNIVARIATE_MODE

          SUMA_RESULT = data.frame()
          tmp_data_envir = MultiElectrodes$new(subject)
          for(e in current_electrodes){
            params[[elec_param_name]] <- e

            if(e %in% loaded_electrodes){
              self$runtime_env$data_env = .data_env
              res <- self$runtime_env$SHINY_EXECUTE(params)
            }else{
              tmp_data_envir$load(electrodes = e, async = F)
              tmp_data_envir$bind_electrodes(electrodes = e, debug = T)
              self$runtime_env$data_env = tmp_data_envir
              res <- self$runtime_env$SHINY_EXECUTE(params, use_cache = F)
            }


            value = res[['SUMA_RESULT']]

            new_row = data.frame(
              electrode = e
            )
            dim(value) = c(1, length(value))
            new_row = cbind(new_row, as.data.frame(value))
            SUMA_RESULT = rbind(SUMA_RESULT, new_row)
          }

          return(SUMA_RESULT)
        }) ->
          results

        SUMA_RESULT = data.frame()
        for(i in 1:length(results)){
          SUMA_RESULT = rbind(SUMA_RESULT, results[[i]])
        }

        SUMA_RESULT = SUMA_RESULT[order(SUMA_RESULT$electrode), ]
      }


      if(exists('cl', envir = environment())){
        parallel::stopCluster(cl)
      }
      self$runtime_env$data_env = .data_env
      return(SUMA_RESULT)
    },
    get_SUMA_old = function(input, electrodes = NULL){

      if(self$is_univariate){
        loaded_electrodes <- data_repository$get_data()$electrodes
        if(!is.null(electrodes) && length(electrodes[!electrodes %in% loaded_electrodes] > 0)){
          # load new electrodes
          d = data_repository$get_data()
          d$load(electrodes, async = F)
          d$bind_electrodes(electrodes)
          data_repository$set_data(d)

          loaded_electrodes = electrodes
        }
        params <- shiny::isolate(input)
        params[['.IS_SUMA']] <- TRUE
        nc <- future::availableCores() - 1
        nr <- ceiling(length(electrodes) / nc)
        esm <- matrix(rep(NA, nc*nr), nrow = nc)
        esm[1:length(loaded_electrodes)] <- loaded_electrodes
        workers <- c()

        for(i in 1:nc){
          es <- esm[i, ]
          es <- es[!is.na(es)]
          if(length(es) > 0){
            worker <- Worker$new()
            workers <- c(workers, worker)
            worker$run({
              lapply(es, function(e){
                params[[self$runtime_env$UNIVARIATE_MODE]] <- e
                res <- self$runtime_env$SHINY_EXECUTE(params)
                list(
                  electrode = e,
                  value = res[['SUMA_RESULT']])
              })
            })
            prophet$observer$push(worker)
          }
        }
        manager <- Manager$new(workers, process = function(){

          plyr::ldply(workers, function(w){
            plyr::ldply(w$result, as.data.frame)
          }) ->
            suma_table
          return(suma_table)

          # This ugly code need to be migrated
          # plyr::ldply(workers, function(w){
          #   plyr::ldply(w$result, function(x){
          #     e <- x$electrode
          #     val <- x$value
          #     data.frame(
          #       node = (e - 1) * 42 + 0:41,
          #       value = rep(val, 42)
          #     )
          #   })
          # }) ->
          #   suma_table
          #
          # if(flush){
          #   suma$push_to_suma(value = suma_table$value)
          # }
          #
          #
          # return(suma_table)
        })
        prophet$observer$push(manager)

        return(manager)
      }
    },

    set_cache = function(name, val, params){
      private$cache[[name]] <- list(
        params = params,
        value = val
      )
    },
    get_cache = function(name){
      private$cache[[name]]  # remember to compare params by yourself
    },
    set_local_var = function(..., .list){
      args = shiny::isolate(list(...))
      for(k in names(.list)){
        args[[k]] = shiny::isolate(.list[[k]])
      }

      for(n in names(args)){
        private$params[[n]] <- args[[n]]
        logger('[DEBUG] Local value - ', n, ' is set to: ', args[[n]])
      }
    },
    clone_params = function(){
      return(private$params)
    },
    get_local_var = function(key, default = NULL){
      val <- shiny::isolate(private$params[[key]])
      if(is.null(val)){
        val <- default
      }
      return(val)
    },
    shinirize = function(input, output, session, triggers){
      with(self$runtime_env, {
        # TODO: change variables in default modules
        data_env = get_data()
        # frequencies = data_env$subject$frequencies
        # time = data_env$subject$time
        # trials = data_env$subject$trials
        # content = data_env$data
        #
        # cumsum_by_trial = data_env$cumsum
        #
        # subject = data_env$subject$id
        # electrode = data_env$electrodes
      })

      logger('Shinirize Module - ', self$id)
      input_inst <- self$runtime_env$SHINY_INPUT
      output_inst <- self$runtime_env$SHINY_OUTPUT

      lapply(input_inst, function(comp){
        comp$activate(input, triggers = triggers, module_id = self$id, session = session)
      })


      for(i in 1:length(output_inst)){
        panel <- output_inst[[i]]
        lapply(panel, function(comp){
          comp$activate(output, triggers = triggers, module_id = self$id)
        })

      }
      self$activated = TRUE
    }
  )
)




