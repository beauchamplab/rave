

AsyncEvaluator <- R6::R6Class(
  classname = 'AsyncEvaluator',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    cl = NULL,
    path_or_name = character(0)
  ),
  public = list(
    master_uuid = NULL,
    uuid = NULL,
    backend = character(0),
    queues = NULL,
    master = NULL,
    # 1. ready: run in any queue
    # 2. paused: only run the highest priority and only certain functions
    # 3. stopped: TODO (allow revoke?)
    # 4. high-priority: only run the highest priority 
    state = 'init',
    idx = 0,
    
    child_uuids = list(),
    
    priority_levels = factor(
      c('init', 'ready', 'high-priority', 'paused', 'stopped', 'running'), 
      levels = c('init', 'ready', 'high-priority', 'paused', 'stopped', 'running')),
    
    check_status = function(level_min = 'init', level_max = 'ready'){
      status_queue = self$queues[[5]];
      if( status_queue$count > 0 ){
        status = status_queue$pop(status_queue$count)
        for( s in status ){
          state = s$value$state
          idx = s$value$idx
          if( idx > 0 ){
            self$child_uuids[[idx]] %?<-% data.frame(
              idx = idx, 
              state = self$priority_levels[[1]]
            )
            self$child_uuids[[idx]]$state = state
          }
          
        }
        
      }
      
      level_min = which(self$priority_levels == level_min)
      level_max = which(self$priority_levels == level_max)
      
      valid_cls = lapply(self$child_uuids, function(tb){
        level = which(self$priority_levels == tb$state)
        if(level_min <= level && level_max >= level){
          return(tb$idx)
        }
        return(NULL)
      })
      
      unlist(dipsaus::drop_nulls(valid_cls))
    },
    
    report_state = function(){
      if(length(self$queues) >= 5 ){
        self$queues[[5]]$push(list(
          master_uuid = self$master_uuid,
          uuid = self$uuid,
          backend = self$backend,
          state = self$state,
          idx = self$idx
        ), message = self$uuid)
      }
    },
    
    fire_future = function(expr, which, quoted = FALSE, packages = NULL, ...){
      if( !quoted ){ expr = substitute(expr) }
      n_workers = self$n_workers
      if( n_workers == 0 ){
        return()
      }
      if( missing(which) ){
        which = seq_len(n_workers)
      }else{
        which = which[which %in% seq_len(n_workers)]
      }
      if(length(which) == 0){
        return()
      }
      
      packages = unique(c(packages, 'dipsaus', 'rave'))
      lapply(which, function(ii){
        future::run(
          future::MultiprocessFuture(expr, substitute = FALSE, local = TRUE, 
                                packages = packages, workers = private$cl[ii], 
                                lazy = FALSE, ...)
        )
      })
      
    },
    
    fire_direct = function(which, expr, vars = list(), quoted = FALSE){
      if(!quoted){
        expr = substitute(expr)
      }
      which = which[which %in% seq_along(private$cl)]
      if(length(which)){
        try({
          env = new.env()
          env$...rave_call = expr
          env$...rave_vars = vars
          parallel::clusterExport(private$cl[which], 
                                  varlist = c('...rave_call', '...rave_vars'), 
                                  envir = env)
          parallel::clusterCall(private$cl[which], function(){
            eval(...rave_call, 
                 envir = ...rave_vars,
                 enclos = new.env(parent = globalenv()))
          })
        })
      }
    },
    fire_quo = function(expr, env = parent.frame(), priority = c(1,2,3), quoted = FALSE){
      priority = priority[priority %in% c(1,2,3)][[1]]
      
      if( !quoted ){
        expr = rlang::enquo(expr)
      }
      
      self$queues[[priority]]$push(value = list(
        expr = expr,
        env = env
      ), message = 'dipsaus::eval_dirty')
    },
    
    shutdown = function( force = TRUE ){
      if( !self$is_master || self$n_workers == 0 ){
        # This is slave node
        return()
      }
      future::autoStopCluster(private$cl, debug = FALSE)
      if( force ){
        tryCatch({
          parallel::stopCluster(private$cl)
          force(private$cl)
        }, error = function(e){
          # Print ?
        }, finally = {
          private$cl = NULL
        })
        
      }else{
        # Let slave nodes run q(save='no')
        # Need to think about preventing the situation
        # when node is a shared node
        replicate(self$n_workers, {
          self$queues[[3]]$push(value = list(expr = quote({
            
            async_evaluator = rave::getCurrentAsyncEvaluator()
            if( !is.null(async_evaluator) ){
              async_evaluator$`@set_state`('paused')
              # quit
              if( !async_evaluator$is_master ){
                do.call('base::q', list(save = 'no'))
              }
            }
            
            
          })), message = 'base::eval')
        })
        
      }
    },
    scale_up = function( worker, ... ){
      if( !self$is_master ){
        return()
      }
      curernt_workers = self$n_workers
      more_workers = as.integer(worker) - curernt_workers
      if( more_workers > 0 ){
        dipsaus::cat2('Initializing ', more_workers, ' workers. Total workers: ', curernt_workers)
        new_cl = future::makeClusterPSOCK(worker = more_workers, ...)
        if( curernt_workers == 0 ){
          private$cl = new_cl
        }else{
          private$cl = c(private$cl, new_cl)
        }
        self$`@update_children`(seq_len(more_workers) + curernt_workers)
        
      }
    },
    
    # FIXME: Shutdown before switch
    init_backend = function(backend = c('qs', 'rds', 'redis'), 
                              path_or_name, quiet = FALSE){
      if(!self$is_master || length(self$backend)){ return() }
      backend = match.arg(backend)
      
      create_q = function(sub = '0'){
        switch (
          backend,
          'qs' = dipsaus::qs_queue(path = file.path(path_or_name, 'qs', sub)),
          'rds' = dipsaus::rds_queue(path = file.path(path_or_name, 'rds', sub)),
          'redis' = dipsaus::redis_queue(name = paste0(path_or_name, '-', sub))
        )
      }
      priorities = seq_len(5)
      new_queues = lapply(as.character( priorities ), create_q)
      
      self$backend = backend
      self$queues = new_queues
      private$path_or_name = path_or_name
      
      self$`@update_children`()
      
    },
    `@update_children` = function(which){
      if(!self$n_workers){
        return()
      }
      if(missing(which)){
        which = seq_len(self$n_workers)
      }else{
        which = which[which %in% seq_len(self$n_workers)]
      }
      
      if( self$is_master ){
        
        lapply(which, function(ii){
          self$fire_direct(
            which = ii, {
              rave = asNamespace('rave')
              dipsaus = asNamespace('dipsaus')
              
              async_evaluator = rave$getCurrentAsyncEvaluator()
              
              if(is.null(async_evaluator)){
                # Create new one
                rave$activate_async_evaluator(backend = backend, path_or_name = path_or_name, idx = cluster_idx)
                async_evaluator = rave$getCurrentAsyncEvaluator()
                current_state = 'ready'
              }else{
                current_state = async_evaluator$state
                async_evaluator$`@set_state`('paused')
                # FIXME: Clean/destroy previous queue
                create_q = function(sub = '0'){
                  switch (
                    backend,
                    'qs' = dipsaus$qs_queue(path = file.path(path_or_name, 'qs', sub)),
                    'rds' = dipsaus$rds_queue(path = file.path(path_or_name, 'rds', sub)),
                    'redis' = dipsaus$redis_queue(name = paste0(path_or_name, '-', sub))
                  )
                }
                
                
                async_evaluator$`@set_queue`(
                  backend,
                  lapply(seq_len(4), function(ii){
                    create_q(as.character(ii))
                  })
                )
                
              }
              
              
              # Change slave nodes
              async_evaluator$`@set_uuid`( uuid )
              if( current_state == 'init' ){
                async_evaluator$`@set_state`('ready')
              }else{
                async_evaluator$`@set_state`(current_state)
              }
              
              async_evaluator$report_state()
              
            }, list(
              uuid = self$master_uuid, backend = self$backend, 
              path_or_name = private$path_or_name,
              cluster_idx = ii
            ))
        })
        
      }
      
      
    },
    initialize = function(backend = c('qs', 'rds', 'redis'), 
                          workers = rave_options('max_worker'), 
                          path_or_name, idx = 0){
      
      self$master_uuid = session_uuid(attributes = FALSE)
      self$uuid = dipsaus:::rand_string()
      
      backend = match.arg(backend)
      self$init_backend(backend, path_or_name)
      self$.later_loop = later::create_loop(autorun = FALSE)
      self$idx = idx
      if( idx != 0 ){
        self$is_master = FALSE
      }
      
      # Register finalizer:
      rave:::RaveFinalizer$new(function(){
        cat2('Finalizing - closing clusters')
        later::destroy_loop( self$.later_loop )
        self$shutdown( force = TRUE )
      })
      
    },
    
    # Listen to queue
    listen = function(interval = 0.5, force = FALSE){
      # Cannot run in slave nodes, I only schedule in main session
      if( !self$is_master || (!force && self$state == 'stopped') ){ return() }
      
      which = self$check_status()
      
      # Check queue
      
      self$fire_direct(
        which, {
          rave = asNamespace('rave')
          rave$async_evaluator_check_queue()
        }
      )
      
      later::later(self$listen, interval)
      
    },
    check_queue = function(){
      if( self$state == 'stopped' ){
        return(FALSE)
      }
      
      order = list(ready = c(3,2,1),
                   paused = 3, `high-priority` = 3)[[ self$state ]]
      
      for( ii in order ){
        item = tryCatch(self$queues[[ii]]$pop(), error = function(e){})
        if(length(item)){
          item = item[[1]]
          # Need to handle
          valid = TRUE
          if( self$state == 'paused' ){
            valid = item$message %in% c('base::q')
          }
          if( valid ){
            current_status = self$state
            tryCatch({
              self$`@set_state`('running')
              # can evaluate
              re = do.call(eval(str2lang(item$message)), item$value, 
                           envir = new.env(parent = globalenv()))
              
              self$queues[[4]]$push(list(
                original_message = item$message,
                value = re,
                state = 'success',
                node_uuid = session_uuid()
              ), message = self$master_uuid)
              re
            }, error = function(e){
              self$queues[[4]]$push(list(
                original_message = item$message,
                error = e,
                state = 'error',
                node_uuid = session_uuid()
              ), message = self$master_uuid)
            }, finally = {
              self$`@set_state`(current_status)
            })
            
            break()
            
          }
        }
      }
      
    },
    
    `@set_queue` = function(bd, qus){
      self$backend = bd
      self$queues = qus
    },
    `@set_state` = function(
      state =  c('stopped', 'ready', 'paused', 'high-priority', 'running', 'init')
    ){

      tryCatch({
        self$state = match.arg(state)[[1]]
        self$report_state()
      }, error = function(e){
        
      })
    },
    `@set_uuid` = function(master_uuid){
      self$master_uuid = master_uuid
    },
    .later_loop = NULL
    
  ),
  active = list(
    n_workers = function(){
      length(private$cl)
    },
    is_master = function(v){
      if(!missing(v)){
        self$master = isTRUE(v)
      }
      if( is.logical(self$master) ){
        return( self$master )
      }
      
      return( self$master_uuid == session_uuid() )
    }
  )
)

session_uuid <- local({
  uuids <- list()
  
  function (pid = Sys.getpid(), attributes = TRUE) {
    pidstr <- as.character(pid)
    uuid <- uuids[[pidstr]]
    if (!is.null(uuid)) {
      if (!attributes) 
        attr(uuid, "source") <- NULL
      return(uuid)
    }
    info <- Sys.info()
    host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"))
    host <- host[nzchar(host)]
    host <- if (length(host) == 0L) 
      info[["nodename"]]
    else host[1L]
    oseed <- .GlobalEnv$.Random.seed
    on.exit({
      if (is.null(oseed)) {
        rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      } else {
        .GlobalEnv$.Random.seed <- oseed
      }
    })
    info <- list(host = host, info = info, pid = pid, time = Sys.time(), 
                 random = sample(.Machine$integer.max, size = 1L, replace = FALSE))
    uuid <- digest::digest(info)
    attr(uuid, "source") <- info
    uuids[[pidstr]] <<- uuid
    if (!attributes) 
      attr(uuid, "source") <- NULL
    uuid
  }
})


.async_evaluator <- local({
  async_evaluator <- NULL
  initialized = FALSE
  
  initialize <- function(backend = 'redis', 
                         path_or_name = 'rave',
                         idx = 0){
    if(is.null(async_evaluator)){
      async_evaluator <<- AsyncEvaluator$new(backend = backend, 
                                             path_or_name = path_or_name, 
                                             workers = 0, idx = idx)
      initialized <<- TRUE
    }
  }
  
  getCurrentAsyncEvaluator <- function(){
    async_evaluator
  }
  
  check_queue <- function(){
    if( initialized ){
      async_evaluator$check_queue()
    }
  }
  
  list(
    initialize = initialize,
    getCurrentAsyncEvaluator = getCurrentAsyncEvaluator,
    check_queue = check_queue
  )
  
})

#' @export
getCurrentAsyncEvaluator <- .async_evaluator$getCurrentAsyncEvaluator


#' @export
activate_async_evaluator <- .async_evaluator$initialize

#' @export
async_evaluator_check_queue <- .async_evaluator$check_queue

.onUnload <- function(libpath){
  async_evaluator = getCurrentAsyncEvaluator()
  if(!is.null(async_evaluator)){
    tryCatch({
      async_evaluator$state = 'stopped'
      async_evaluator$shutdown( force = TRUE )
    }, error = function(e){
      
    })
  }
}
