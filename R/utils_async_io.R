future_assign_lapply <- function(
  x, varnames, expr, elname = 'el', nworkers = 2, envir = parent.frame(), substitute = TRUE, seed = NULL, globals = TRUE, assign.env = envir,
  force_assign = FALSE, check_interval = 1, ...
){
  # make a function
  expr = substitute(expr)
  subenvir = new.env(parent = envir)
  ...fun = function(){}
  environment(...fun) = subenvir
  body(...fun) = expr
  params = alist(el=)
  names(params) = elname
  formals(...fun) = params

  if(!isTRUE(globals)){
    if(isFALSE(globals)){
      globals = NULL
    }
    globals = c('...fun', paste0('...el_future_', varnames))
  }
  subenvir$`...x` = x
  subenvir$`...fun` = ...fun

  future_args <- list(envir = subenvir, lazy = TRUE, seed = seed, globals = globals, substitute = substitute, ...)
  args <- getOption("future.disposable", NULL)
  if (!is.null(args)) {
    for (name in names(args)) future_args[name] <- args[name]
    on.exit(options(future.disposable = NULL))
  }

  jobs = new.env()
  jobs$lists = list()
  jobs$callbacks = list()
  jobs$ii = 1
  jobs$funished = FALSE

  lapply(seq_along(x), function(ii){
    el_name = paste0('...el_future_', varnames[[ii]])
    future_name <- sprintf(".future_%s", varnames[[ii]])
    subenvir[[el_name]] = x[[ii]]

    expr = sprintf('quote({...fun(%s)})', el_name)
    expr = eval(parse(text = expr))
    future_args$expr = expr
    f = do.call(future::future, future_args, envir = subenvir)
    f$.gcenv <- NULL


    jobs$lists[[future_name]] = f

    assign_function = function(do_assign = FALSE){
      value <- tryCatch({
        future::value(f)
      }, error = function(e){
        NULL
      })

      jobs$lists[[future_name]] = NULL
      jobs$callbacks[[future_name]] = NULL
      if(exists(el_name, envir = subenvir, inherits = FALSE)){
        rm(list = el_name, envir = subenvir)
      }
      if(do_assign && (force_assign || !exists(varnames[[ii]], envir = assign.env, inherits = FALSE))){
        assign(varnames[[ii]], value = value, envir = assign.env)
      }
      value
    }

    jobs$callbacks[[future_name]] = function(){
      if(future::resolved(f)){
        assign_function(do_assign = TRUE)
      }
    }

    # add delayed assign
    delayedAssign(varnames[[ii]], local({assign_function(do_assign = FALSE)}), assign.env = assign.env)
  })

  future_names = sprintf(".future_%s", varnames)
  run = function(){
    lapply(jobs$callbacks, function(.f){.f()})

    # check states
    states = vapply(future_names, function(nm){
      f = jobs$lists[[nm]]
      if(is.null(f)){
        'assigned'
      }else{
        f$state
      }
    }, FUN.VALUE = 'created')

    # states can be created, running, failed, finished, assigned
    sel_running = states == 'running'

    sel_inited = states == 'created'
    available_workers = nworkers - sum(sel_running)
    if(available_workers > 0){
      if(sum(sel_inited)){
        sel_inited = which(sel_inited)
        sel_inited = sel_inited[1:min(length(sel_inited), available_workers)]
        for(ii in sel_inited){
          future::run(jobs$lists[[future_names[[ii]]]])
        }
      }
    }

    all_finished = all(!(sel_running | sel_inited))
    if(!all_finished){
      later::later(run, delay = check_interval)
    }else{
      subenvir[['...f']] = NULL
      jobs$funished = TRUE
    }
  }
  later::later(run, delay = 0)

  invisible(jobs)
}
