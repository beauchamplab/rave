# Scripts to handle settings

opt <- list(
  # Max memory in GB. R cannot auto detect RAM cap in linux and macos so I set this value
  max_mem = 256,

  # hard drive speed: seconds per MB (sec/MB)
  drive_speed = c(0.02, 0.05),

  # disable startup disk check (speed test)
  disable_startup_speed_check = FALSE,

  # check threejsr, rutabaga updates
  check_updates_onstartup = T,

  # data repositories
  raw_data_dir = '~/rave_data/raw_dir/',
  data_dir = '~/rave_data/data_dir/',
  module_root_dir = '~/rave_modules/',
  module_lookup_file = '~/rave_modules/modules.csv',

  # Enable logger
  logger_enabled = TRUE,
  logger_level = 'DEBUG',

  # color logger
  crayon_enabled = TRUE,


  # init app test_mode
  test_mode = FALSE,

  # Not really used, might be depricated in the future. currently used to trim environment
  # and remove big objects
  big_object_size = 500000,

  # Not used, but maybe useful in the future
  server_time_zone = 'America/Chicago',


  # input update firing speed (20ms default)
  delay_input = 20,

  # number of cores to be used
  max_worker = 8L,

  ## SUMA

  suma_to_niml = '~/abin/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -prefix %s',
  suma_send_niml = '~/abin/DriveSuma -com surf_cont -load_dset %s',  # Not used because x11 on mac sucks
  suma_nodes_per_electrodes = 42L,
  suma_parallel_cores = 2L,
  # dyld_library_path = '/opt/X11/lib/flat_namespace',
  suma_lib = c('DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace', "DYLD_FALLBACK_LIBRARY_PATH=/Applications/AFNI"),
  suma_path = '/Applications/AFNI',
  suma_spec_file = 'test.spec',

  matlab_path = '/Applications/MATLAB_R2016b.app/bin',
  py2_path = Sys.which('python'),
  py3_path = Sys.which('python'),
  py_virtualenv = '',

  # images
  image_width = 1280L,
  image_height = 768L,

  # Cache
  # Use fst package to cache power phase volt after wavelet and reference?
  # If running in the local machine, we don't need it because h5 is fast enough
  # and h5 with mpi is even faster
  # However, if data is stored at data server, fst is way faster than h5 (single threaded with forked)
  # Also on windows, fst should be faster than hdf5 in general
  fast_cache = TRUE,






  # Deprecated
  debug = FALSE,
  session_based_datarepo = FALSE,
  module_export = './export',
  content_regex = 'e([0-9]+)[^0-9]*',
  content_format = 'mat',
  export_path = './export',
  temp_dir = 'temp',
  suma_monitor_dir = 'temp/monitor',
  suma_export_dir = 'suma',  # [SUBJECT]/suma, must be relative path, dirname, stores spec files
  suma_gifti_name_regex = 'electrode_[a-zA-Z0-9]+.gii'
)

# TODO We can use this to make sure we are attaching the dataset to the correct surface, asuming we know / can set the surface name of the electrodes
# -load_dset DSET: Load a dataset
# ! NOTE: When using -load_dset you can follow it
# with -surf_label in order to attach
# the dataset to a particular target surface.

#' Internal R6 class for rave-options
Options <- R6::R6Class(
  "Options",
  private = list(
    opts = list()
  ),
  public = list(
    initialize = function(conf_path = '~/.rave.yaml', save_default = F){

      private$opts = opt
      if(length(conf_path) != 1 || is.na(conf_path) || !is.character(conf_path) || !file.exists(conf_path)){
        private$opts = opt
        if(save_default){
          self$save_settings(path = '~/.rave.yaml')
        }
      }else{
        self$load_settings(conf_path = conf_path)
      }
    },
    get_options = function(...){
      re <- private$opts[...]
      if(length(re) == 1){
        re = unlist(re)
      }
      return(re)
    },
    set_options = function(...){
      o = list(...)
      for(n in names(o)){
        private$opts[[n]] = o[[n]]
      }
      return(invisible(self$get_options(names(o))))
    },
    reset = function(){
      private$opts = opt
    },
    load_settings = function(
      conf_path = system.file('settings.yaml', package = 'rave')
    ){
      if(length(conf_path) != 1 || is.na(conf_path) || !is.character(conf_path) || !file.exists(conf_path)){
        return()
      }
      v = yaml::read_yaml(conf_path)

      do.call(self$set_options, args = v)
      self$set_options(conf_path = conf_path)
    },
    save_settings = function(path = '~/.rave.yaml'){
      dname = dirname(path)
      if(!dir.exists(dname)){
        dir.create(dname, showWarnings = FALSE, recursive = TRUE)
      }

      if(!file.exists(path)){
        self$set_options(max_worker = future::availableCores() - 1)
        try({
          ram = mem_limit()$total / 1024^3
          self$set_options(max_mem = ram)
        })
      }
      opt = private$opts
      yaml::write_yaml(opt, file = path, fileEncoding = 'UTF-8')
    }
  )
)

# export
# rave_opts <- Options$new(conf_path = '~/.rave.yaml', save_default = T)

#' Function to locally save options (deprecated)
#' @export
save_options <- function(){
  ..setup_env$rave_opts$save_settings()
}

#' Function to change rave-options
#' @param ... Key-Value option pairs
#' @param .save save to disk? ignored most of the time
#' @param launch_gui launch shiny app?
#' @export
rave_options <- function(..., .save = T, launch_gui = T){
  if(!exists('rave_opts', envir = ..setup_env, inherits = F)){
    ..setup_env$rave_opts <- Options$new(conf_path = '~/.rave.yaml', save_default = T)
  }
  args = list(...)
  if(length(args) && length(names(args))){
    # set options
    re = ..setup_env$rave_opts$set_options(...)

    if(.save){
      ..setup_env$rave_opts$save_settings()
    }
  }else{
    # get options
    re = ..setup_env$rave_opts$get_options(...)
    if(length(args) == 0 && launch_gui){
      # make a small shiny app to set options
      return(rave_options_gui())
    }
  }

  return(re)
}



ugly_sample <- function(x, size = length(x), replace = FALSE, ...){
  oldseed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- oldseed)
  sample(x, size = size, replace = replace, ...)
}


#' Default setup for rave, mainly create forked sub-processes
#' @param n_workers NULL by default, maximum number of clusters for parallel
#' @param ignore_error whether ignore the errors
#' @export
rave_setup_workers <- function(n_workers = NULL, ignore_error = TRUE){
  logger('Setting up workers')
  if(!length(n_workers)){
    n_workers = max(rave_options('max_worker'), 1)
  }else{
    n_workers = as.integer(n_workers)
    if(n_workers <= 0){
      if(ignore_error){
        n_workers = max(rave_options('max_worker'), 1)
      }else{
        stop('n_workers must be positive')
      }
    }

  }

  # as states in issue
  # https://github.com/HenrikBengtsson/future/issues/309
  # we decide to preserve our own clusters
  if(!exists('clusters', ..setup_env)){
    ..setup_env$clusters = future::makeClusterPSOCK(n_workers, autoStop = FALSE)
    future::plan(future::cluster, workers = ..setup_env$clusters)

  }else{

    nclusters = future::nbrOfWorkers(future::plan())

    # If need more clusters
    if(nclusters < n_workers){

      ranks = sapply(..setup_env$clusters, function(cl){
        tryCatch({
          # Never use print(cl). Because when cluster is runningm this will block the main session
          if(summary.connection(cl$con)$opened == 'opened'){
            cl$rank
          }
        }, error = function(e){
          0
        })
      })

      more_ranks = seq_len(n_workers)
      more_ranks = more_ranks[!more_ranks %in% ranks]

      cl = lapply(more_ranks, function(rank){
        port0 <- Sys.getenv("R_PARALLEL_PORT", "random")
        if (identical(port0, "random")) {
          port <- 11000:11999
        }
        else {
          port <- suppressWarnings(as.integer(port0))
          if (is.na(port)) {
            warning("Non-numeric value of environment variable 'R_PARALLEL_PORT' coerced to NA_integer_: ",
                    sQuote(port0))
            port <- 11000:11999
          }
        }
        if (length(port) > 1L) {
          port <- ugly_sample(port, size = 1L)
        }
        if (is.na(port) || port < 0L || port > 65535L) {
          stop("Invalid port: ", port)
        }
        future::makeNodePSOCK(port = port, rank = as.integer(rank))
      })

      cl = c(
        ..setup_env$clusters, cl
        # future::makeClusterPSOCK(n_workers - nclusters, autoStop = FALSE)
      )

      ..setup_env$clusters = future::as.cluster(cl)
      try({
        future::plan(future::cluster, workers = ..setup_env$clusters)
      }, silent = TRUE)
    }else if(nclusters > n_workers){
      future::autoStopCluster(..setup_env$clusters[(n_workers + 1) : nclusters])
    }
  }

  nclusters = future::nbrOfWorkers(future::plan())

  # check crayon
  if(!rave_options('crayon_enabled')){
    # if(exists('RStudio.Version') && is.function(RStudio.Version)){
    #   rsver = as.character(RStudio.Version()$version)
    #   if(utils::compareVersion('1.1', rsver)  > 0){
    #     warning("Please install newest version of RStudio")
    #     rave_options(crayon_enabled = FALSE, .save = F)
    #   }
    # }else{
    rave_options(crayon_enabled = FALSE, .save = F)
    # }
  }


  logger('Number of workers switched to - ', nclusters)

}





