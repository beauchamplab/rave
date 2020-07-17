# Scripts to handle settings
default_opts <- function(...){
  # --------------------- Hardware information ---------------------
  
  # RAM
  max_mem = dipsaus::get_ram() / 1024^3
  
  # Harddrive speed
  drive_speed = c(0.02, 0.05)
  disable_startup_speed_check = FALSE
  
  # number of cores to be used
  max_worker = future::availableCores() - 1
  
  # Not used, but maybe useful in the future
  server_time_zone = 'America/Chicago'
  
  # --------------------- RAVE settings ---------------------
  
  # Repositories
  raw_data_dir = '~/rave_data/raw_dir/'
  data_dir = '~/rave_data/data_dir/'
  module_root_dir = '~/rave_modules/'
  module_lookup_file = '~/rave_modules/modules.csv'
  
  # Shiny settings
  # input update firing speed (20ms default)
  delay_input = 20
  # init app test_mode
  test_mode = FALSE
  
  # Cache
  # Use fst package to cache power phase volt after wavelet and reference?
  # If running in the local machine, we don't need it because h5 is fast enough
  # and h5 with mpi is even faster
  # However, if data is stored at data server, fst is way faster than h5 (single threaded with forked)
  # Also on windows, fst should be faster than hdf5 in general
  fast_cache = TRUE
  
  
  # --------------------- SUMA Setttings ---------------------
  # this section is not really used anymore as threeBrain can do most of the work
  # We still keep it in case 
  suma_nodes_per_electrodes = 42L
  suma_lib = c('DYLD_LIBRARY_PATH=/opt/X11/lib/flat_namespace', "DYLD_FALLBACK_LIBRARY_PATH=/Applications/AFNI")
  suma_path = '/Applications/AFNI'
  suma_spec_file = 'test.spec'
  
  
  # --------------------- Paths ---------------------
  # Matlab path not used right now
  matlab_path = '/Applications/MATLAB_R2016b.app/bin'
  
  # Python path, soft deprecated
  py2_path = Sys.which('python')
  py3_path = Sys.which('python')
  py_virtualenv = ''
  
  # --------------------- Export options ---------------------
  image_width = 1280L
  image_height = 768L
  
  
  
  
  
  # Deprecated
  # debug = FALSE
  # session_based_datarepo = FALSE
  # module_export = './export'
  # content_regex = 'e([0-9]+)[^0-9]*'
  # content_format = 'mat'
  # export_path = './export'
  # temp_dir = 'temp'
  # suma_monitor_dir = 'temp/monitor'
  # suma_export_dir = 'suma'  # [SUBJECT]/suma, must be relative path, dirname, stores spec files
  # suma_gifti_name_regex = 'electrode_[a-zA-Z0-9]+.gii'
  # logger_enabled = TRUE
  # logger_level = 'DEBUG'
  # suma_to_niml = '~/abin/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -prefix %s'
  # suma_send_niml = '~/abin/DriveSuma -com surf_cont -load_dset %s'  # Not used because x11 on mac sucks
  # suma_parallel_cores = 2L
  # dyld_library_path = '/opt/X11/lib/flat_namespace'
  # big_object_size = 500000
  
  list2env(list(...), envir = environment())
  as.list(environment())
}


# Internal R6 class for rave-options
Options <- R6::R6Class(
  "Options",
  private = list(
    opts = list()
  ),
  public = list(
    initialize = function(conf_path = '~/.rave.yaml', save_default = F){
      self$reset()
      loaded = self$load_settings( conf_path )
      
      if( !loaded && save_default ){
        self$save_settings(path = '~/.rave.yaml')
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
      private$opts = default_opts()
    },
    load_settings = function(conf_path){
      if(length(conf_path) != 1 || is.na(conf_path) || !is.character(conf_path) || !file.exists(conf_path)){
        return(FALSE)
      }
      v = as.list(raveio::load_yaml(conf_path))
      
      do.call(self$set_options, args = v)
      self$set_options(conf_path = conf_path)
      return(TRUE)
    },
    save_settings = function(path = '~/.rave.yaml'){
      dname = dirname(path)
      if(!dir.exists(dname)){
        dir.create(dname, showWarnings = FALSE, recursive = TRUE)
      }
      raveio::save_yaml(private$opts, file = path, fileEncoding = 'UTF-8')
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
#' @param host IP address of host
#' @param port Port number
#' @export
rave_options <- function(..., .save = TRUE, launch_gui = TRUE,
                         host = '127.0.0.1', port = NULL){
  if(!exists('rave_opts', envir = ..setup_env, inherits = FALSE)){
    ..setup_env$rave_opts <- Options$new(conf_path = '~/.rave.yaml', 
                                         save_default = TRUE)
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
      return(rave_options_gui(host = host, port = port))
    }
  }
  
  return(re)
}



ugly_sample <- function(x, size = length(x), replace = FALSE, ...){
  oldseed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- oldseed)
  sample(x, size = size, replace = replace, ...)
}


rave_setup_workers <- function(n_workers = 0){
  
  # Force n_workers to be rave_option
  if( n_workers <= 0 ){
    n_workers = max(rave_options('max_worker'), 1)
  }
  
  dipsaus::make_forked_clusters(workers = n_workers)
  
  catgl('Number of workers switched to - {n_workers}')
  
}





