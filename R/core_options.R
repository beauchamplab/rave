# Scripts to handle settings

opt <- list(
  debug = FALSE,
  raw_data_dir = '../Researches/rave/adhoc/Dipterix/Data/ECoG/neuralData/originalData/',
  data_dir = '../rafe/hdf5_data/',
  big_object_size = 500000,
  server_time_zone = 'America/Chicago',
  module_export = './export',
  content_regex = 'e([0-9]+)[^0-9]*',
  content_format = 'mat',
  module_lookup_file = '~/rave_modules/modules.csv',
  delay_input = 200,
  max_worker = 3L,
  export_path = './export',
  temp_dir = 'temp',
  suma_monitor_dir = 'temp/monitor',
  suma_export_dir = 'suma',  # [SUBJECT]/suma, must be relative path, dirname, stores spec files
  suma_to_niml = '~/abin/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -prefix %s',
  suma_send_niml = '~/abin/DriveSuma -com surf_cont -load_dset %s',  # Not used because x11 on mac sucks
  suma_nodes_per_electrodes = 42L,
  suma_parallel_cores = 2L,
  suma_gifti_name_regex = 'electrode_[a-zA-Z0-9]+.gii',
  dyld_library_path = '/opt/X11/lib/flat_namespace',
  suma_path = '~/abin',
  suma_spec_file = 'test.spec',
  unbuffer_path = '/usr/local/bin',  # if you want to use expect package, which has no buffer, otherwise set to ""

  matlab_path = '/Applications/MATLAB_R2016b.app/bin',

  # images
  image_width = 1280L,
  image_height = 768L,

  logger_enabled = TRUE,
  logger_level = 'DEBUG',
  crayon_enabled = TRUE,
  session_based_datarepo = FALSE,
  test_mode = TRUE
)

# TODO We can use this to make sure we are attaching the dataset to the correct surface, asuming we know / can set the surface name of the electrodes
# -load_dset DSET: Load a dataset
# ! NOTE: When using -load_dset you can follow it
# with -surf_label in order to attach
# the dataset to a particular target surface.

#' @import yaml
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
      if(!file.exists(path)){
        dname = dirname(path)
        if(!file.exists(dname)){
          dir.create(dname)
        }
        mw = self$get_options('max_worker')
        if(length(mw) > 0 && mw == 3L && 'parallel' %in% installed.packages()[,1]){
          self$set_options(max_worker = parallel::detectCores() - 1)
        }
      }
      opt = private$opts
      yaml::write_yaml(opt, file = path, fileEncoding = 'UTF-8')
    }
  )
)

#' @export
rave_opts <- Options$new(conf_path = '~/.rave.yaml', save_default = T)


..setup_env = new.env(parent = emptyenv())
..setup_env$setup_func = list()
#' @export
rave_setup <- function(func = NULL){
  if(!is.function(func)){
    if(rave_opts$get_options('test_mode')){
      # future::plan(future::sequential)
      future::plan(future::multiprocess, workers = rave_opts$get_options('max_worker'))
    }else{
      future::plan(future::multiprocess, workers = rave_opts$get_options('max_worker'))
    }
  }else{
    ..setup_env$setup_func[[length(..setup_env$setup_func) + 1]] = func
  }
  if(length(..setup_env$setup_func)){
    lapply(..setup_env$setup_func, function(f){
      if(is.function(f)){
        f()
      }
    })
  }
}

