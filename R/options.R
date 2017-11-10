# Scripts to handle settings

opt <- list(
  debug = 'FALSE',
  data_dir = './data',
  big_object_size = '500000',
  server_time_zone = 'America/Chicago',
  module_export = './export',
  content_regex = 'e([0-9]+)[^0-9]*',
  content_format = 'mat',
  module_lookup_file = './modules.csv',
  delay_input = '200',
  max_worker = '20',
  export_path = './export',
  temp_dir = 'temp',
  suma_monitor_dir = 'temp/monitor',
  suma_export_dir = 'suma',  # [SUBJECT]/suma, must be relative path, dirname, stores spec files
  suma_to_niml = '~/abin/ConvertDset -o_niml -input %s -i_1D -node_index_1D %s -prefix %s',
  suma_send_niml = '~/abin/DriveSuma -com surf_cont -load_dset %s',  # Not used because x11 on mac sucks
  suma_nodes_per_electrodes = '42',
  suma_parallel_cores = '2',
  suma_gifti_name_regex = 'electrode_[a-zA-Z0-9]+.gii',
  dyld_library_path = '/opt/X11/lib/flat_namespace',
  suma_path = '~/abin',
  suma_spec_file = 'test.spec',
  unbuffer_path = '/usr/local/bin',  # if you want to use expect package, which has no buffer, otherwise set to ""

  matlab_path = '/Applications/MATLAB_R2016b.app/bin',

  use_rhdf5 = 'FALSE',
  batch_bytes = '5000000000'
)

# TODO We can use this to make sure we are attaching the dataset to the correct surface, asuming we know / can set the surface name of the electrodes
# -load_dset DSET: Load a dataset
# ! NOTE: When using -load_dset you can follow it
# with -surf_label in order to attach
# the dataset to a particular target surface.


Options <- R6::R6Class(
  "Options",
  private = list(
    opts = list()
  ),
  public = list(
    initialize = function(conf_path = system.file('settings.conf', package = 'rave')){
      private$opts = opt
      if(length(conf_path) != 1 || is.na(conf_path) || !is.character(conf_path) || !file.exists(conf_path)){
        private$opts = opt
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
      return(self$get_options(names(o)))
    },
    reset = function(){
      private$opts = opt
    },
    load_settings = function(
      conf_path = system.file('settings.conf', package = 'rave')
    ){
      if(length(conf_path) != 1 || is.na(conf_path) || !is.character(conf_path) || !file.exists(conf_path)){
        return()
      }
      regex <- '^([a-zA-Z0-9_\\.]+)[ ]*:=(.*)$'
      cf <- file(conf_path, 'r')
      lines = readLines(cf)
      close(cf)

      # Parse settings
      settings <- lines[stringr::str_detect(lines, regex)]
      settings <- stringr::str_match(settings, regex)[,-1]
      v <- as.list(stringr::str_trim(settings[,2]))
      names(v) <- settings[,1]
      do.call(self$set_options, args = v)
      self$set_options(conf_path = conf_path)
    },
    save_settings = function(path = '~/.rave.conf'){
      if(!file.exists(path)){
        dname = dirname(path)
        if(!file.exists(dname)){
          dir.create(dname)
        }
      }
      opt = private$opts
      s = c()
      for(k in names(opt)){
        s = c(s, sprintf('%s := %s', k, opt[[k]]))
      }
      writeLines(s, con = path)
    }
  )
)

#' @export
rave_opts <- Options$new(conf_path = '~/.rave.conf')




