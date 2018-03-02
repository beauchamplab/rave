# data.dir     "~/Documents/MATLAB/ECoG/neuralData/originalData/YAB/010"
# CAR.dir      "~/Documents/MATLAB/ECoG/neuralData/CARData/YAB/010"
# BP.dir       "~/Documents/MATLAB/ECoG/neuralData/BPData/YAB/010"
# Comp.dir     "~/Documents/MATLAB/ECoG/neuralData/CompData/YAB/010"
# Filt.dir     "~/Documents/MATLAB/ECoG/neuralData/FiltData/YAB/010"
# reRef.dir    "~/Documents/MATLAB/ECoG/neuralData/reRefData/YAB/010"
# Spec.dir     "~/Documents/MATLAB/ECoG/neuralData/SpecData/YAB/010"
# psych.dir    "~/Documents/MATLAB/ECoG/psychData/YAB/010"
# result.dir   "~/Documents/MATLAB/ECoG/Results/Congruency/YAB/010"



#### Master.m
pre_load_settings <- function(
  subject_code, setting_file = 'variables.txt'
){
  raw_data_dir = rave_opts$get_options('raw_data_dir')
  # Check data validity
  sf = file.path(raw_data_dir, 'neuralData', 'originalData', subject_code, setting_file)
  if(!file.exists(sf)){
    logger('Raw data directory does not contain "', setting_file, '"', level = 'ERROR')
  }

  # load settings
  source(sf, local = T)

  # complete settings
  blocks = settings[['blocks']]
  template = NULL
  block_nums = NULL
  for(name in names(blocks)){
    if(is.null(template)){
      template = blocks[[name]]
    }
    else{
      tmp = blocks[[name]]
      sel = (! names(template) %in% names(tmp))
      blocks[[name]] = c(blocks[[name]], template[sel])
    }

    block_nums = c(block_nums, blocks[[name]][['block.num']])
  }

  names(blocks) = block_nums
  settings[['blocks']] = blocks

  return(settings)
}



