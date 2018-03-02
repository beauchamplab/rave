# Shiny app for pre-processing


### Load functions

.load_preprocess_func <- function(
  # Here are the packages needed to pre-process data
  packages = c('rave', 'tidyverse', 'rhdf5', 'grid', 'gridBase', 'signal'),

  # don't change it
  local = F
){
  # fixed usage !
  env = new.env()
  for(p in packages){
    require(p, character.only = T)
  }
  if(local){
    # used by the package DON'T CHANGE

    source = function(file, local = T,...){
      if(is.null(file) || file == ''){
        return(NULL)
      }
      args = list(...)

      env$.args = c(
        list(
          file = file,
          local = T
        ),
        args
      )
      with(env, {
        do.call(base::source, args = .args)
      })

    }
  }

  #########################################
  # Source your functions here

  source(system.file('preprocess/plot_signal.R', package = 'rave'))
  source(system.file('preprocess/pre_import_signal.R', package = 'rave'))
  source(system.file('preprocess/pre_load_settings.R', package = 'rave'))
  source(system.file('preprocess/pre_resample.R', package = 'rave'))
  source(system.file('preprocess/pre_notch_filter.R', package = 'rave'))
  source(system.file('preprocess/pre_wavelet.R', package = 'rave'))
  source(system.file('preprocess/pre_units.R', package = 'rave'))

  # End
  #########################################

  return(env)
}



#' If you want to change the functional parts such as different ways of applying notch filter, or CARefencing, or
#' wavelet, go to:
#'      pre_notch_filter.R, pre_wavelet.R, or pre_units.R and have a look
#' However, if you have structural change, be careful, or write your own pre-processing script
#'
#' You don't need to be the same as what I did here, however, if you want to use web interface,
#' here are some functions that are needed, and examples of how they will be called.
#'


# (MUST comment out this block)
#
# # notch filter
# bulk_notch(project_name, subject_code, blocks, channels, srate = 2000,
#            frequencies = seq(4, 200, by = 4))
#
# # Common average referencing
# bulk_CAR(project_name, subject_code, blocks, channels)
#
# # wavelet
# bulk_wavelet(project_name, subject_code, blocks, channels, data_dir = rave_opts$get_options('data_dir'),
#              srate = 2000, frequencies = seq(4, 200, by = 4), wave_num = 7, compress = 2)
#
#
# # concatenate blocks together (TODO make it bulk function and run automatically after wavelet)
# pre_concat(project_name = 'Congruency', subject_code = 'YAB', blocks = c('008', '010', '011', '012'),
#            chl = 20)
#
# ## inspect (visualizations)
#
# # Plot for notch before and after comparison
# pre_inspect(process_name = 'notch', project_name = 'Congruency', subject_code = 'YAB', block_num = '008', chls = 20)
#
# # for CARinspect, it should return the compressed signals for parallel plots (check eegplot.m)
# ss = pre_inspect(process_name = 'CARinspect', compress = 200, project_name = 'Congruency',
#                  subject_code = 'YAB', block_num = '008', chls = 1:84)
#
# plot_signals(
#   ss, sample_rate = 20, space = 0.99999, plot = 'plotly', channel_names = 1:84,  # xlim = c(0, 50)
#   col = col
#
# )
#
#
# # CAR, Post inspect: brfore and after CAR
# pre_inspect(process_name = 'CAR', project_name = 'Congruency', subject_code = 'YAB', block_num = '008', chls = 20)
#
#

