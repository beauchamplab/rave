# for dev use

.load_preprocess_func <- function(
  # Here are the packages needed to pre-process data
  packages = c('rave', 'tidyverse', 'rhdf5', 'grid', 'gridBase', 'signal'),

  # don't change it
  local = F
){
  # fixed usage !
  env = new.env()

  if(!local){
    # used on global scope
    for(p in packages){
      library(p, character.only = T)
    }

  }else{
    # used by the package DON'T CHANGE
    # TODO: add code to automatically load environment

    source = function(file, local = T,...){
      rave::logger('Loading preprocess script - ', file)

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
  source('inst/preprocess/plot_signal.R')
  source('inst/preprocess/pre_import_signal.R')
  source('inst/preprocess/pre_load_settings.R')
  source('inst/preprocess/pre_resample.R')
  source('inst/preprocess/pre_notch_filter.R')
  source('inst/preprocess/pre_wavelet.R')
  source('inst/preprocess/pre_units.R')
  source('inst/preprocess/bulk_preprocess.R')


  # End
  #########################################

  return(env)
}
