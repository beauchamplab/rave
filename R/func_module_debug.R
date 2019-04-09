# This script stores some default functions used to debug modules
# These functions will be override by RAVE during actual runtime

#' Check if data is loaded for current module
#' @param ... see defails
#' @param data same as ..., but can be a vector
#' @details This function checks whether ECoG data is loaded. The format is: DATA+(blankspace)+TYPE. DATA can be
#' "power" (Wavelet transform amplitude), "phase" (Complex angle), or "volt"/"voltage" (Before wavelet). TYPE can
#' be "raw" (no reference), "referenced" (referenced by common average reference, white matter reference, or bipolar reference).
#' For voltage data, there is one more special type "full" which loads voltage data for all electrodes.
#' @export
rave_checks <- function(..., data = NULL){
  data = unlist(c(data, list(...)))
  if(!length(data)){
    return()
  }
  is_reactive = F
  if(is.null(getDefaultReactiveDomain())){
    `.__internal_reactives__.` = list()
    is_reactive = T
  }

  rave_data = getDefaultDataRepository()
  module_tools = rave_data$module_tools
  preload_info = rave_data$preload_info
  subject = rave_data$subject


  n1 = nrow(module_tools$get_meta(name = 'trials'))
  n2 = length(preload_info$frequencies)
  n3 = length(preload_info$time_points)
  n4 = length(preload_info$electrodes)
  srate_wave = module_tools$get_sample_rate(original = F)
  srate_volt = module_tools$get_sample_rate(original = T)

  data = unlist(stringr::str_split(data, ','))
  data = stringr::str_to_lower(data)
  data = stringr::str_split(data, '\\ ')

  quos = NULL
  msg = NULL
  for(d in data){
    referenced = 'referenced' %in% d
    full = 'full' %in% d

    # 8 bytes is the default value. However, reference might not be cached, therefore in reference cases RAM size doubles. 8.25 takes into account for left-over objects
    base_size = ifelse(referenced, 16.5, 8.25)

    if('power' %in% d){
      dat = module_tools$get_power(force = F, referenced = referenced)
      if(is.null(dat)){
        quos = c(quos, rlang::quo({
          module_tools$get_power(referenced = !!referenced)
        }))
        size = to_ram_size(n1 * n2 * n3 * n4 * base_size)

        msg = c(msg, sprintf('Power (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
      }
      rm(dat)
    }else if('phase' %in% d){
      dat = module_tools$get_phase(force = F, referenced = referenced)
      if(is.null(dat)){
        quos = c(quos, rlang::quo({
          module_tools$get_phase(referenced = !!referenced)
        }))
        size = to_ram_size(n1 * n2 * n3 * n4 * base_size)
        msg = c(msg, sprintf('Phase (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
      }
      rm(dat)
    }else if('volt' %in% d || 'voltage' %in% d){
      if(full){
        data_env = getDefaultDataRepository()
        dat = data_env$.private[['volt_unblocked']]
        rm(data_env)
        if(is.null(dat)){
          quos = c(quos, rlang::quo({
            module_tools$get_voltage2()
          }))
          n_tp = nrow(subject$time_points) / srate_wave * srate_volt
          n_el = nrow(subject$electrodes)
          size = to_ram_size(n_el * n_tp * base_size)
          msg = c(msg, sprintf('Voltage (No epoch, %s)', size))
        }
      }else{
        dat = module_tools$get_voltage(force = F, referenced = referenced)
        if(is.null(dat)){
          quos = c(quos, rlang::quo({
            module_tools$get_voltage(referenced = !!referenced)
          }))
          size = to_ram_size(n1 * n3 * n4 * base_size / srate_wave * srate_volt)
          msg = c(msg, sprintf('Voltage (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
        }
      }
      rm(dat)
    }
  }

  if(length(quos)){
    # we have data pending to be loaded
    order = order(msg)
    msg = msg[order]
    quos = quos[order]
    # show modal
    `.__internal_reactives__.`[['miss_data']] = T
    `.__internal_reactives__.`[['miss_data_message']] = msg
    `.__internal_reactives__.`[['miss_data_comps']] = quos
    stop('Needs to load data')
  }else{
    `.__internal_reactives__.`[['miss_data']] = F
  }

  if(is_reactive){
    print(`.__internal_reactives__.`)
  }

  return()

}



