session = getDefaultReactiveDomain()
output = getDefaultReactiveOutput()
input = getDefaultReactiveInput()


# Default script for modules
`.__internal_reactives__.` = reactiveValues(
  miss_data = FALSE,
  miss_data_message = '',
  miss_data_comps = NULL
)

# for debug
if(is.null(session)){
  ns = I
  ..runtime_env = environment()
}


output[['.__rave_modal__.']] <- renderUI({
  miss_data = `.__internal_reactives__.`[['miss_data']]
  miss_data_message = `.__internal_reactives__.`[['miss_data_message']]
  # miss_data_comps = `.__internal_reactives__.`[['miss_data_comps']]
  if(!miss_data || !length(miss_data_message)){
    return(NULL)
  }

  div(
    style = "width: 100%;
    height: 100vh;
    margin-bottom: -100vh;
    background: rgba(0,0,0,0.8);
    margin-top:  -35px;
    z-index: 1000;
    position: relative;",
    div(
      class = 'centered',
      h3('More Data Needed...'),
      p('The following datasets need to be loaded.'),
      tags$ul(
        tagList(
          lapply(miss_data_message, tags$li)
        )
      ),
      actionButton(ns('.__load_data__.'), 'Load Data')
    )
  )
})

observeEvent(input[['.__load_data__.']], {
  miss_data = `.__internal_reactives__.`[['miss_data']]
  quos = `.__internal_reactives__.`[['miss_data_comps']]
  msg = `.__internal_reactives__.`[['miss_data_message']]
  if(miss_data && length(quos) && length(msg) == length(quos)){
    # load data
    n_data = length(msg)
    progress = progress('Loading data', max = n_data)
    on.exit({progress$close()})
    for(i in seq_len(n_data)){
      progress$inc(message = msg[[i]])
      eval_dirty(quos[[i]], env = ..runtime_env)
    }
    `.__internal_reactives__.`[['miss_data']] = F
    reload_module()
  }
})

rave_checks = function(..., data = NULL){
  data = unlist(c(data, list(...)))
  if(!length(data)){
    return()
  }
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

}


rave_execute({})
