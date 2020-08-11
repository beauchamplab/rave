session = getDefaultReactiveDomain()
output = getDefaultReactiveOutput()
input = getDefaultReactiveInput()


# Default script for modules
`.__internal_reactives__.` = reactiveValues(
  miss_data = FALSE,
  miss_data_message = '',
  miss_data_comps = NULL,
  miss_data_size = 0,
  prevent_load = TRUE,
  incomplete_data = NULL
)

# for debug
if(is.null(session)){
  ns = I
  ..runtime_env = environment()
}

#global_reactives$execute_module
output[['.__rave_modal__.']] <- renderUI({
  dipsaus::cat2('Prepared to show data loading modal')
  # Unlock data loading
  `.__internal_reactives__.`[['prevent_load']] = FALSE

  miss_data = `.__internal_reactives__.`[['miss_data']]
  miss_data_message = `.__internal_reactives__.`[['miss_data_message']]
  total_size = `.__internal_reactives__.`[['miss_data_size']]
  if(!is.numeric(total_size) || length(total_size) != 1){
    total_size = NA
  }
  speed = rave_options('drive_speed')
  if(!is.numeric(speed) || length(speed) != 2){
    speed = 100
  }else{
    speed = speed[2]
  }
  # miss_data_comps = `.__internal_reactives__.`[['miss_data_comps']]
  if(!miss_data || !length(miss_data_message)){
    dipsaus::cat2('No data is missing, proceed.')
    return(NULL)
  }

  # Check if there's any broken data
  broken_data = `.__internal_reactives__.`[['incomplete_data']]
  if(length(broken_data)){
    load_btn = NULL
    data_msg = '*There is one or more data missing. This module is diabled.'
  }else{


    load_btn = tags$button(
      id = ns('.__load_data__.'),
      type = "button",
      class = "btn btn-primary action-button shiny-bound-input",
      style = 'margin-left:15px;',
      "Load Data"
    )
    data_msg = sprintf('Estimated size: %s, (%.1f seconds)', dipsaus::to_ram_size(total_size), total_size * 3 / speed / 1e6)
  }

  div(
    class = 'rave-content-modal',
    div(
      class = 'centered',
      h3('Load Data...', style = 'color: cadetblue;'),
      p('This module requires the following datasets to be loaded.', style = 'font-style: italic; color: cadetblue;'),
      tags$ul(
        tagList(
          lapply(miss_data_message, tags$li)
        )
      ),
      hr(),
      div(
        style = 'float: right;',
        actionButton(ns('.__switch_back__.'), 'Go Back'),
        load_btn
        # actionButton(ns('.__load_data__.'), 'Load Data')
      ),
      p(
        data_msg
      )
    )
  )
})

observeEvent(input[['.__switch_back__.']], {
  switch_to()
})

observeEvent(input[['.__load_data__.']], {
  # Check if lock is on, prevent multiple click
  if(isolate(`.__internal_reactives__.`[['prevent_load']])){
    return()
  }
  `.__internal_reactives__.`[['prevent_load']] = TRUE

  miss_data = `.__internal_reactives__.`[['miss_data']]
  quos = `.__internal_reactives__.`[['miss_data_comps']]
  msg = `.__internal_reactives__.`[['miss_data_message']]
  if(miss_data && length(quos) && length(msg) == length(quos)){
    # load data
    n_data = length(msg)
    progress = progress('Loading data', max = n_data)
    on.exit({progress$close()})

    tryCatch({
      for(i in seq_len(n_data)){
        progress$inc(msg[[i]])
        dipsaus::eval_dirty(quos[[i]], env = ..runtime_env)
      }
      `.__internal_reactives__.`[['miss_data']] = F
    }, error = function(e){
      showNotification(p('One or more error occur during loading. The data might be broken or missing.'), type = 'error')
    })
    

    reload_module()
  }
})

rave_checks = function(..., data = NULL){

  data = unlist(c(data, list(...)))
  if(!length(data)){
    return()
  }
  is_reactive = F
  if(is.null(getDefaultReactiveDomain())){
    `.__internal_reactives__.` = list()
    is_reactive = T
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
  broken_data_type = NULL
  total_size = 0
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
        size = n1 * n2 * n3 * n4 * base_size
        total_size = total_size + size
        size = dipsaus::to_ram_size(size)

        # check if directory exists
        if(!data_check$check$power_dir){
          broken_data_type = c(broken_data_type, 'power')
          msg = c(msg, 'Power (Missing)')
        }else{
          msg = c(msg, sprintf('Power (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
        }
      }
      rm(dat)
    }else if('phase' %in% d){
      dat = module_tools$get_phase(force = F, referenced = referenced)
      if(is.null(dat)){
        quos = c(quos, rlang::quo({
          module_tools$get_phase(referenced = !!referenced)
        }))
        size = n1 * n2 * n3 * n4 * base_size
        total_size = total_size + size
        size = dipsaus::to_ram_size(size)

        # check if directory exists
        if(!data_check$check$phase_dir){
          broken_data_type = c(broken_data_type, 'phase')
          msg = c(msg, 'Phase (Missing)')
        }else{
          msg = c(msg, sprintf('Phase (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
        }
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

          size = n_el * n_tp * base_size
          total_size = total_size + size

          size = dipsaus::to_ram_size(size)

          msg = c(msg, sprintf('Voltage (No epoch, %s)', size))
        }
      }else{
        dat = module_tools$get_voltage(force = F, referenced = referenced)
        if(is.null(dat)){
          quos = c(quos, rlang::quo({
            module_tools$get_voltage(referenced = !!referenced)
          }))

          size = n1 * n3 * n4 * base_size / srate_wave * srate_volt
          total_size = total_size + size

          size = dipsaus::to_ram_size(size)

          # check if directory exists
          if(!data_check$check$phase_dir){
            broken_data_type = c(broken_data_type, 'voltage')
            msg = c(msg, 'Voltage (Missing)')
          }else{
            msg = c(msg, sprintf('Voltage (%s, %s)', ifelse(referenced, 'Referenced', 'Raw'), size))
          }
        }
      }
      rm(dat)
    }
  }

  broken_data_type = unique(broken_data_type)
  if(length(quos)){
    # we have data pending to be loaded
    order = order(msg)
    msg = msg[order]
    quos = quos[order]

    # show modal
    `.__internal_reactives__.`[['miss_data']] = T
    `.__internal_reactives__.`[['miss_data_message']] = msg
    `.__internal_reactives__.`[['miss_data_comps']] = quos
    `.__internal_reactives__.`[['miss_data_size']] = total_size
    `.__internal_reactives__.`[['incomplete_data']] = broken_data_type

    stop('Need to load data')
  }else{
    `.__internal_reactives__.`[['miss_data']] = F
  }



  return()

}


register_auto_calculate_widget = local({
  
  session = getDefaultReactiveDomain()
  output = getDefaultReactiveOutput()
  input = getDefaultReactiveInput()
  
  this_env = environment()
  checkbox = NULL
  buttons = NULL
  
  input_ids = get_input_ids()
  
  eval_when_ready(function(...){
    input_ids = get_input_ids()
    
    params = new.env(parent = emptyenv())
    
    observe({
      if(!auto_recalculate( include_temporary = FALSE ) && length(input_ids)){
        
        changed = vapply(input_ids, function(id){
          if(identical(params[[id]], input[[id]])){
            return(FALSE)
          }
          TRUE
        }, FUN.VALUE = FALSE)
        
        if(any(changed)){
          dipsaus::cat2('At least one changed: ', paste(input_ids[changed], collapse = ', '))
          lapply(this_env$buttons, function(bid){
            dipsaus::updateActionButtonStyled(session, bid, disabled = FALSE, icon = shiny::icon('arrow-right'))
          })
        }else{
          lapply(this_env$buttons, function(bid){
            dipsaus::updateActionButtonStyled(session, bid, disabled = TRUE, icon = NULL)
          })
        }
      }
    }, env = environment(), priority = 1) # needs to be prior to rave_execute
    
    if(length(this_env$checkbox) == 1){
      observeEvent(input[[this_env$checkbox]], {
        auto_calc = input[[this_env$checkbox]]
        if(!(length(auto_calc) == 1 && is.logical(auto_calc))){ return() }
        auto_recalculate( auto_calc )
        
        if(auto_calc){
          icon = shiny::icon('arrow-right')
        }else{
          icon = NULL
        }
        
        
        lapply(this_env$buttons, function(bid){
          dipsaus::updateActionButtonStyled(session, bid, disabled = auto_calc, icon = icon)
        })
        
        
        if(auto_calc){
          trigger_recalculate()
        }
        
      }, event.env = environment(), handler.env = environment())
    }
    
    
    
    lapply(this_env$buttons, function(inputId){
      observeEvent(input[[inputId]], {
        dipsaus::cat2('Recalculate Triggered!', level = 'INFO')
        
        lapply(input_ids, function(id){
          params[[id]] = input[[id]]
          NULL
        })
        
        # trigger re-calculate
        trigger_recalculate( force = TRUE )
        
        lapply(buttons, function(bid){
          dipsaus::updateActionButtonStyled(session, bid, disabled = TRUE, icon = NULL)
        })
      }, event.env = environment(), handler.env = environment())
      NULL
    })
    
  })
  
  function(inputId, type = c('button', 'checkbox'), default_on = TRUE){
    type = match.arg(type)
    
    if(type == 'checkbox'){
      if(!is.null(this_env$checkbox)){
        dipsaus::cat2('Auto-recalculate checkbox is defined. Only one widget is allowed', level = 'WARNING')
        print(this_env$checkbox)
      }
      this_env$checkbox = inputId
      auto_recalculate( default_on )
      
    }else{
      this_env$buttons = c(buttons, inputId)
    }
    
  }
  
})

rave_execute({
  missing_data = isolate(`.__internal_reactives__.`[['miss_data']])
  if( missing_data ){
    rave_failure('Need to load data. Waiting for an action.', level = 'INFO')
  }
  
  if( !auto_recalculate( include_temporary = TRUE, cancel_temporary = TRUE ) ){
    rave_failure('Auto Re-calculate is off.', level = 'INFO')
  }
})
