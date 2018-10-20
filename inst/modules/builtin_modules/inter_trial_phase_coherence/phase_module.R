# Load packages
require(magrittr)
# Load utils
source('./plot_helpers.R')
source('./plot_funcs.R')
source('./phase_module_ui.R')
source('./phase_module_outputs.R')

# Reactive values
local_data = reactiveValues()

# Initialize some variables so that they always exist
calc_result = NULL
has_groups = F

rave_execute({
  calc_result = main_function(electrode = electrode, frequency_range = FREQUENCY)
  if(is.null(calc_result)){
    has_groups = F
  }else{
    has_groups = calc_result$n_groups > 0
  }
})

main_function = function(electrode, frequency_range){

  dat = tryCatch({phase$subset(
    Frequency = Frequency %within% frequency_range,
    Electrode = Electrode == electrode
  )}, error = function(e){
    NULL
  })


  if(is.null(dat) || is.null(dat$dim) || any(dat$dim == 0)){
    return(NULL)
  }

  time = dat$dimnames$Time
  frequency = dat$dimnames$Frequency

  if(dat$hybrid){
    dat$hybrid = TRUE
  }

  # Make phase data complex numbers
  val = dat$get_data()
  dat$set_data(exp(1i * val))

  # for each groups, calculate inter-trial coherence for each frequencies
  lapply(1:length(GROUPS), function(ii){
    g = GROUPS[[ii]]
    trial_number = epoch_tbl$Trial[epoch_tbl$Condition %in% g$GROUP]
    n_trials = length(trial_number)
    if(!n_trials){
      return(list(
        has_trials = F
      ))
    }
    sub = dat$subset(Trial = Trial %in% trial_number)
    intc_cplx = sub$collapse(keep = c(3, 2))

    intc_real = Mod(intc_cplx)

    plot_title = sprintf('%s %d Trials', g$GROUP_NAME, n_trials)

    list(
      which_group = ii,
      has_trials = T,
      n_trials = n_trials,
      group_name = g$GROUP_NAME,
      data = intc_real,
      range = range(intc_real),
      name = plot_title
    )
  }) ->
    res

  n_groups = sum(0, vapply(res, function(x){x$has_trials}, FUN.VALUE = FALSE))

  if(n_groups > 0){
    actual_lim = lapply(res, function(x){
      x$range
    })

    actual_lim = range(unlist(actual_lim))
    actual_lim[abs(actual_lim) < 0.01] = 0
  }else{
    actual_lim = c(0,1)
  }


  return(list(
    results = res,
    n_groups = n_groups,
    actual_lim = actual_lim,
    time = time,
    frequency = frequency
  ))
}

threeD_viewer = function(){
  div(
    class = 'rave-grid-inputs',
    style = 'padding: 5px 0px 5px 5px;',
    div(
      style="flex-basis: 30%; display: block; ",
      actionLink(ns('calc_data'), 'Refresh/Generate Data'),
      uiOutput(ns('ui_viewer_download'))
    ),
    div(
      style="flex-basis: 70%; display: block; ",
      selectInput(ns('viewer_var'), 'Group Name', choices = '')
    ),
    div(
      style="flex-basis: 100%; display: block; width: 100%;",
      threejsr::threejsOutput(ns('three_viewer'), height = '500px')
    )
  )
}


output$ui_viewer_download <- renderUI({
  if(!is.null(input$viewer_var) && is.list(local_data$viewer_data)){
    downloadLink(ns('viewer_download'), 'Export & Download')
  }
})

observeEvent(input$calc_data, {
  if(!has_groups){
    showNotification(p('No group found.'), type = 'error', id = ns('noti'))
    return()
  }
  es = preload_info$electrodes
  progress = rave::progress('Calculating ITPC', max = length(es))
  on.exit({progress$close()})

  lapply(es, function(e){
    progress$inc(message = sprintf('electrode %d', e))
    re = main_function(electrode = e, frequency_range = FREQUENCY)
    lapply(re$results, function(x){
      if(!x$has_trials){
        return(NULL)
      }
      if(is.blank(x$group_name)){
        x$group_name = sprintf('Group %d', x$which_group)
      }
      # Collapse INTC
      data = rowMeans(x$data, na.rm = T)
      list(
        group_name = x$group_name,
        data = data
      )
    }) ->
      dat
    dat = dropNulls(dat)

    names(dat) = sapply(dat, '[[', 'group_name')
    list(
      dat = dat,
      time = re$time
    )
  }) ->
    re

  # collapse
  time = re[[1]]$time
  group_names = names(re[[1]]$dat)
  lapply(group_names, function(name){
    sapply(1:length(es), function(ii){
      re[[ii]]$dat[[name]]$data
    })
  }) ->
    dat
  names(dat) = group_names

  updateSelectInput(session, 'viewer_var', choices = group_names)

  local_data$viewer_data = list(
    dat = dat,
    group_names = group_names,
    time = time,
    electrodes = preload_info$electrodes
  )
})


output$viewer_download <- downloadHandler(
  filename = function(){
    sid = stringr::str_replace(subject$id, '/', '_')
    sprintf('[%s] ITPC.html', sid)
  },
  content = function(con){
    showNotification(p('Generating... This will take a while. Please stay and wait :)'),
                     type = 'message', id = ns('noti'), duration = NULL)

    tryCatch({
      sid = stringr::str_replace(subject$id, '/', '_')
      viewer_data = isolate(local_data$viewer_data)
      viewer_var = input$viewer_var

      # Get data
      electrodes = viewer_data$electrodes
      time = viewer_data$time
      keyframe_shift = min(time)

      widget = module_tools$plot_3d_electrodes(
        electrodes = electrodes, key_frame = time - keyframe_shift,
        values = viewer_data$dat[[viewer_var]], control_gui = T, keyframe_shift = keyframe_shift
      )
      htmlwidgets::saveWidget(widget, con, selfcontained = T,
                              title = sprintf('ITPC - %s', sid))
      showNotification(p('Finished exporting INTC'), type = 'message', id = ns('noti'), duration = 5)
    }, error = function(e){
      showNotification(p('Error: ', as.character(e),
                         '(thanks for reporting errors to github: beauchamplab/rave)'),
                       type = 'error', id = ns('noti'), duration = 5)
    })


  }
)


output$three_viewer <- threejsr::renderThreejs({
  validate(
    need(!is.null(input$viewer_var) && is.list(local_data$viewer_data),
         message = 'Click the link "Refresh/Generate Data" to start.')
  )
  viewer_data = local_data$viewer_data
  viewer_var = input$viewer_var

  # Get data
  electrodes = viewer_data$electrodes
  time = viewer_data$time
  keyframe_shift = min(time)

  module_tools$plot_3d_electrodes(
    electrodes = electrodes, key_frame = time - keyframe_shift,
    values = viewer_data$dat[[viewer_var]], control_gui = T, keyframe_shift = keyframe_shift
  )
})


if(is.null(shiny::getDefaultReactiveDomain())){
  debug = TRUE
}else{
  debug = FALSE
}

if(debug){
  # Debug module
  m = ModuleEnvir$new(module_id = 'phase', 'Phase', script_path = 'phase_module.R'); init_app(m)
}
