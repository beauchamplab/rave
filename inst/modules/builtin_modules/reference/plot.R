# Debug
if(FALSE){
  m = ModuleEnvir$new(module_id = 'mid', 'ref', script_path = './inst/modules/builtin_modules/reference/main.R'); init_app(m)
  ns = shiny::NS('mid')

  # Post
  self = execenv = m$private$exec_env$WTiF6hxsLeCa0YtykHuD
  execenv$private$inputs
  ref_group = execenv$param_env$ref_group

  session = execenv$static_env$session


  group_info = execenv$static_env$current_group()
  ref_tbl = isolate(execenv$static_env$local_data$ref_tbl)
  local_data = isolate(reactiveValuesToList(execenv$static_env$local_data))
  v = execenv$static_env$env$volt
  env = execenv$static_env$env
  object_size(v)

  execenv$static_env$channel_plot()
  # max(v$`008`[[1]])

}


observe({
  local_data$electrode_plt_block = input$electrode_plt_block
  local_data$electrode_plt_electrode = input$electrode_plt_electrode
  local_data$electrode_plt_win = input$electrode_plt_win
  local_data$electrode_plt_mfreq = input$electrode_plt_mfreq
})

electrode_plot_ui = function(){
  ref_tbl = local_data$ref_tbl
  local_data$refresh
  logger('electrode_plot_ui')

  group_info = current_group()
  if(is.null(group_info) || !is.data.frame(ref_tbl)){
    return(p('Please select a valid group.'))
  }

  blocks = subject$preprocess_info('blocks'); blocks %?<-% ''
  electrode_plt_block = isolate(local_data$electrode_plt_block);
  electrode_plt_block %?<-% blocks[1]; if(!electrode_plt_block %in% blocks){ electrode_plt_block = blocks[1] }

  electrodes = group_info$electrodes
  electrode_plt_electrode = isolate(local_data$electrode_plt_electrode);
  electrode_plt_electrode %?<-% electrodes[1]; electrode_plt_electrode = as.integer(electrode_plt_electrode);
  if(!electrode_plt_electrode %in% electrodes){ electrode_plt_electrode = electrodes[1] };

  srate = subject$preprocess_info('srate')
  electrode_plt_win = isolate(local_data$electrode_plt_win); electrode_plt_win %?<-% ceiling(srate * 2)

  electrode_plt_mfreq = isolate(local_data$electrode_plt_mfreq); electrode_plt_mfreq %?<-% min(300, srate/2);


  fluidRow(
    column(
      width = 9,
      plotOutput(ns('electrode_plot_raw'), height = '70vh'),
      plotOutput(ns('electrode_plot_ref'), height = '70vh')
    ),
    div(
      class = 'col-sm-3 rave-elastic-fixed-right',
      h4('Data Control'),
      hr(),
      selectInput(ns('electrode_plt_block'), 'Block', choices = blocks, selected = electrode_plt_block),
      selectInput(ns('electrode_plt_electrode'), 'Electrode', choices = electrodes, selected = electrode_plt_electrode),
      div(
        actionButton(ns('electrode_plt_prev'), 'Previous'),
        actionButton(ns('electrode_plt_next'), 'Next')
      ),
      h4('Graphic Control'),
      hr(),
      h6('Periodogram'),
      sliderInput(ns('electrode_plt_win'), 'Pwelch Window Length', min = 100, max = ceiling(srate * 2), value = electrode_plt_win),
      sliderInput(ns('electrode_plt_mfreq'), 'Max Frequency', min = 10, max = floor(srate / 2), value = electrode_plt_mfreq)
    )
  )
}

observeEvent(input$electrode_plt_prev, {
  try({
    group_info = current_group()
    electrodes = group_info$electrodes
    e = as.integer(input$electrode_plt_electrode)
    e = electrodes[electrodes < e]
    if(length(e)){
      updateSelectInput(session, inputId = 'electrode_plt_electrode', selected = max(e))
    }
  }, silent = T)
})

observeEvent(input$electrode_plt_next, {
  try({
    group_info = current_group()
    electrodes = group_info$electrodes
    e = as.integer(input$electrode_plt_electrode)
    e = electrodes[electrodes > e]
    if(length(e)){
      updateSelectInput(session, inputId = 'electrode_plt_electrode', selected = min(e))
    }
  }, silent = T)
})

load_ref = function(ref){
  blocks = subject$preprocess_info('blocks')
  re = sapply(blocks, function(b){
    list(
      volt = 0,
      coef = 0
    )
  }, simplify = F, USE.NAMES = T)


  if(!length(ref) || is.blank(ref)){
    return(re)
  }

  if(!is.null(env[[ref]])){
    return(env[[ref]])
  }


  es = str_extract(ref, '[0-9\\-,]+'); if(is.na(es)){es = ''}
  e = rave:::parse_selections(es)
  e = subject$filter_all_electrodes(e)

  if(length(e) == 0){
    # noref
    return(re)
  }

  fpath = file.path(subject$dirs$channel_dir, 'reference', ref %&% '.h5')

  if(!file.exists(fpath)){
    # Maybe bipolar reference
    if(length(e) != 1){
      return(re)
    }
    # Bipolar
    fname = sprintf('%d.h5', e)
    sapply(blocks, function(b){
      power = load_h5(file.path(subject$dirs$channel_dir, 'power', fname), name = '/raw/power/' %&% b)[]
      phase = load_h5(file.path(subject$dirs$channel_dir, 'phase', fname), name = '/raw/phase/' %&% b)[]
      list(
        volt = load_h5(file.path(subject$dirs$channel_dir, 'voltage', fname), name = '/raw/voltage/' %&% b)[],
        coef = sqrt(power) * exp(1i * phase)
      )
    }, simplify = F, USE.NAMES = T) ->
      re
  }else{
    sapply(blocks, function(b){
      coef = load_h5(fpath, '/wavelet/coef/' %&% b)[]
      list(
        volt = load_h5(fpath, '/voltage/' %&% b)[],
        coef = coef[,,1] * exp(1i * coef[,,2])
      )
    }, simplify = F, USE.NAMES = T) ->
      re
  }
  env[[ref]] = re
  return(re)
}

output$electrode_plot_raw = renderPlot({
  ref_tbl = local_data$ref_tbl


  blocks = subject$preprocess_info('blocks'); blocks %?<-% ''
  block = local_data$electrode_plt_block; block %?<-% blocks[1]

  group_info = current_group()
  electrode = local_data$electrode_plt_electrode
  electrode %?<-% group_info$electrodes[1]; electrode = as.integer(electrode)

  srate = subject$preprocess_info('srate')

  # s = env$volt[[block]][[electrode]] has one problem, when electrode is not from 1-XXX, some
  # electrodes are excluded at the very first
  s = env$volt[[block]][ref_tbl$Electrode == electrode][[1]]

  # get reference info
  ref_name = ref_tbl$Reference[ref_tbl$Electrode == electrode]

  ref = load_ref(ref_name)[[block]][['volt']]

  valid = T
  if(is.blank(ref_name)){
    ref_name = 'Invalid'
    valid = F
  }else{
    ref_name = 'Reference - ' %&% ref_name
  }

  grid::grid.newpage()
  lay <- rbind(c(1,1,1), c(2,2,2), c(3,4,5))
  graphics::layout(mat = lay)
  mai = par('mai');
  on.exit({par(mai = mai)}, add = T)
  par(mai = c(0.7, 0.8 ,0.4, 0.25))

  col = c(ifelse(valid, 'cornflowerblue', 'tomato3'), 'grey60')

  main = sprintf('Electrode - %d [raw]', electrode)
  rave::diagnose_signal(
    s, col = col[2],
    srate = srate,
    main = main,
    cex = 2,
    name = 'Reference',
    which = 1
  ) ->
    re

  main = sprintf('Electrode - %d [%s, %s]', electrode, ref_name, ref_tbl$Type[ref_tbl$Electrode == electrode])
  rave::diagnose_signal(
    s - ref, col = col[1],
    srate = srate,
    main = main,
    cex = 2,
    name = 'Reference',
    which = 1, boundary = re$boundary
  )

  win_len = (local_data$electrode_plt_win); win_len %?<-% ceiling(srate * 2)
  max_freq = (local_data$electrode_plt_mfreq); max_freq %?<-% min(300, srate/2);


  rave::diagnose_signal(
    s - ref, col = col,
    s2 = s,
    srate = srate,
    name = 'Reference',
    cex = 2, window = win_len, noverlap = win_len / 2, max_freq = max_freq,
    main = main, which = c(2,3,4)
  )
})


observe({
  local_data$parallel_plt_start = input$parallel_plt_start
})
observe({
  local_data$parallel_plt_block = block = input$parallel_plt_block
  local_data$parallel_plt_space = input$parallel_plt_space
  local_data$parallel_plt_duration = duration = input$parallel_plt_duration
  local_data$parallel_plt_excl = rave:::parse_selections(input$parallel_plt_excl)
  local_data$parallel_plt_refed_hidden = input$parallel_plt_refed_hidden

  try({
    len = unlist(lapply(env$volt[[block]], length))
    len = max(c(len, 1024) - 1000) / subject$preprocess_info('srate')
    duration %?<-% 1; duration = max(duration, 1)
    local_data$parallel_plt_start_step = duration * 0.8
    local_data$parallel_plt_start_max = len
    updateSliderInput(session, 'parallel_plt_start', step = duration * 0.8, max = len)
  }, silent = T)
})

parallel_plot_ui = function(){
  ref_tbl = local_data$ref_tbl
  local_data$refresh
  logger('parallel_plot_ui')

  group_info = current_group()
  if(is.null(group_info) || !is.data.frame(ref_tbl)){
    return(p('Please select a valid group.'))
  }


  blocks = subject$preprocess_info('blocks'); blocks %?<-% ''
  parallel_plt_block = isolate(local_data$parallel_plt_block);
  parallel_plt_block %?<-% blocks[1]; if(!parallel_plt_block %in% blocks){ parallel_plt_block = blocks[1] }

  parallel_plt_space = isolate(local_data$parallel_plt_space)
  parallel_plt_space %?<-% 1000; parallel_plt_space = max(parallel_plt_space, 0)

  parallel_plt_start = isolate(local_data$parallel_plt_start)
  parallel_plt_start %?<-% 0;
  parallel_plt_start_step = isolate(local_data$parallel_plt_start_step); parallel_plt_start_step %?<-% 5
  parallel_plt_start_max = isolate(local_data$parallel_plt_start_max); parallel_plt_start_max %?<-% 300

  parallel_plt_duration = isolate(local_data$parallel_plt_duration)
  parallel_plt_duration %?<-% 5;

  parallel_plt_excl = isolate(local_data$parallel_plt_excl)
  parallel_plt_excl = parallel_plt_excl[parallel_plt_excl %in% group_info$electrodes]

  parallel_plt_refed_hidden = isolate(local_data$parallel_plt_refed_hidden); parallel_plt_refed_hidden %?<-% 'all'

  local_data$do_parallel_plot = Sys.time()

  fluidRow(
    column(
      width = 9,
      plotOutput(ns('parallel_plot'), height = '90vh')
    ),
    div(
      class = 'col-sm-3 rave-elastic-fixed-right',
      h4('Data Control'),
      hr(),
      selectInput(ns('parallel_plt_block'), 'Block', choices = blocks, selected = parallel_plt_block),
      numericInput(ns('parallel_plt_space'), 'Vertical Space', min = 0, value = parallel_plt_space),
      sliderInput(ns('parallel_plt_duration'), 'Duration', min = 1, value = parallel_plt_duration, max = 30, round = -1, step = 0.1, ticks = F),
      sliderInput(ns('parallel_plt_start'), 'Start Time', min = 0, max = parallel_plt_start_max, value = parallel_plt_start, step = parallel_plt_start_step, round = T, post = 's', ticks = F),
      h4('Graphic Control'),
      hr(),
      radioButtons(ns('parallel_plt_refed_hidden'), label = NULL,
                   choiceNames = c('Show all', 'Show original signals only', 'Show referenced signals only'),
                   choiceValues = c('all', 'raw', 'ref'),
                   selected = parallel_plt_refed_hidden),
      textInput(ns('parallel_plt_excl'), 'Hide Electrodes', value = rave:::deparse_selections(parallel_plt_excl))
    )
  )
}

channel_plot = function(){
  # Set par
  mai = par('mai')
  on.exit({par(mai = mai)})
  par(mai = local({
    mai[3:4] = 0.01;
    mai
  }))


  group_info = current_group()
  ref_tbl = local_data$ref_tbl
  # find bad electrodes
  sel = ref_tbl$Electrode %in% group_info$electrodes
  ref = ref_tbl$Reference
  col = rep('cornflowerblue', sum(sel))
  isbad = ref[sel] == ''
  col[isbad] = 'red'
  all_electrodes = seq_along(ref_tbl$Electrode)

  n_electrode_total = length(ref_tbl$Electrode)

  sel_hide = group_info$electrodes %in% local_data$parallel_plt_excl

  space = local_data$parallel_plt_space; space %?<-% 1000; if(space <= 0) space = 1000;
  block = local_data$parallel_plt_block
  start = local_data$parallel_plt_start; start %?<-% 0; start = max(start, 0)
  duration = local_data$parallel_plt_duration; duration %?<-% 1; duration = max(duration, 1)
  srate = subject$preprocess_info('srate')


  s = sapply(env$volt[[block]][all_electrodes], function(x){
    ind = seq(start*srate + 1, min((start + duration) * srate + 2, length(x)))
    x[ind]
  })
  s = t(s)

  # Calculate reference
  sapply(ref, function(r){
    r = str_extract(r, '[0-9,\\-]+')
    if(is.na(r)){
      return(rep(0, n_electrode_total))
    }
    s = rave:::parse_selections(r)
    s = ref_tbl$Electrode %in% s
    s / sum(s)
  }) ->
    mat
  mat = -t(mat)
  diag(mat) = diag(mat) + 1


  s_na = rep(NA, ncol(s))
  if(group_info$rg_type == "Bipolar Reference"){
    s_mean = s_na
  }else{
    r = unique(ref[sel])
    r = str_extract(r, '[0-9,\\-]+')
    r = r[!is.na(r)]
    if(!length(r)){
      s_mean = s_na
    }else{
      s_mean = colMeans(s[ref_tbl$Electrode %in% rave:::parse_selections(r), , drop = F])
    }
  }

  hide_ref = local_data$parallel_plt_refed_hidden; hide_ref %?<-% 'all'


  switch (hide_ref,
          'all' = {
            col2 = rep('grey60', sum(sel))
            s_ref = mat[sel, , drop = F] %*% s

            rave::plot_signals(
              rbind(s_mean, s_ref[!sel_hide, ]),
              sample_rate = srate,
              col = c('black', col2[!sel_hide]),
              space = space,
              space_mode = ifelse(space <= 1, 'quantile', 'abs'),
              channel_names = c('REF', group_info$electrodes[!sel_hide]),
              ylab = 'Electrode', time_shift = start
            ) ->
              re


            rave::plot_signals(
              rbind(s_na, s[sel, , drop = F][!sel_hide, ]),
              sample_rate = srate,
              col = c('black', col[!sel_hide]),
              space = re$space,
              space_mode = 'abs',
              time_shift = start, new_plot = F
            )
          },
          'raw' = {
            rave::plot_signals(
              rbind(s_mean, s[sel, , drop = F][!sel_hide, ]),
              sample_rate = srate,
              col = c('black', col[!sel_hide]),
              space = space,
              space_mode = ifelse(space <= 1, 'quantile', 'abs'),
              channel_names = c('REF', group_info$electrodes[!sel_hide]),
              ylab = 'Electrode', time_shift = start
            )
          },
          'ref' = {
            col2 = rep('grey60', sum(sel))
            s_ref = mat[sel, , drop = F] %*% s

            rave::plot_signals(
              rbind(s_mean, s_ref[!sel_hide, ]),
              sample_rate = srate,
              col = c('black', col2[!sel_hide]),
              space = space,
              space_mode = ifelse(space <= 1, 'quantile', 'abs'),
              channel_names = c('REF', group_info$electrodes[!sel_hide]),
              ylab = 'Electrode', time_shift = start
            ) ->
              re
          }
  )



  # get epoch info
  trials = module_tools$get_meta('trials')
  sel = trials$Block == block
  onset = trials$Time[sel]
  onset = onset[onset %within% c(start, start + duration)]
  offset = trials$Time[sel] + trials$Duration[sel]
  offset = offset[!is.na(offset)]
  offset = offset[offset %within% c(start, start + duration)]

  if(length(onset)){
    abline(v = onset, col = 'green', lty = 2, lwd = 1.5)
  }

  if(length(offset)){
    abline(v = offset, col = 'purple3', lty = 2, lwd = 1.5)
  }
}

output$parallel_plot = renderPlot({
  group_info = current_group()
  ref_tbl = local_data$ref_tbl
  local_data$do_parallel_plot

  validate(need(!is.null(group_info) && is.data.frame(ref_tbl), message = 'Please select a valid group.'))
  try({
    channel_plot()
  }, silent = T)
})

