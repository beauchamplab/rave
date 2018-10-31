# Phase module
# This is a basic phase module that calculates inter-trial coherence
#
#
NULL


# Definte the input variables
rave_inputs(

  # compound Input allows to grow the number of independent conditions
  compoundInput(
    inputId = 'GROUPS',
    label = 'Group',
    components = {
      textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
      selectInput('GROUP', ' ', choices = '', multiple = TRUE)
    }, inital_ncomp = 1
  ),

  sliderInput('FREQUENCY', 'Frequency', min = 1, max = 200, value = c(1,200), step = 1, round = 1),
  customizedUI('preset_freq_ui'),
  sliderInput('TIME_RANGE', 'Analysis', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

  selectInput('electrode', 'Electrode', choices = '', multiple = F),
  numericInput('max_zlim', 'Maximum Plot Value', value = 1, min = 0, step = 1),
  checkboxInput('MERGE_PLOTS', 'Merge Plots', value = FALSE),

  checkboxInput('log_scale', 'Log Freq'),
  checkboxInput('sort_trials_by_type', 'Sort Trials'),

  checkboxInput('collapse_using_median', 'Use median'),

  # .tabsets = list(
  #     'Global Variables' = c(
  #         'GROUPS', 'FREQUENCY', 'BASELINE', 'TIME_RANGE'
  #     )
  # )

  .input_panels = list(
    '[#cccccc]Electrode' = list(
      c('electrode')
    ),
    '[#99ccff]Trial Selector' = list(
      'GROUPS'
    ),
    'Analysis Settings' = list(
      'FREQUENCY',
      'preset_freq_ui',
      'TIME_RANGE'
    ),
    '[-]Plotting' = list(
      c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
      c('max_zlim', 'MERGE_PLOTS')
    )
  )


)

# define the output options
rave_outputs(
  'Inter-trial Phase Coherence' = plotOutput('itpc_plot', width = 12),
  'Average Phase Coherence (Collapse freq)' = plotOutput('itpc_time_plot', width = 12),
  '3D Viewer' = customizedUI('threeD_viewer', width = 12L)
)

# how are the variables updated
rave_updates(
  {
    rave_checks('phase referenced')
    #creating easy variable names
    phase = module_tools$get_phase()
    electrodes = preload_info$electrodes
    frequencies = preload_info$frequencies
    time_points = preload_info$time_points
    trials = preload_info$condition
    epoch_data = module_tools$get_meta('trials')
    epoch_tbl = epoch_data
    freq_tbl = module_tools$get_meta('frequencies')
    phase_max_freq = module_tools$get_sample_rate() / 2

    frange <- c(0, 20)
    # volt <- module_tools$get_voltage(force=TRUE, referenced = TRUE)
  },
  electrode = local({
    e = cache_input('electrode', electrodes[1])
    if(is.character(e)){
      e = as.integer(e)
    }
    if(!e %in% electrodes){
      e = electrodes[1]
    }
    list(
      choices = electrodes,
      selected = e
    )
  }),
  GROUPS = list(
    initialize = list(
      GROUP = list(
        choices = unique(preload_info$condition)
      )
    ),
    value = cache_input('GROUPS', list(
      list(
        GROUP = list(selected = unique(preload_info$condition)),
        GROUP_NAME = list(value = 'All Conditions')
      )
    ))
  ),
  FREQUENCY = local({
    min = min(phase_max_freq - 1, min(round(frequencies)))
    max = min(phase_max_freq, max(round(frequencies)))
    list(
      min = floor(min),
      max = ceiling(max),
      value = cache_input('FREQUENCY', round(frange))
    )
  }),
  TIME_RANGE = local({
    list(
      min = min(time_points),
      max = max(time_points),
      value = cache_input('TIME_RANGE', c(0, max(time_points)))
    )
  })
)



fband = list(
  'Delta' = c(0.5, 3),
  'Theta' = c(4, 7),
  'Alpha' = c(8, 13),
  'Mu' = c(7.5, 12.5),
  'Beta' = c(16, 31),
  'Low Gamma' = c(32, 74),
  'High Gamma' = c(75, 150)
)

preset_freq_ui = function(){
  tagList(
    tags$label('Presets'),
    p(
      tags$label(
        class = 'label label-default bg-red', style = 'display: inline-block;',
        tags$a('Delta', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_1'), href = '#')
      ),
      tags$label(
        class = 'label label-default bg-orange', style = 'display: inline-block;',
        tags$a('Theta', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_2'), href = '#')
      ),
      tags$label(
        class = 'label label-default bg-yellow', style = 'display: inline-block;',
        tags$a('Alpha', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_3'), href = '#')
      ),
      tags$label(
        class = 'label label-default bg-green', style = 'display: inline-block;',
        tags$a('Beta', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_5'), href = '#')
      ),
      tags$label(
        class = 'label label-default bg-blue', style = 'display: inline-block;',
        tags$a('Low Gamma', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_6'), href = '#')
      ),
      tags$label(
        class = 'label label-default bg-purple', style = 'display: inline-block;',
        tags$a('High Gamma', style = 'color:white!important', class = 'action-button',
               id = ns('freq_preset_7'), href = '#')
      )
    )
  )
}


lapply(1:7, function(ii){
  env = environment()
  observeEvent(input[['freq_preset_' %&% ii]], {
    fs = fband[[ii]]
    updateSliderInput(session = session, inputId = 'FREQUENCY', value = fs)
  }, event.env = env, handler.env = env)
})
