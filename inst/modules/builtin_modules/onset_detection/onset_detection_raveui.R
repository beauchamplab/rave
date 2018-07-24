

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

    sliderInput('FREQUENCY', 'Frequencies', min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
    sliderInput('BASELINE', 'Baseline Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
    sliderInput('TIME_RANGE', 'Analysis Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

    selectInput('electrode', 'Electrode', choices = '', multiple = F),

    numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1),
    checkboxInput('sort_trials_by_type', 'Sort Trials by Type'),
    checkboxInput('collapse_using_median', 'Use median'),

    #specific to onset detection
    selectInput('od_method', 'Detection Method (currently ignored)', choices = c('Consecutive significance', 'Linear Trend'), multiple = F),
    numericInput('od_alpha', 'Detection Alpha', value = 0.05, min = 0, max=1, step=0.01),
    numericInput('od_window_size', 'Window Size (# time points)', value = 5, min = 1, max=100, step=1),

    .input_panels = list(
        '[#cccccc]Electrode' = list(
            c('electrode')
        ),
        '[#99ccff]Trial Selector' = list(
            c('GROUPS')
        ),
        'Analysis Settings' = list(
            'FREQUENCY',
            'BASELINE',
            'TIME_RANGE'
        ),
        'Detection Settings' = list(
          c('od_method'),
          c('od_alpha', 'od_window_size')
        ),
        '[-]Plotting' = list(
            c('sort_trials_by_type', 'collapse_using_median'),
            c('max_zlim')
        )
    )
    
)


# define the output options
rave_outputs(
    'Activity over time per trial (Collapse over frequency)' = plotOutput('od_by_trial_heat_map', width = 12),
    'Activity over time (Collapse over freq and trial)' = plotOutput('od_over_time_plot', width = 8),
    'Mean Activity during FIRST onset window' = plotOutput('od_trial_scatter_plot', width = 4),
    'Side Message' = textOutput('od_msg_out', width = 2)
    # ,'Async Message' = textOutput('od_async_out', width = 2)
)


# how are the variables updated
rave_updates(
    {
        #make easier names for key variables
        power = module_tools$get_power()
        electrodes = preload_info$electrodes
        frequencies = preload_info$frequencies
        time_points = preload_info$time_points
        trials = preload_info$condition
        epoch_data = module_tools$get_meta('trials')
        
        frange <- c(max(c(min(frequencies), 75)), min(c(max(frequencies), 150)))
    },
    electrode = list(
        choices = electrodes,
        selected = electrodes[1]
    ),
    GROUPS = list(
        initialize = list(
            GROUP = list(
                choices = unique(trials)
            )
        ),
        value = cache_input('GROUPS', list(
            list(
                GROUP = list(trials)
            )
        ))
    ),

    BASELINE = local({
        list(
            min = min(time_points),
            max = max(time_points),
            value = cache_input('BASELINE', c(min(time_points), 0))
        )
    }),
    FREQUENCY = local({
        list(
            min = min(round(frequencies)),
            max = max(round(frequencies)),
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
