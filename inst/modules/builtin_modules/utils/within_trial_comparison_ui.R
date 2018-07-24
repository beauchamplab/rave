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

    sliderInput('FREQUENCY', 'Frequencies',
                min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
    sliderInput('BASELINE', 'Baseline Range',
                min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
    # sliderInput('TIME_RANGE', 'Analysis Range',
    #             min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

    compoundInput(
        inputId = 'TIMES',
        label = 'Analysis Windows',
        components = {
            textInput('TIME_NAME', 'Name', value = '', placeholder = 'Name')
            sliderInput('RANGE', 'Window', min=0, max=1, value=c(0,0), step=0.01,
                        round=-2)
        }, inital_ncomp = 2
    ),

    selectInput('electrode', 'Electrode', choices = '', multiple = F),
    numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1),

    checkboxInput('sort_trials_by_type', 'Sort Trials by Type'),
    checkboxInput('collapse_using_median', 'Collapse using median'),

    .input_panels = list(
        '[#cccccc]Electrode' = list(
            c('electrode')
        ),
        '[#99ccff]Trial Selector' = list(
            'GROUPS'
        ),
        'Analysis Settings' = list(
            'FREQUENCY',
            'BASELINE'
        ),
        '[-]Plotting' = list(
            c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
            c('max_zlim')
        )
    )
)


# define the output options
rave_outputs(
    'Activity over time (Collapse over freq and trial)' = plotOutput('wtc_over_time_plot', width = 12),
    'Fixed Effect Comparison' = plotOutput('summary_effect_plot', width = 6),
    'Fixed Effect Comparison (per trial power)' = plotOutput('fixed_effects_plot', width = 6),
    'LME Output' = htmlOutput('msg_out', width = 12),
    'Random Effects Structure' = plotOutput('random_effects_plot', width=10),
    'Async Message' = textOutput('async_out', width = 2)
)


# how are the variables updated
rave_updates(
    {
        #creating easy variable names
        power = module_tools$get_power()
        electrodes = preload_info$electrodes
        frequencies = preload_info$frequencies
        time_points = preload_info$time_points
        trials = preload_info$condition
        epoch_data = module_tools$get_meta('trials')

        frange <- c(max(c(min(frequencies), 75)), min(c(max(frequencies), 150)))
    },
    electrode = list(
        choices = module_tools$get_loaded_electrodes(),
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
                GROUP = list(trials),
                GROUP_NAME = 'All Conditions'
            )
        ))
    ),
    TIMES = list(
        initialize=list(
            RANGE = list(
                min = min(time_points),
                max = max(time_points),
                value = c(0,0)
            )
        )
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
            value = cache_input('FREQUENCY', range(round(frange)))
        )
    })
)
