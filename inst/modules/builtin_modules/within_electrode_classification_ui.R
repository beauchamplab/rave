# Definte the input variables
rave_inputs(

    # compound Input allows to grow the number of independent conditions
    compoundInput(
        inputId = 'GROUPS',
        label = 'Group',
        components = {
            textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Name')
            selectInput('GROUP', ' ', choices = '', multiple = TRUE)
        }, inital_ncomp = 2
    ),

    selectInput('novel_generalization', 'Generalization Class', choices='', multiple=TRUE),

    sliderInput('FREQUENCY', 'Frequencies', min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
    sliderInput('BASELINE', 'Baseline Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
    sliderInput('TIME_RANGE', 'Analysis Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

    selectInput('electrode', 'Electrode', choices = '', multiple = F),
    numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1),

    checkboxInput('log_scale', 'Show Log Frequency'),
    checkboxInput('sort_trials_by_type', 'Sort Trials by Type'),

    checkboxInput('collapse_using_median', 'Collapse using median'),

    .tabsets = list(
        'Global Variables' = c(
            'GROUPS', 'FREQUENCY', 'BASELINE', 'TIME_RANGE'
        )
    )
)


# define the output options
rave_outputs(
    'Activity over time per trial (Collapse over frequency)' = plotOutput('wec_by_trial_heat_map', width = 12),
    'Side Message' = textOutput('msg_out', width = 4),
    'Async Message' = textOutput('async_out', width = 4)
)


# how are the variables updated
rave_updates(
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
                GROUP = trials[1]
            )
        ))
    ),
    novel_generalization = list(
        choices=unique(trials)
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
            value = cache_input('FREQUENCY', range(round(frequencies)))
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
