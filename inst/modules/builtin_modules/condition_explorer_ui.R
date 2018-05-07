# Definte the input variables
rave_inputs(

    # compound Input allows to grow the number of independent conditions
    compoundInput(
        inputId = 'GROUPS_CMPD',
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
    'Heat Map (Collapse over Trials)' = plotOutput('heat_map_plot', width = 12),
    'Activity over time (Collapse over freq and trial)' = plotOutput('over_time_plot', width = 8),
    'Windowed Comparison (Collapse over time and freq)' = plotOutput('windowed_comparison_plot', width = 4),
    'Activity over time per trial (Collapse over frequency)' = plotOutput('by_trial_heat_map', width = 12),
    'Side Message' = textOutput('msg_out', width = 4),
    'Async Message' = textOutput('async_out', width = 4)
)


# how are the variables updated
rave_updates(
    electrode = list(
        choices = power$dimnames$Electrode,
        selected = power$dimnames$Electrode[1]
    ),
    GROUPS_CMPD = local({
      tools = module_tools
      trials = tools$get_meta('trials')
      cond = unique(trials$Condition)

      list(
        initialize = list(
          GROUP = list(
            choices = cond
          )
        ),
        value = cache_input('GROUPS_CMPD', list(
          list(
            GROUP = cond[1]
          )
        ))
      )
    }),

    BASELINE = local({
      time_points = power$dimnames$Time

        list(
            min = min(time_points),
            max = max(time_points),
            value = cache_input('BASELINE', c(min(time_points), 0))
        )
    }),
    FREQUENCY = local({
      frequencies = power$dimnames$Frequency
        list(
            min = min(round(frequencies)),
            max = max(round(frequencies)),
            value = cache_input('FREQUENCY', range(round(frequencies)))
        )
    }),
    TIME_RANGE = local({
      time_points = power$dimnames$Time
        list(
            min = min(time_points),
            max = max(time_points),
            value = cache_input('TIME_RANGE', c(0, max(time_points)))
        )
    })
)
