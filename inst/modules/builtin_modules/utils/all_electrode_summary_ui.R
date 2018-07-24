# Definte the input variables
rave_inputs(

    # compound Input allows to grow the number of independent conditions
    compoundInput(
        inputId = 'GROUPS',
        label = 'Trials',
        components = {
            textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Condition Name')
            selectInput('GROUP', ' ', choices = '', multiple = TRUE)
        }, inital_ncomp = 1
    ),

    sliderInput('FREQUENCY', 'Frequencies', min = 1, max = 200, value = c(1,200), step = 1, round = TRUE),
    sliderInput('BASELINE', 'Baseline Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
    sliderInput('TIME_RANGE', 'Analysis Range', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),


    # --- Local variables ---
    selectInput('collapse_method', choices=c('mean', 'median', 'PCA 95%', 'PCA Eigen > 1'),
                label='Choose time/electrode collapse technique'),
    selectInput('connectivity_method', choices=c('Pearson', 'Spearman', 'Granger (not yet)', 'Cannonical Correlation (not yet)'),
                label='Choose connectivity measure'),

    checkboxInput('sort_trials_by_type', 'Sort Trials by Type'),

    numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1),
    .tabsets = list(
        'Global Variables' = c(
            'GROUPS', 'FREQUENCY', 'BASELINE', 'TIME_RANGE'
        )
    )
)


# define the output options
rave_outputs(
    'Side Message' = textOutput('msg_out', width = 4),

    'Collapsed Time series' = plotOutput('aec_plot_timeseries', width = 12),
    'Connection between areas' = plotOutput('aec_plot_connections', width = 12),
    'Single Trial Estimates' = plotOutput('aec_plot_trial_means', width = 12),


    # 'Activity over time (Collapse over freq and trial)' = plotOutput('over_time_plot', width = 8),
    # 'Windowed Comparison (Collapse over time and freq)' = plotOutput('windowed_comparison_plot', width = 4),
    # 'Activity over time per trial (Collapse over frequency)' = plotOutput('by_trial_heat_map', width = 12),
    'Async Message' = textOutput('async_out', width = 4)
)


# how are the variables updated
rave_updates(
    GROUPS = list(
        initialize = list(
            GROUP = list(
                choices = unique(trials)
            )
        ),
        value = cache_input('GROUPS', list(
            list(
                GROUP = unique(trials)
            )
        ))
    ),
    FROM_ELEC_GROUPS = list(
        initialize = list(
            GROUP = list(
                choices = electrodes
            )
        )
    ),
    TO_ELEC_GROUPS = list(
        initialize = list(
            GROUP = list(
                choices = electrodes
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
