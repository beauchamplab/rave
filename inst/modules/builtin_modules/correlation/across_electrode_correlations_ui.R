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
    compoundInput(
        inputId = 'FROM_ELEC_GROUPS',
        label = 'From/Sender Electrodes',
        components = {
            textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Elec Group Name')
            selectInput('GROUP', ' ', choices = '', multiple = TRUE)
        }, inital_ncomp = 1
    ),

    compoundInput(
        inputId = 'TO_ELEC_GROUPS',
        label = 'To/Receiver Electrodes',
        components = {
            textInput('GROUP_NAME', 'Name', value = '', placeholder = 'Elec Group Name')
            selectInput('GROUP', ' ', choices = '', multiple = TRUE)
        }, inital_ncomp = 1
    ),

    selectInput('collapse_method', choices=c('mean', 'median', 'PCA 95%', 'PCA Eigen > 1'),
                label='Choose time/electrode collapse technique'),
    selectInput('connectivity_method', choices=c('Pearson', 'Spearman', 'Granger (not yet)', 'Cannonical Correlation (not yet)'),
                label='Choose connectivity measure'),


    .input_panels = list(
        '[#99ccff]Trial Selector' = list(
            'GROUPS'
        ),
        'From Electrodes' = list(
            c('FROM_ELEC_GROUPS')
        ),
        'To Electrodes' = list(
            c('TO_ELEC_GROUPS')
        ),
        'Analysis Settings' = list(
            'FREQUENCY',
            'BASELINE',
            'TIME_RANGE'
        ),
        '[-]Plotting' = list(
            c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
            c('max_zlim')
        )
    )

)


# define the output options
rave_outputs(
    'Collapsed Time series' = plotOutput('aec_plot_timeseries', width = 12),#, height = '60vh'),
    'Connection between areas' = plotOutput('aec_plot_connections', width = 12),
    'Single Trial Estimates' = plotOutput('aec_plot_trial_means', width = 12),
    'Pairwise Correlations' = plotOutput('aec_plot_trial_means_without_time', width = 12),

    # 'Activity over time (Collapse over freq and trial)' = plotOutput('over_time_plot', width = 8),
    # 'Windowed Comparison (Collapse over time and freq)' = plotOutput('windowed_comparison_plot', width = 4),
    # 'Activity over time per trial (Collapse over frequency)' = plotOutput('by_trial_heat_map', width = 12),
    'Async Message' = textOutput('async_out', width = 4),
    'Side Message' = textOutput('msg_out', width = 8)
)


# how are the variables updated
rave_updates(
    {
        #creating easy variable names
        power = module_tools$get_power(force=TRUE)
        electrodes = preload_info$electrodes
        frequencies = preload_info$frequencies
        time_points = round(preload_info$time_points, 5)
        trials = preload_info$condition
        epoch_data = module_tools$get_meta('trials')

        frange <- c(max(c(min(frequencies), 75)), min(c(max(frequencies), 150)))
        # volt <- module_tools$get_voltage(force=TRUE, referenced = TRUE)
    },
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
