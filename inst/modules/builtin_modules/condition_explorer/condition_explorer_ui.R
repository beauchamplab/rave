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
    sliderInput('BASELINE', 'Baseline', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),
    sliderInput('TIME_RANGE', 'Analysis', min = 0, max = 1, value = c(0,1), step = 0.01, round = -2),

    # selectInput('electrode', 'Electrode', choices = '', multiple = F),
    textInput('electrode_text', 'Electrodes', value = "", placeholder = '1-5,8,11-20'),

    selectInput('combine_method', 'Electrode Transforms',
                choices = c('none', 'z-score', 'max-scale', '0-1 scale', 'rank'), multiple = F, selected = 'none'),

    selectInput('reference_type', 'Transform Reference',
                choices = c('Trial', 'Trial type', 'Active trial types', 'All trials'), selected='Trial'),

    selectInput('reference_group', 'Reference Group',
                choices = c('Per Electrode', 'All Electrodes'), selected = 'Per Electrode'),

    numericInput('max_zlim', 'Maximum Plot Value', value = 0, min = 0, step = 1),

    checkboxInput('log_scale', 'Log Freq'),
    checkboxInput('sort_trials_by_type', 'Sort Trials'),

    checkboxInput('collapse_using_median', 'Use median (BRKN)'),

    # .tabsets = list(
    #     'Global Variables' = c(
    #         'GROUPS', 'FREQUENCY', 'BASELINE', 'TIME_RANGE'
    #     )
    # )

    .input_panels = list(
        '[#cccccc]Electrodes' = list(
            c('electrode_text'),
            c('combine_method'),
            c('reference_type', 'reference_group')
        ),
        '[#99ccff]Trial Selector' = list(
            'GROUPS'
        ),
        'Analysis Settings' = list(
            'FREQUENCY',
            'BASELINE',
            'TIME_RANGE'
        ),
        '[-][#33aaff]Export Options' = list(),
        '[-]Plotting' = list(
            c('log_scale', 'sort_trials_by_type', 'collapse_using_median'),
            c('max_zlim')
        )
    )


)

# define the output options
rave_outputs(
    'Heat Map (Collapse trial)' = plotOutput('heat_map_plot', width = 12, brush = 'brushed'),
    'Activity over time by trial (Collapse freq)' = plotOutput('by_trial_heat_map', width = 12),
    'Activity over time (Collapse freq + trial)' = plotOutput('over_time_plot', width = 8),
    'Windowed Comparison (Collapse time + freq)' = plotOutput('windowed_comparison_plot', width = 4),
    # 'Side Message' = textOutput('msg_out', width = 4),
    # 'Async Message' = textOutput('async_out', width = 4),
    '3D Viewer' = customizedUI('viewer_3d')
)

# how are the variables updated
rave_updates(
    {
        # Check if power is loaded
        rave_checks('power referenced')
        #creating easy variable names
        power = module_tools$get_power()
        electrodes = preload_info$electrodes
        frequencies = preload_info$frequencies
        time_points = preload_info$time_points
        trials = preload_info$condition
        epoch_data = module_tools$get_meta('trials')

        frange <- c(max(c(min(frequencies), 75)), min(c(max(frequencies), 150)))
    },
    electrode_text = local({
      txt = deparse_selections(electrodes)
      e = cache_input('electrode_text', NULL)

      if(!is.character(e) || !length(e)){
        e = electrodes[1]
      }else{
        e = parse_selections(e)
        e = e[e %in% electrodes]
        if(!length(e)){
          e = electrodes[1]
        }else{
          e = deparse_selections(e)
        }
      }
      list(
        value = e,
        label = 'Electrodes (' %&% txt %&% ')'
      )
    }),
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


