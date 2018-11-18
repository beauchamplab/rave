# Package overall config
config = list(

  package_name = '${{PACKAGE}}',

  dependencies = list(
    'cran' = c(
      'devtools',
      'shiny (>= 1.2.0)',
      'rlang (>= 0.3.0)',
      'stringr (>= 1.3.1)',
      'yaml (>= 2.2.0)',
      'stringr'
    ),
    'github' = c(
      'dipterix/rutabaga (>= 0.1.1)',
      'dipterix/threejsr (>= 0.1.20)',
      'beauchamplab/rave@rave-fir'
    )
  ),

  dev_subject = list(
    project_name = 'demo',
    subject_code = 'sub_large',
    electrodes = '14,15',
    epoch = 'Auditory',
    time_range = list(
      pre = 1,
      post = 2
    ),
    reference = 'default',
    frequency_range = NULL,
    data_types = NULL,
    load_brain = T,
    download_url = 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
  ),

  modules = list(
    list(
      module_id = 'module_1',
      module_label = 'Test Package',
      config_name = 'module1.yaml'
    )
  )
)

yaml::write_yaml(config, './inst/rave.yaml', fileEncoding = 'utf-8')



# First module

config = list(
  data_checks = list(
    'power referenced'
  ),

  init_function = 'init_module1',
  main_function = 'main_module1',

  inputs = list(
    'ELECTRODE' = list(
      type = 'selectInput',
      label = 'Electrode: ',
      init = 'init_electrode'
    ),
    'BASELINE_RANGE' = list(
      type = 'sliderInput',
      label = 'Baseline: ',
      init = 'init_baseline',
      value = c(-1,0),
      min = -1,
      max = 1,
      step = 0.01
    ),

    # customizedInput

    'input_customized' = list(
      type = 'customizedInput' # No init since this is a special type of inputs
    ),

    # Compound Inputs
    'GROUP' = list(
      type = 'compoundInput',
      prefix = 'Group',  # Use prefix and label is ignored
      inital_ncomp = 1,
      init = 'init_group',
      components = list(
        'GROUP_NAME' = list(
          type = 'textInput',
          label = 'Group Name: '
        ),
        'CONDITION' = list(
          type = 'selectInput',
          label = 'Condition: ',
          multiple = TRUE
        )
      )
    ),


    # More examples

    'MORE_TEXT' = list(
      type = 'textInput',
      label = 'Text Input: '
    ),
    'MORE_NUMBER' = list(
      type = 'numericInput',
      label = 'Number Input: '
    ),
    'BASELINE_TYPE' = list(
      type = 'radioButtons',
      label = 'Baseline Method: ',
      inline = TRUE,
      choices = c('Percentage', 'Decibel'),
      selected = 'Decibel',
      init = 'init_baseline_type'
    ),


    # Hidden panel
    'HIDDEN_PANEL_NUMBER' = list(
      type = 'numericInput',
      label = 'Number Input: '
    )

  ),

  input_layouts = list(
    'Base Examples' = list(
      'ELECTRODE',
      'BASELINE_RANGE',
      'BASELINE_TYPE'
    ),
    'Compound Example' = list(
      'GROUP'
    ),
    'Customized Example' = list(
      'input_customized'
    ),
    '[#99ccff] More Examples' = list(
      c('MORE_TEXT', 'MORE_NUMBER')
    ),
    '[-] Collapsed Example' = list(
      'HIDDEN_PANEL_NUMBER'
    )
  ),

  outputs = list(
    'output_text_function' = list(
      title = 'textOutput Title',
      type = 'textOutput',
      width = 4L
    ),
    'output_plot_function' = list(
      title = 'plotOutput Title',
      type = 'plotOutput',
      width = 8L
    ),
    'output_print_function' = list(
      title = 'verbatim Title',
      type = 'verbatimTextOutput',
      width = 12L
    ),
    'output_table_function' = list(
      title = 'This is a data table',
      type = 'tableOutput',
      width = 12L
    )
  ),

  output_layouts = list(
    'Tabset One' = list(
      'Multiple Output' = c('output_plot_function', 'output_text_function'),
      'Verbatim Output' = 'output_print_function'
    ),
    width = 12L
  )

)


yaml::write_yaml(config, './inst/module1.yaml', fileEncoding = 'utf-8')
