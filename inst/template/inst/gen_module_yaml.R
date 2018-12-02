# Script to generate module.yaml
# First module

config = list(
  
  # power/phase/voltage + referenced/raw
  data_checks = list(
    'power referenced'
  ),

  init_function = '__init__${{MODULEID}}',
  main_function = '__main__${{MODULEID}}',
  reactive_function = '__reactive__${{MODULEID}}',

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


yaml::write_yaml(config, './inst/module.yaml', fileEncoding = 'utf-8')
