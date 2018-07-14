
rave_inputs(
  colourInput('bgcolor', label = 'Background Color', palette = 'limited', returnName = F),
  selectInput('mouse_control', label = 'Mouse Control', choices = c('trackball', 'orbit'), selected = 'trackball'),
  customizedUI('data_picker'),
  customizedUI('data_controls_name'),
  customizedUI('data_controls_details'),
  .input_panels = list(
    'Data Import' = list(
      'data_picker'
    ),
    'Controls' = list(
      'data_controls_name',
      'data_controls_details'
    ),
    'Graphic Settings' = list(
      'bgcolor',
      'mouse_control'
    )
  )
)

rave_updates({
  masks = module_tools$get_subject_data(
    name = 'file_list',
    path = .module_path,
    check_cache = T,
    default = list(),
    try_open = T
  )
  env$masks = masks
})

rave_outputs(
  '3D Viewer' = threejsr::threejsOutput('viewer', height = '80vh')
)
