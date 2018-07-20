.module_path = 'Viewer3D'
.module_id = 'viewer_3d'
.preserved = c('Voltage, Referenced', 'Power, Referenced', 'Phase, Raw')



rave_inputs(
  customizedUI('data_picker'),
  customizedUI('data_controls_name'),
  customizedUI('data_controls_import'),
  customizedUI('data_controls_details'),
  customizedUI('data_controls_misc'),
  .input_panels = list(
    'Dataset' = list(
      'data_controls_name',
      'data_controls_import'
    ),
    'Controls' = list(
      'data_controls_details',
      'data_controls_misc'
    ),
    '[-] Data Import' = list(
      'data_picker'
    )
  )
)

rave_updates({
  masks = module_tools$get_subject_data(
    name = 'file_list',
    path = .module_path,
    check_cache = T,
    default = new.env(parent = baseenv()),
    try_open = T
  )
  if(!is.environment(masks)){
    masks = new.env(parent = baseenv())
  }

  for(p in .preserved){
    masks[[p]] = NULL
  }
  env$masks = masks
})

rave_outputs(
  '3D Viewer' = threejsr::threejsOutput('viewer', height = '80vh')
)
