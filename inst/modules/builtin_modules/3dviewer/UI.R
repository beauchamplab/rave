
rave_inputs(
  customizedUI('data_picker'),
  customizedUI('data_controls_name'),
  customizedUI('data_controls_details'),
  customizedUI('data_controls_misc'),
  .input_panels = list(
    'Data Import' = list(
      'data_picker'
    ),
    'Controls' = list(
      'data_controls_name',
      'data_controls_details',
      'data_controls_misc'
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

  # Load volt-raw if not exist
  volt = module_tools$get_voltage(force = F, referenced = T)
  if(!is.null(masks[['Voltage Referenced']])){
    rm('Voltage Referenced', envir = masks)
  }
  if(!is.null(volt) && is(volt, 'Tensor')){
    masks[['Voltage Referenced']] = list(
      name = 'Voltage Referenced',
      header = seq_len(volt$dim[2]) * (100 / subject$preprocess_info('srate')),
      body = rutabaga::collapse(volt$data, c(3,2)) / volt$dim[1],
      type = 'animation',
      electrodes = volt$dimnames$Electrode,
      loaded = rep(T, length(volt$dimnames$Electrode)),
      valid = volt$dimnames$Electrode %in% subject$valid_electrodes,
      cached = TRUE
    )
  }


  env$masks = masks
})

rave_outputs(
  '3D Viewer' = threejsr::threejsOutput('viewer', height = '80vh')
)
