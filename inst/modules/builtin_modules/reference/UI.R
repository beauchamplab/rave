# IO

rave_inputs(
  # Overall settings
  selectInput('ref_name_alt', 'Import From', choices = 'new..', selected = 'new..'),
  compoundInput('ref_group', label = 'Reference Group', max_ncomp = 20, components = {
    textInput('rg_name', 'Name', value = '')
    selectInput('rg_type', 'Type', choices = c('Common Average Reference', 'Bipolar Reference', 'White Matter Reference', 'No Reference'), selected = 'No Reference')
    textInput('rg_electrodes', 'Electrodes', value = '', placeholder = 'e.g. 1-12,14')
  }),
  # selectInput('ref_group', label = 'Reference Group', choices = '', selected = '', multiple = F),
  # textInput('ref_group_electrodes', label = 'Electrodes', value = '', placeholder = 'e.g. 1-3,5'),

  # Group inspection
  selectInput('cur_group', 'Group Number', choices = 1:20, selected = NULL),
  customizedUI('cur_group_ui'),
  actionButton('cur_group_save', 'Preview & Export',width = '100%'),


  # generator
  customizedUI('ref_generator_ui'),

  # Groups


  .input_panels = list(
    'Overall' = list(
      'ref_name',
      'ref_name_alt',
      'ref_group'
    ),
    '[-] Group Inspection' = list(
      'cur_group',
      'cur_group_ui',
      'cur_group_save'
    ),
    '[-] Reference Generator' = list(
      'ref_generator_ui'
    )
  )
)

rave_updates(
  ref_name_alt = local({
    # clean all caches
    nms = ls(env, all.names = T)
    if(length(nms)){
      rm(list = nms, envir = env)
    }

    # Trick 1, update all information at the first component
    env$dirs = module_tools$get_subject_dirs()
    env$existing_refs = list.files(env$dirs$meta_dir, pattern = '^reference_.*\\.[cC][sS][vV]$')
    env$ref_dir = file.path(env$dirs$channel_dir, 'reference')
    env$last_import = 'new..'
    choices = unique(c('new..', env$existing_refs))

    list(
      choices = choices,
      selected = 'new..'
    )
  })
)


rave_outputs(
  'Voltage Plot' = customizedUI('parallel_plot_ui'),
  'Electrode Statistics' = customizedUI('electrode_plot_ui'),
  .output_tabsets = list(
    width = 12L,
    # Tabset name
    'Visualization' = list(
      # Tab name 1
      'Group Inspection' = list(
        'parallel_plot_ui'
      ),

      # Tab name 2
      'Electrode Inspection' = list(
        'electrode_plot_ui'
      )
    )
  )
)
