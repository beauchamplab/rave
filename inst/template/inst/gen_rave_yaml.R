# Script to generate rave.yaml

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
      module_id = '${{MODULEID}}',
      module_label = '${{MODULELABEL}}',
      config_name = 'module.yaml'
    )
  )
)

yaml::write_yaml(config, './inst/rave.yaml', fileEncoding = 'utf-8')
