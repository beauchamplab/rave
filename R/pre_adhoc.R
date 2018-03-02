pre_meta <- function(project_name, subject_code, blocks, channel_names, frequencies, srate = 100){
  data_dir = rave_opts$get_options('data_dir')
  meta_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'meta')
  cache_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name), 'rave', 'cache')
  suma_dir = file.path(data_dir, sprintf('%s_%s', subject_code, project_name),
                       rave_opts$get_options('suma_export_dir'), 'rave')

  if(!dir.exists(meta_dir)){
    dir.create(meta_dir, recursive = T)
  }
  if(!dir.exists(suma_dir)){
    dir.create(suma_dir, recursive = T)
  }

  # generate frequency file
  f = file.path(meta_dir, 'frequencies.csv')
  dat = data.frame(
    Index = 1:length(frequencies),
    Frequency = frequencies
  )
  write.csv(dat, f, row.names = F)

  # generate time data
  # pick the first channel file and extract block time.
  channel_files = list.files(cache_dir)

  # generate event data
  file.path(
    rave_opts$get_options('raw_data_dir'), 'neuralData', 'originalData',
    subject_code, block_num, sprintf(
      '%sDatafile%s_ch%d.mat', subject_code, block_num, chl
    )
  ) %>%
    R.matlab::readMat() ->
    dat
}
