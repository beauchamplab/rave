#' Function to download demo data to data repository
#' @param subject demo subject
#' @export
download_sample_data <- function(subject = 'sub1'){
  project_name = 'demo'
  data_dir = rave_options('data_dir')
  raw_dir = rave_options('raw_data_dir')
  tmp_dir = tempdir()
  switch (subject,
    'sub1' = {
      # sub1 is at github
      # link addr: https://github.com/dipterix/rave_example_data/archive/master.zip
      url = 'https://github.com/dipterix/rave_example_data/archive/master.zip'
      tmp_zip = file.path(tmp_dir, 'downloaded_sub1.zip')
      extract_dir = file.path(tmp_dir, 'extract_dir')

      download.file(url = url, destfile = tmp_zip)
      logger('Expanding zip file', level = 'INFO')

      unzip(tmp_zip, overwrite = T, exdir = extract_dir)
      sub_data_dir = file.path(extract_dir, 'rave_example_data-master', 'data', 'data_dir', 'demo')
      sub_raw_dir = file.path(extract_dir, 'rave_example_data-master', 'data', 'raw_dir', 'sub1')

      # copy files
      logger('Copy from tempdir to data repository', level = 'INFO')
      file.copy(sub_data_dir, data_dir, overwrite = T, recursive = T)
      file.copy(sub_raw_dir, raw_dir, overwrite = T, recursive = T)

      # clean up
      logger('Clean up', level = 'INFO')
      unlink(extract_dir, recursive = T)
      unlink(tmp_zip)
      logger('Done. Subject [sub1] is now at \n', '[Raw Data]: ',
             file.path(raw_dir, 'sub1'), '\n[RAVE Data]: ',
             file.path(raw_dir, 'demo'), level = 'INFO')
    },
    {
      stop('There is only one sample subject [sub1] right now.')
    }
  )


}
