#' Function to download demo data to data repository
#' @param subject demo subject
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

#' Function to download subjects from internet/local
#' @param con an url or local file path
#' @param replace_if_exists Automatically replace current subject if subject files exist (default FALSE)
#' @param temp_dir temp directory to store downloaded zip files and extracted files
#' @param remove_zipfile clear downloaded zip files? if con is local file, this will be forced to FALSE
#' @param subject_settings override subject.yaml see details
#' @examples
#' \dontrun{
#' # Normal example
#' download.file(
#'   'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip',
#'   destfile = "~/rave_data/data-small.zip")
#' download_subject_data(con = "~/rave_data/data-small.zip")
#'
#' # or the following
#' # download_subject_data(
#' # 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
#' # )
#'
#' # rename project to demo_junk
#' download_subject_data(con = "~/rave_data/data-small.zip",
#'            override_project = 'demo_junk')
#'
#' # override settings
#' download_subject_data(
#'   con = "~/rave_data/data-small.zip",
#'   subject_settings = list(
#'     # subject conf
#'     'demo_project/demo_subject' = list(
#'       data_dir = 'data 2/data_dir/demo/sub1',
#'       raw_dir = 'data 2/raw_dir/sub1'
#'     )
#'   )
#' )
#' }
#' @details
#' Each downloaded zip file should have a subject.yaml file indicating default
#' project name, subject code, data directory and raw data directory. (download
#' 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
#' as an example).
#'
#' If you want to override subject settings, you need to implement your own
#' \code{subject_settings}. See examples.
#' @export
download_subject_data <- function(
  con, replace_if_exists = F, override_project = NULL, override_subject = NULL,
  temp_dir = tempdir(), remove_zipfile = TRUE, subject_settings = NULL,
  ...){
  url = con
  # url = 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
  # url = "/var/folders/rh/4bkfl5z50wgbbjd85xvc695c0000gn/T//RtmpmUoaTy/junk_45d3370d10d.zip"

  if(!file.exists(url)){
    # this is not a local file, download

    # First, try to download subject data
    logger('Download from - ', url, level = 'INFO')

    # prepare files
    temp_file = tempfile(pattern = 'junk_', temp_dir, fileext = '.zip')
    # download
    download.file(url, destfile = temp_file, ...)
  }else{
    remove_zipfile = FALSE
    temp_file = url
  }

  extract_dir = file.path(temp_dir, paste(sample(LETTERS, 10), collapse = ''))
  dir.create(extract_dir, recursive = T, showWarnings = F)
  on.exit({
    # clean up
    unlink(extract_dir, recursive = T)
    if(remove_zipfile){
      unlink(temp_file, recursive = T)
    }else{
      logger('Please manually remove zip file by running:\n',
             sprintf('unlink("%s")', temp_file), level = 'INFO')
    }
  })


  # Extract
  logger('Unzip the folder', level = 'INFO')
  unzip(temp_file, exdir = extract_dir, overwrite = T)

  # Check folder
  # look for meta.yaml
  if(is.null(subject_settings)){
    yaml_files = list.files(extract_dir, pattern = 'subjects.yaml$', recursive = T, full.names = T)

    if(length(yaml_files)){
      depth = stringr::str_count(yaml_files, '(/|\\\\)')
      file = yaml_files[which.min(depth)[1]]
      meta = yaml::read_yaml(file)
    }else{
      logger('No subjects.yaml found! Please use "subject_settings" argument to specify subject settings', level = 'FATAL')
    }
  }else{
    meta = subject_settings
  }


  # check data
  for(ii in seq_along(meta)){
    logger('----------------------------', level = 'INFO')
    subject_id = names(meta)[[ii]]
    subject_id %?<-% ''

    # get subject project name and subject code
    s = strsplit(subject_id, '/|\\\\')[[1]]

    s = s[s!='']
    if(length(s) < 2){
      logger('Invalid subject ID - ', subject_id, ' (abort)', level = 'ERROR')
      next()
    }
    if(is.null(override_project)){
      project_name = s[1]
    }else{
      project_name = override_project
    }

    if(is.null(override_subject)){
      subject_code = s[2]
    }else{
      subject_code = override_subject
    }
    logger('Project Name: [', project_name, ']; Subject Code: [', subject_code, '] (checking)', level = 'INFO')



    data_dir = meta[[ii]][['data_dir']]
    raw_dir = meta[[ii]][['raw_dir']]
    if(length(data_dir) == 1 && is.character(data_dir)){
      # try to find dir
      data_dir = file.path(extract_dir, data_dir)
      if(!dir.exists(data_dir)){
        logger('\n\tdata_dir not exists, abort this one\n',
               '\tPlease check existence of data_dir: \n', data_dir, level = 'ERROR')
        next();
      }else{
        logger('\tdata directory found! - \n', data_dir, level = 'INFO')
      }
    }else{
      logger('No "data_dir" in subject settings, abort this one', level = 'ERROR')
      next();
    }


    if(length(raw_dir) == 1 && is.character(raw_dir)){
      # try to find dir
      raw_dir = file.path(extract_dir, raw_dir)
      if(!dir.exists(raw_dir)){
        logger('\n\traw_dir not exists, abort this one\n',
               '\tPlease check existence of raw_dir: \n', raw_dir, level = 'ERROR')
        next;
      }else{
        logger('\traw directory found! - \n', raw_dir, level = 'INFO')
      }
    }else{
      logger('No "raw_dir" in subject settings, abort this one', level = 'ERROR')
      next();
    }


    # Check subject existence
    rave_data_dir = rave_options('data_dir')
    rave_raw_dir = rave_options('raw_data_dir')
    check_existence = function(subject_code){
      has_subject = c(F, F)
      exist_proj = list.dirs(rave_data_dir, full.names = F, recursive = F)
      if(project_name %in% exist_proj){
        # need to check if subject exists
        exist_subs = list.dirs(file.path(rave_data_dir, project_name), full.names = F, recursive = F)
        if(subject_code %in% exist_subs){
          has_subject[2] = T
        }
      }
      if(subject_code %in% list.dirs(rave_raw_dir, full.names = F, recursive = F)){
        has_subject[1] = T
      }
      has_subject
    }


    if(!replace_if_exists && any(check_existence(subject_code))){
      count = 5
      choice = subject_code
      while(count > 0){
        count = count - 1
        logger('\nSubject [', choice, '] already exists. Replace? or enter new subject code here:\n',
               '\t- yes, or Y(y) to overwrite\n',
               '\t- any other characters for new subject code\n',
               '\t- or leave it blank to cancel importing this subject', level = 'WARNING')
        choice = readline(prompt = ':')
        choice = stringr::str_trim(choice)
        if(!stringr::str_detect(stringr::str_to_lower(choice), '^(y$)|(yes$)')){
          # rename
          if(choice == ''){
            logger('Cancel importing ', subject_code, level = 'INFO')
            subject_code = ''
            break
          }
          if(!any(check_existence(choice))){
            logger('Rename subject to ', choice, level = 'INFO')
            logger('Renaming subjects might cause some problems for SUMA', level = 'WARNING')
            subject_code = choice

            break()
          }
        }else{
          logger('Overwrite subject ', subject_code, level = 'INFO')
          break()
        }
      }


    }

    # Now we need to check
    if(subject_code == ''){
      next()
    }

    # importing subject
    logger('Copy files:', level = 'INFO')
    # raw dir
    to_dir = file.path(rave_raw_dir, subject_code)
    dir.create(to_dir, recursive = T, showWarnings = F)
    lapply(list.files(raw_dir, all.files = T, full.names = T, recursive = F), function(d){
      file.copy(d, to_dir, overwrite = T, recursive = T)
    })


    logger('[New raw dir] ', to_dir, level = 'INFO')

    # data dir
    to_dir = file.path(rave_data_dir, project_name, subject_code)
    dir.create(to_dir, recursive = T, showWarnings = F)
    lapply(list.files(data_dir, all.files = T, full.names = T, recursive = F), function(d){
      file.copy(d, to_dir, overwrite = T, recursive = T)
    })

    logger('[New data dir] ', to_dir, level = 'INFO')

    logger('\n\t[', project_name, '/', subject_code, '] Done.\n', level = 'INFO')
  }

  logger('\n----------------------------', level = 'INFO')

}


# download_subject_data(con = "~/rave_data/data-small.zip",
# override_project = 'demo_junk', replace_if_exists = F)
