#' Function to download demo data to data repository
#' @param subject demo subject
download_sample_data <- function(subject = 'KC', ...){
  project_name = 'demo'
  data_dir = rave_options('data_dir')
  raw_dir = rave_options('raw_data_dir')
  tmp_dir = tempdir()
  url = sprintf('https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/demo_%s.zip', subject)
  tmp_zip = file.path(tmp_dir, 'downloaded_tmp.zip')
  
  download_subject_data(url, ...)

}

#' Function to download subjects from internet/local
#' @param con an url or local file path
#' @param replace_if_exists Automatically replace current subject if subject files exist (default FALSE)
#' @param temp_dir temp directory to store downloaded zip files and extracted files
#' @param remove_zipfile clear downloaded zip files? if con is local file, this will be forced to FALSE
#' @param subject_settings override subject.yaml see details
#' @param override_project if not null, project will be renamed to this value
#' @param override_subject if not null, subject will be renamed to this value
#' @param ... passed to download.file
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



archive_subject <- function(project_name, subject_code, 
                            include_cache = FALSE, include_fs = TRUE, include_raw = FALSE,
                            save_to = tempdir()){
  subject = Subject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
  root_dir = file.path(tempdir(check = TRUE), 'archive')
  rave_dir = file.path(root_dir, 'data_dir', project_name, subject_code, 'rave')
  raw_dir = file.path(root_dir, 'raw_dir', subject_code)
  wd = getwd()
  on.exit({
    unlink(root_dir, recursive = TRUE, force = FALSE)
    setwd(wd)
  })
  
  # setwd(subject$dirs$rave_dir)
  dir.create(rave_dir, recursive = TRUE, showWarnings = TRUE)
  dir.create(raw_dir, recursive = TRUE, showWarnings = TRUE)
  
  dirs = list.dirs(subject$dirs$rave_dir, full.names = FALSE, recursive = TRUE)
  
  dirs = dirs[!stringr::str_detect(dirs, '^fs(/|$)')]
  
  sapply(file.path(rave_dir, dirs), function(d){
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  })
  
  paths = c(
    'data/cache/cached_reference.csv',
    'data/power/',
    'data/phase/',
    'data/reference/',
    'data/voltage/',
    'log.yaml',
    'meta/',
    'preprocess/'
  )
  if(include_cache){
    paths[1] = 'data/cache/'
  }
  
  from_paths = file.path(subject$dirs$rave_dir, paths)
  sel = file.exists(from_paths)
  from_paths = from_paths[sel]
  to_paths = file.path(rave_dir, paths)[sel]
  
  apply(cbind(from_paths, to_paths), 1, function(x){
    x[2] = dirname(x[2])
    dir.create(x[2], showWarnings = FALSE, recursive = TRUE)
    print(x[2])
    if(file.info(x[1])[['isdir']]){
      file.copy(x[1], x[2], overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE, recursive = TRUE)
    }else{
      file.copy(x[1], x[2], overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
    }
  })
  
  if(include_fs){
    fs_paths = file.path(subject$dirs$rave_dir, c('fs', '../fs'))
    sel = dir.exists(fs_paths)
    if(any(sel)){
      fs_paths = normalizePath(fs_paths[sel][1])
      target_fs = normalizePath(file.path(rave_dir, '..'))
      file.copy(fs_paths, target_fs, overwrite = TRUE, recursive = TRUE, copy.mode = FALSE, copy.date = TRUE)
    }
    if(!include_cache){
      unlink(file.path(rave_dir, '..', 'fs', 'RAVE'), recursive = TRUE)
    }
  }
  
  if(include_raw){
    raws = list.dirs(subject$dirs$pre_subject_dir, full.names = TRUE, recursive = FALSE)
    lapply(raws, function(f){
      file.copy(f, raw_dir, recursive = TRUE, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
    })
  }
  
  # yaml file
  conf = list(list(
    data_dir = sprintf('data_dir/%s/%s', project_name, subject_code),
    raw_dir = sprintf('raw_dir/%s', subject_code)
  ))
  names(conf) = subject$id
  
  yaml::write_yaml(conf, file = file.path(root_dir, 'subjects.yaml'), fileEncoding = 'utf-8')
  dir.create(save_to, showWarnings = FALSE, recursive = TRUE)
  save_to = file.path(normalizePath(save_to), sprintf('%s_%s.zip', project_name, subject_code))
  setwd(root_dir)
  utils::zip(zipfile = save_to, files = '.')
  setwd(wd)
  
  cat2('Please check zip file at ', save_to, level = 'INFO')
}
