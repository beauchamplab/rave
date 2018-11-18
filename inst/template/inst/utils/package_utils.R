package_utils = function(package){
  force(package)

  package_installed <- function(p, version = NULL, default_if_error = FALSE){
    if(p %in% installed.packages()[,1]){
      if(is.null(version)){
        return(TRUE)
      }

      re = tryCatch({
        v1 = as.character(utils::packageVersion(p))
        utils::compareVersion(v1, version) >= 0
      },
      error = function(e){ default_if_error },
      warning = function(e){ default_if_error })

      return(re)
    }
    return(FALSE)
  }

  # ------------------ Check load packages ------------------
  check_load_packages <- function(cran = NULL, github = NULL){
    pkgs = NULL
    # Check cran first
    if(length(cran)){
      tmp = strsplit(cran, '[ \\(\\)>=]+')
      for(p in tmp){
        pkgs = c(pkgs, p[1])
        installed = FALSE
        if(length(p) == 1){
          installed = package_installed(p[1])
        }else{
          installed = package_installed(p[1], version = p[2])
        }
        if(!installed){
          install.packages(p[1])
        }
      }
    }

    # Check github next
    if(length(github)){
      tmp = strsplit(github, '[ \\(\\)>=/@]+')
      for(ii in seq_along(github)){
        p = tmp[[ii]]
        if(length(p) == 1){
          stop('Invalid github repository name: ', p, ' (should be "user/repo@branch (>= version)")')
        }
        if(grepl('@', github[[ii]])){
          # branch
          branch = paste0('@', p[3])
          p = p[-3]
        }else{
          branch = ''
        }

        pkgs = c(pkgs, p[2])

        installed = FALSE
        # If package is installed
        if(length(p) == 2){
          installed = package_installed(p[2])
        }else{
          installed = package_installed(p[2], version = p[3])
        }

        if(!installed){
          # devtools install
          devtools::install_github(sprintf('%s/%s%s', p[1], p[2], branch))
        }

      }
    }

    for(p in pkgs){
      do.call('library', list(
        package = p, character.only = T, warn.conflicts = F
      ))
    }
  }

  # Need to load these packages first
  check_load_packages(cran = c('yaml', 'devtools', 'rstudioapi'))



  # ------------------ Function to load rave.yaml ------------------
  load_rave_yaml <- function(yaml_path){
    if(missing(yaml_path)){
      fpath = system.file('rave.yaml', package = package)
    }else{
      fpath = yaml_path
    }
    yaml::read_yaml(fpath)
  }



  # ------------------ Function to load rave.yaml ------------------
  load_demo_subject <- function(
    project_name = 'demo', subject_code = 'sub_large', electrodes = NULL,
    epoch = 'Auditory', time_range = c(1,2), reference = 'default',
    frequency_range = NULL, data_types = NULL, load_brain = T,
    download_url = NULL){

    logger_level = rave_options('logger_level')
    rave_options('logger_level' = 'INFO')
    on.exit({
      rave_options('logger_level' = logger_level)
    })

    # Load & prepare demo subject
    checks = rave:::check_subjects2(project_name = project_name, subject_code = subject_code)

    if(!all(unlist(checks$check))){
      if(!is.null(download_url) || (project_name == 'demo' && subject_code == 'sub_large')){
        if(is.null(download_url)){
          download_url = 'https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip'
        }
        cat2('This function requires downloading subject. Download? [y/N]', level = 'WARNING')
        v = readline(prompt = 'y or N: ')
        v = stringr::str_to_lower(v)
        if(!v %in% c('y', 'n')){
          stop('Please enter "y/Y" or "N"')
        }else if (v == 'y'){
          # download demo subject
          download_subject_data(download_url, override_project = project_name, override_subject = subject_code)
        }else{
          stop('Action aborted.')
        }
      }else{
        stop(sprintf('%s/%s is invalid subject.', project_name, subject_code))
      }
    }

    subject = Subject$new(project_name = project_name, subject_code = subject_code, reference = reference)

    if(!epoch %in% checks$epochs){
      epoch = checks$epochs[1]
    }

    if(! reference %in% checks$references){
      reference = checks$references[1]
    }

    logger('Loading subject. Please wait...', level = 'INFO')

    rave::rave_prepare(
      subject = subject, electrodes = subject$valid_electrodes, epoch = epoch,
      time_range = time_range, reference = reference, frequency_range = frequency_range,
      data_types = data_types, load_brain = load_brain, attach = T)


  }

  return(environment())
}
