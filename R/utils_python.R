# function to call python using rave, reticulate

#' @export
py_source <- function(con, ..., workdir = tempdir(), file_only = FALSE,
                      py3 = TRUE, virtualenv = rave_options('py_virtualenv'),
                      convert = T){
  # find connection to a py file
  if(file_only || (length(con) == 1 && file.exists(con))){
    ..file_path = con
  }else{
    ..file_path = file.path(workdir, '__rave_tmp.py')
    writeLines(con, ..file_path)
  }

  # get compiler
  compiler_path = rave_options(ifelse(py3, 'py3_path', 'py2_path'))
  reticulate::use_python(compiler_path)
  if(length(virtualenv) == 1 && virtualenv %in% reticulate::virtualenv_list()){
    reticulate::use_virtualenv(virtualenv)
  }

  source_python(..file_path, convert = convert, envir = environment())

  return(environment())
}


#' Launch python console within R
#'
#' @export
py_console <- function(compiler_path = '', virtualenv = rave_options('py_virtualenv'), py3 = TRUE, ...){
  if(!missing(compiler_path)){
    version = system2(compiler_path, "--version", stdout = TRUE, stderr = TRUE)
    error_status <- attr(version, "status")
    if (!is.null(error_status))
      stop("Error ", error_status, " occurred while checking for python version",
           call. = FALSE)
    matches <- regexec("^[^ ]+\\s+(\\d+)\\.(\\d+).*$", version)
    matches <- regmatches(version, matches)[[1]]
    if (length(matches) != 3)
      stop("Unable to parse Python version '", version[[1]],
           "'", call. = FALSE)
    py3 = matches[[2]] == '3'
  }else{
    if(py3){
      compiler_path = rave_options('py3_path')
    }else{
      compiler_path = rave_options('py2_path')
    }
  }
  logger('Launching python - ', compiler_path, level = 'INFO')

  if(compiler_path != Sys.which('python')){
    reticulate::use_python(compiler_path)
  }

  if(length(virtualenv) == 1 && virtualenv %in% reticulate::virtualenv_list()){
    logger('Using virtualenv - ', virtualenv, level = 'INFO')
    reticulate::use_virtualenv(virtualenv)
  }

  reticulate::repl_python(...)
}

#' @export
py_save <- function(..., file){
  names = as.character(substitute(list(...)))[-1L]
  args = list(...)
  if(length(names) > 1){
    names(args) = names
  }else{
    args = args[[1]]
  }
  reticulate::py_save_object(args, file)
}


