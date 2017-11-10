



#' @export
load_module <- function(
  module_id,
  source_path,
  category = 'Default',
  label = 'Default Module',
  packages = '',
  author = '',
  version = '0.0.0',
  is_univariate = TRUE,
  suma_enabled = FALSE,
  id_only = TRUE,
  ...
){

  if(id_only){
    module = ModuleEnv$new(
      module_id = stringr::str_trim(module_id),
      label = stringr::str_trim(label),
      category = stringr::str_trim(category),
      is_univariate = as.logical(is_univariate),
      source_path = stringr::str_trim(source_path),
      packages = stringr::str_trim(stringr::str_split(packages, '\\,', simplify = T)),
      author = author,
      version = version,
      suma_enabled = suma_enabled,
      session = NULL,
      ...
    )
  }else{
    module = ModuleEnv$new(
      module_id = stringr::str_trim(module_id),
      label = stringr::str_trim(label),
      category = stringr::str_trim(category),
      is_univariate = as.logical(is_univariate),
      source_path = stringr::str_trim(source_path),
      packages = stringr::str_trim(stringr::str_split(packages, '\\,', simplify = T)),
      author = author,
      version = version,
      suma_enabled = suma_enabled,
      ...
    )
  }

}

#' @export
load_modules_from_file <- function(
  csv_path = rave_opts$get_options('module_lookup_file'),
  id_only = FALSE # Only get IDs for module, not init
){
  module_list <- read.csv(csv_path, stringsAsFactors = F)
  module_list <- module_list[module_list$active, ]

  for(i in 1:nrow(module_list)){
    args <- as.list(module_list[i, ])
    if(stringr::str_detect(args$source_path, '^system\\.file\\(.+\\)')){
      args$source_path <- eval(parse(text = args$source_path))
    }
    args$id_only = id_only
    m <- do.call('load_module', args = args)
  }
  return(module_list$module_id)
}

exprToString <- function(expr, sep = '\n'){
  deparse(expr) %>%
    paste(collapse = sep)
}

export_module <- function(subject, module, params){
  if(!"ModuleEnv" %in% class(module)){
    logger('module object must be a ModuleEnv object', level = 'WARNING')
  }else{
    now <- Sys.time()
    path <- file.path(rave_opts$get_options('export_path'), module$label)
    script_name <- sprintf('%s%s.R', format(now, '[%Y%m%d-%H%M%S]'), module$id)
    script_path <- file.path(path, script_name)
    if(!file.exists(path) || !file.info(path)['isdir']){
      dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }
    # prepare scripts
    args <- list()
    args$work_dir <- sprintf(
      'setwd(%s)',
      exprToString(getwd())
    )
    args$module_info <- sprintf(
      paste(
        '# Module: %s',
        '# Author: %s',
        '# Version: %s',
        '# Packages: %s',
        '# Generated: %s',
        sep = '\n'),
      module$label,
      toString(module$info[['author']]),
      toString(module$info[['version']]),
      paste(module$packages, collapse = ', '),
      format(now, '%Y-%m-%d %H:%M:%S')
    )

    opts <- local({
      current_opt <- rave_opts$get_options()
      default_opt <- rave:::opt
      for(name in names(current_opt)){
        if(assertthat::are_equal(current_opt[[name]], default_opt[[name]])){
          current_opt[[name]] <- NULL
        }
      }
      current_opt
    })
    args$opts <- sprintf(
      'opts <- list(\n%s\n)\n',
      sapply(names(opts), function(n){
        paste0('  ', n, ' = ', exprToString(opts[[n]]))
      }) %>%
        paste(collapse = ',\n')
    )

    # subject_id, electrodes, module_path, packages, is_univariate
    args$module_settings <- sprintf(
      paste(
        'packages <- c("%s")',
        'subject_id <- %s',
        'electrodes <- %s',
        'module_path <- %s',
        'is_univariate <- %s',
        sep = '\n'
      ),
      paste(module$packages, collapse = '", "'),
      exprToString(subject$id),
      exprToString(subject$data_environment$electrodes),
      exprToString(module$source_path),
      exprToString(module$is_univariate)
    )

    args$params <- sprintf(
      'params <- list(\n%s\n)\n',
      sapply(names(params), function(n){
        paste0('  ', n, ' = ', exprToString(params[[n]]))
      }) %>%
        paste(collapse = ',\n')
    )

    results <- module$runtime_env$SHINY_EXECUTE(params)

    args$results_expr <- sapply(names(results), function(n){
      result <- results[[n]]
      if(is.function(result)){
        exprToString(body(result)) ->
          body
        return(sprintf('# Output ID: %s \nwith(env, %s)\n\n', n, body))
      }else{
        return(sprintf('# Output ID: %s \n%s = %s',
                n, n, exprToString(result)))
      }

    }) %>%
      paste(collapse = '\n\n')

    # render template
    template <- readLines(system.file('export_template.R', package = 'rave'))
    template_trim <- stringr::str_trim(template)
    for(n in names(args)){
      key <- paste0('#{{', n, '}}')
      template[template_trim == key] <- args[[n]]
    }
    writeLines(template, con = script_path)
    return(script_path)
  }
}





#' @export
empty_module <- function(
  subject_id = NULL,
  electrodes = NULL,
  is_univariate = TRUE,
  suma_enabled = FALSE){
  module <- load_module(
    module_id = 'temp_module',
    source_path = system.file('default.R', package = 'rave'),
    category = 'TEMP',
    label = 'Temp Module',
    packages = '',
    author = '',
    version = '0.0.0',
    is_univariate = is_univariate,
    suma_enabled = suma_enabled
  )

  if(!is.null(subject_id)){
    subject <- get_subject(subject_id, temp = FALSE)
    subject$data_environment$load(electrodes = electrodes)
    subject$data_environment$bind_electrodes(electrodes = electrodes, debug = T)
    data_repository$set_data(subject$data_environment)
    data_repository$add_module(module)

  }

  return(module)
}
