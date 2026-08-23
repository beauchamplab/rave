#' @keywords internal
"_PACKAGE"

# ---------------------------------------------------------------------------
# RAVE 1.0 compatibility shim.
#
# The vast majority of the original `rave` (RAVE 1.0) package has been split
# into the modern RAVE 2.0 ecosystem (dipsaus, ieegio, ravecore, ravepipeline,
# ravedash, filearray, threeBrain, ravetools, ravemanager). This single file
# retains only the thin surface that the modern packages still rely on. The
# code here is intentionally minimal and will gradually phase out.
#
# Exported (public) API:
#   * start_rave / start_rave2 / start_yael  -- launchers used by `ravemanager`,
#     `rave-pipelines`, and friends via `rave::start_*()`.
#
# Internal (NOT exported) -- reachable within the namespace and via
# `asNamespace("rave")$...`, which is how `ravecore` calls `download_sample_data`:
#   * download_sample_data / download_subject_data
#   * rave_options / save_options / arrange_data_dir
#   * rave_version / latest_version / .onAttach
#   * open_tab / close_tab
#   * catgl / dir_create / is_invalid / get_val / `%?<-%`
# ---------------------------------------------------------------------------


# ===========================================================================
# Internal helpers (not exported)
# ===========================================================================

# Open or close a tab in the (legacy) 'RAVE' main application. Sends a custom
# message to the current 'shiny' session.
close_tab <- function(module_id, tabname) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage("rave_close_tab", list(
      module_id = module_id,
      title = tabname
    ))
  }
}

open_tab <- function(module_id, tabname) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    session$sendCustomMessage("rave_open_tab", list(
      module_id = module_id,
      title = tabname
    ))
  }
}


# Left-hand side checked assignment (copied verbatim from `dipsaus`; the package
# author maintains `dipsaus`, so there is no license/authorship concern). Assign



# Lightweight replacement for the historical `catgl` (cat + glue) helper.
# The canonical implementation now lives in `ravepipeline` (internal), so this
# delegates to the exported `ravepipeline::logger` / `ravepipeline::glue` and
# preserves the original `FATAL -> stop` behavior.
catgl <- function(..., .envir = parent.frame(), level = "DEBUG") {
  level <- toupper(level)
  msg <- tryCatch(
    as.character(ravepipeline::glue(..., .envir = .envir)),
    error = function(e) paste0(...)
  )
  log_level <- switch(
    level,
    "DEFAULT" = "trace",
    "DEBUG" = "debug",
    "INFO" = "info",
    "WARNING" = "warning",
    "SUCCESS" = "success",
    "ERROR" = "error",
    "FATAL" = "fatal",
    "trace"
  )
  ravepipeline::logger(msg, level = log_level)
  if (level == "FATAL") {
    stop(msg)
  }
  invisible(msg)
}


dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    catgl("Cannot create directory at ", shQuote(x), level = "FATAL")
  }
  invisible(normalizePath(x))
}


is_invalid <- function(x, any = FALSE, .invalids = c("null", "na")) {
  if ("null" %in% .invalids) {
    if (is.null(x) || !length(x)) {
      return(TRUE)
    }
  }
  for (func in paste0("is.", .invalids)) {
    res <- do.call(func, args = list(x))
    if (length(res) > 1) {
      if (any) {
        res <- any(res)
      } else {
        res <- all(res)
      }
    }
    if (res) {
      return(TRUE)
    }
  }
  return(FALSE)
}


# Get a value from `x` by `key`, returning a default when the value is invalid.
get_val <- function(x, key = NULL, ..., .invalids = c("null", "na")) {

  if (is.null(key)) {
    val <- x
  } else {
    val <- x[[key]]
  }
  if (is_invalid(val, .invalids = .invalids)) {
    if (...length() > 1) {
      return(list(...))
    } else {
      return(...elt(1))
    }
  }
  return(val)
}


# Locally save options (deprecated no-op; kept for backward compatibility).
save_options <- function() {
}


# Get or set RAVE options; delegates to `ravepipeline`. The legacy options GUI
# has been removed.
rave_options <- function(..., .save = TRUE, launch_gui = TRUE,
                         host = "127.0.0.1", port = NULL) {
  args <- list(...)
  if (length(args) &&
     length(names(args))) {
    # set options
    for (nm in names(args)) {
      ravepipeline::raveio_setopt(nm, args[[nm]], .save = .save)
    }
  } else if (length(args)) {
    # get options
    args <- c(...)
    re <- sapply(args, function(nm) {
      val <- ravepipeline::raveio_getopt(nm, default = NULL)
      if (nm %in% c(
        "delay_input",
        "image_width",
        "image_height",
        "drive_speed",
        "max_worker",
        "max_mem"
      )) {
        val <- as.numeric(val)
      } else if (nm %in% c("test_mode", "fast_cache")) {
        val <- as.logical(val)
      }
      val
    }, simplify = FALSE, USE.NAMES = TRUE)
    if (length(re) == 1) {
      re <- unlist(re)
    }
    return(re)
  } else if (launch_gui) {
    message(
      "The 'rave_options' GUI has been removed. Please use ",
      "'ravemanager::version_info()' and 'ravepipeline::raveio_setopt()' ",
      "to manage RAVE options."
    )
    return(invisible())
  }

  return(invisible())
}


# Initialize / validate the RAVE data repository directories.
arrange_data_dir <- function(first_time = FALSE, reset = FALSE) {
  if (first_time ||
     reset) {
    data_dir <- "~/rave_data/data_dir"
    raw_dir <- "~/rave_data/raw_dir"
    
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  if (reset) {
    rave_options(data_dir = data_dir, raw_data_dir = raw_dir)
  }
  
  data_dir <- rave_options("data_dir")
  raw_dir <- rave_options("raw_data_dir")
  
  if (!dir.exists(data_dir) || !dir.exists(raw_dir)) {
    catgl("Cannot find data directory for RAVE. Please make sure that these folder exists",
          level = "ERROR")
    catgl(data_dir, level = "ERROR")
    catgl(raw_dir, level = "ERROR")
    catgl(
      "Check existence of these folders, or reset default data repository by typing arrange_data_dir(reset = TRUE)",
      level = "ERROR"
    )
    return(FALSE)
  } else {
    rave_options(data_dir = base::normalizePath(data_dir))
    rave_options(raw_data_dir = base::normalizePath(raw_dir))
    
    return(TRUE)
  }

}


# Download demo data to the data repository. Accessed by `ravecore` via
# `asNamespace("rave")$download_sample_data(...)`, so it is intentionally
# kept internal (unexported).
download_sample_data <- function(subject, version = "v0.1.8-beta", ...) {

  if (missing(subject)) {
    sbj_names <- c("KC", "YAB", "_group_data")
    version <- "v0.1.8-beta"
    sapply(sbj_names, download_sample_data, version = version, ...)
    return(invisible())
  }
  

  url <- sprintf("https://github.com/beauchamplab/rave/releases/download/%s/demo_%s.zip", version, subject)
  download_subject_data(url, ...)
}


# Download subjects from the internet or a local zip file into the RAVE data
# repository. See the (removed) RAVE 1.0 docs for the `subject_settings` format.
download_subject_data <- function(
  con, replace_if_exists = FALSE, override_project = NULL, override_subject = NULL,
  temp_dir = tempdir(), remove_zipfile = TRUE, subject_settings = NULL,
  mode = "wb", ...) {

  # Large files, need long time to download
  opt_timeout <- getOption("timeout", 1e10)
  options("timeout" = 1e10)
  on.exit({
    options("timeout" = opt_timeout)
  }, add = TRUE, after = TRUE)



  url <- con
  # url = "https://s3-us-west-2.amazonaws.com/rave-demo-subject/sfn-demo/data-large.zip"
  # url = "/var/folders/rh/4bkfl5z50wgbbjd85xvc695c0000gn/T//RtmpmUoaTy/junk_45d3370d10d.zip"

  if (!file.exists(url)) {
    # this is not a local file, download

    # First, try to download subject data
    catgl("Download from - ", url, level = "INFO")

    # prepare files
    temp_file <- tempfile(pattern = "junk_", temp_dir, fileext = ".zip")
    # download
    utils::download.file(url, destfile = temp_file, mode = mode, ...)
  } else {
    remove_zipfile <- FALSE
    temp_file <- url
  }

  extract_dir <- file.path(temp_dir, paste(sample(LETTERS, 10), collapse = ""))
  dir_create(extract_dir)
  on.exit({
    # clean up
    unlink(extract_dir, recursive = TRUE)
    if (remove_zipfile) {
      unlink(temp_file, recursive = TRUE)
    } else {
      catgl("Please manually remove zip file by running:\n",
             sprintf('unlink("%s")', temp_file), level = "INFO")
    }
  })


  # Extract
  catgl("Unzip the folder", level = "INFO")
  utils::unzip(temp_file, exdir = extract_dir, overwrite = TRUE)

  # Check folder
  # look for meta.yaml
  if (is.null(subject_settings)) {
    yaml_files <- list.files(extract_dir, pattern = "subjects.yaml$", recursive = TRUE, full.names = TRUE)

    if (length(yaml_files)) {
      depth <- stringr::str_count(yaml_files, "(/|\\\\)")
      file <- yaml_files[which.min(depth)[1]]
      meta <- as.list(ravepipeline::load_yaml(file))
    } else {
      catgl('No subjects.yaml found! Please use "subject_settings" argument to specify subject settings', level = "FATAL")
    }
  } else {
    meta <- subject_settings
  }


  # check data
  for (ii in seq_along(meta)) {
    catgl("----------------------------", level = "INFO")
    subject_id <- names(meta)[[ii]]
    subject_id %?<-% ""

    # get subject project name and subject code
    s <- strsplit(subject_id, "/|\\\\")[[1]]

    s <- s[s != ""]
    if (length(s) < 2) {
      catgl("Invalid subject ID - ", subject_id, " (abort)", level = "ERROR")
      next()
    }
    if (is.null(override_project)) {
      project_name <- s[1]
    } else {
      project_name <- override_project
    }

    if (is.null(override_subject)) {
      subject_code <- s[2]
    } else {
      subject_code <- override_subject
    }
    catgl("Project Name: [", project_name, "]; Subject Code: [", subject_code, "] (checking)", level = "INFO")



    data_dir <- meta[[ii]][["data_dir"]]
    raw_dir <- meta[[ii]][["raw_dir"]]

    # check if data_dir exists
    rdir <- extract_dir
    ds <- list.dirs(extract_dir, full.names = FALSE, recursive = FALSE)
    if ( !"data_dir" %in% ds ) {
      ds <- ds[stringr::str_length(ds) == 1 | stringr::str_detect(ds, "^[^.~][^_]")]
      ds <- ds[!ds %in% c(".", "_", "~", "^")]
      if (length(ds)) {
        rdir <- file.path(rdir, ds[1])
      }
    }

    if (length(data_dir) == 1 && is.character(data_dir)) {
      # try to find dir
      data_dir <- file.path(rdir, data_dir)
      if (!dir.exists(data_dir)) {
        catgl("\n\tdata_dir not exists\n",
               "\tPlease check existence of data_dir: \n", data_dir, level = "WARNING")
      } else {
        catgl("\tdata directory found! - \n", data_dir, level = "INFO")
      }
    } else {
      data_dir <- NULL
      catgl("No \"data_dir\" in subject settings", level = "WARNING")
    }


    if (length(raw_dir) == 1 && is.character(raw_dir)) {
      # try to find dir
      raw_dir <- file.path(rdir, raw_dir)
      if (!dir.exists(raw_dir)) {
        catgl("\n\traw_dir not exists\n",
               "\tPlease check existence of raw_dir: \n", raw_dir, level = "WARNING")
      } else {
        catgl("\traw directory found! - \n", raw_dir, level = "INFO")
      }
    } else {
      raw_dir <- NULL
      catgl("No \"raw_dir\" in subject settings, abort this one", level = "WARNING")
    }


    # Check subject existence
    rave_data_dir <- rave_options("data_dir")
    rave_raw_dir <- rave_options("raw_data_dir")
    check_existence <- function(subject_code) {
      has_subject <- c(FALSE, FALSE)
      exist_proj <- list.dirs(rave_data_dir, full.names = FALSE, recursive = FALSE)
      if (project_name %in% exist_proj) {
        # need to check if subject exists
        exist_subs <- list.dirs(file.path(rave_data_dir, project_name), full.names = FALSE, recursive = FALSE)
        if (subject_code %in% exist_subs) {
          has_subject[2] <- TRUE
        }
      }
      if (subject_code %in% list.dirs(rave_raw_dir, full.names = FALSE, recursive = FALSE)) {
        has_subject[1] <- TRUE
      }
      has_subject
    }


    if (!replace_if_exists && any(check_existence(subject_code))) {
      count <- 5
      choice <- subject_code
      while (count > 0) {
        count <- count - 1
        catgl("\nSubject [", choice, "] already exists. Replace? or enter new subject code here:\n",
               "\t- yes, or Y(y) to overwrite\n",
               "\t- any other characters for new subject code\n",
               "\t- or leave it blank to cancel importing this subject", level = "WARNING")
        choice <- readline(prompt = ":")
        choice <- stringr::str_trim(choice)
        if (!stringr::str_detect(stringr::str_to_lower(choice), "^(y$)|(yes$)")) {
          # rename
          if (choice == "") {
            catgl("Cancel importing ", subject_code, level = "INFO")
            subject_code <- ""
            break
          }
          if (!any(check_existence(choice))) {
            catgl("Rename subject to ", choice, level = "INFO")
            catgl("Renaming subjects might cause some problems for SUMA", level = "WARNING")
            subject_code <- choice

            break()
          }
        } else {
          catgl("Overwrite subject ", subject_code, level = "INFO")
          break()
        }
      }


    }

    # Now we need to check
    if (subject_code == "") {
      next()
    }

    # importing subject
    catgl("Copy files:", level = "INFO")
    # raw dir
    to_dir <- file.path(rave_raw_dir, subject_code)
    if (length(raw_dir)) {
      dir_create(to_dir)
      lapply(list.files(raw_dir, all.files = TRUE, full.names = TRUE, recursive = FALSE), function(d) {
        file.copy(d, to_dir, overwrite = TRUE, recursive = TRUE)
      })
      catgl("[New raw dir] ", to_dir, level = "INFO")
    } else {
      catgl("Raw data is not imported.")
    }


    # data dir
    to_dir <- file.path(rave_data_dir, project_name, subject_code)
    if (length(data_dir)) {
      dir_create(to_dir)
      lapply(list.files(data_dir, all.files = TRUE, full.names = TRUE, recursive = FALSE), function(d) {
        file.copy(d, to_dir, overwrite = TRUE, recursive = TRUE)
      })
      catgl("[New data dir] ", to_dir, level = "INFO")
    } else {
      catgl("RAVE data is not imported.")
    }

    catgl("\n\t[", project_name, "/", subject_code, "] Done.\n", level = "INFO")
  }

  catgl("\n----------------------------", level = "INFO")

}


# Current installed version of `rave`.
rave_version <- function() {
  as.character(utils::packageVersion("rave"))
}


# Best-effort lookup of the latest published `rave` version (for the startup
# message). `raveio` is a soft dependency, so failures are swallowed.
latest_version <- function() {
  tryCatch({
    suppressWarnings({
      ravepipeline <- asNamespace("ravepipeline")
      versions <- ravepipeline$load_json("https://rave-ieeg.r-universe.dev/api/packages/rave")
      return(list(
        version = versions$Version[[1]],
        built = versions$Packaged$Date
      ))
    })
  }, error = function(e) {
    NULL
  })
}

# 
# .onAttach <- function(libname, pkgname) {
#   try({
#     if ( arrange_data_dir(FALSE) ) {
# 
#       current_version <- rave_version()
#       latest <- latest_version()
#       if (is.null(latest)) {
#         latest <- "(Unable to obtain the update information)"
#       } else {
#         latest <- sprintf("%s (built: %s)", latest$version, latest$built)
#       }
# 
#       packageStartupMessage(sprintf(paste(
#         "RAVE is loaded!",
#         "  Current version          - %s",
#         "  Latest available version - %s",
#         "Data Repository:     \t%s",
#         "Raw-data Repository: \t%s",
#         "\nTo check for update, type %s.",
#         sep = "\n"
#       ),
#       current_version, latest,
#       rave_options("data_dir"), rave_options("raw_data_dir"),
#       sQuote("ravemanager::version_info()")
#       ))
#     } else {
#       packageStartupMessage("[WARNING]: Cannot find RAVE repository! Please run the following command set them.\n\trave::rave_options()")
#     }
# 
#   }, silent = TRUE)
# 
# }
