# The script uninstalls RAVE

# Removes cached files
clear_dir <- function(dir) {
  try({
    if(length(dir) != 1) { return() }
    if(is.na(dir)) { return() }
    if(!dir.exists(dir)) { return() }
    cat("Clearing ", dir, "\n")
    unlink(dir, recursive = TRUE)
  }, silent = TRUE)
}

tryCatch({
  raveio::clear_cached_files()
}, error = function() {
  clear_dir('~/rave_data/cache_dir/')
  clear_dir(tools::R_user_dir('raveio', "cache"))
  try({
    clear_dir(raveio::cache_root())
  }, silent = TRUE)
  
  ravetools_path <- file.path(
    getOption(
      x = "ravetools.tempdir",
      default = Sys.getenv(
        x ="RAVETOOLS_TEMPDIR",
        unset = tempdir(check = FALSE)
      )
    ),
    "ravetools"
  )
  if(isTRUE(dir.exists(ravetools_path))) {
    unlink(ravetools_path, recursive = TRUE)
  }
})

# Remove templates
clear_dir(raveio::raveio_getopt("module_root_dir"))
clear_dir(threeBrain::default_template_directory())

# Remove packages
libs <- .libPaths()
pkgs <- c("ravebuiltins", "rave", "ravedash", "raveio", "threeBrain")
for(pkg in pkgs) {
  for(lib in libs) {
    try(silent = TRUE, {
      detach(sprintf("package:%s", pkg), unload = TRUE)
    })
    try(silent = TRUE, {
      unloadNamespace(pkg)
    })
    try(silent = TRUE, {
      utils::remove.packages(pkg, lib = lib)
    })
  }
}


# Remove data files
clear_dir(tools::R_user_dir('shidashi', "data"))
clear_dir(tools::R_user_dir('ravedash', "data"))
clear_dir(tools::R_user_dir('threeBrain', "data"))
clear_dir(tools::R_user_dir('raveio', "data"))

# Remove configurations
clear_dir(tools::R_user_dir('raveio', "config"))
clear_dir(tools::R_user_dir('rpymat', "config"))

