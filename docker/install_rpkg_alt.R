#!/usr/bin/env r
#
# A second example to install one or more packages, now with option parsing
#
# Copyright (C) 2011 - 2014  Dirk Eddelbuettel
# Copyright (C) 2014 - 2017  Carl Boettiger and Dirk Eddelbuettel
# Copyright (C) 2018         Carl Boettiger, Dirk Eddelbuettel, and Brandon Bertelsen
#
# Released under GPL (>= 2)

## load docopt package from CRAN
library(docopt)

## configuration for docopt
doc <- "Usage: install_rpkg_alt [-r REPO...] [-l LIBLOC] [-h] [-x] [-s] [-d DEPS] [-n NCPUS] [--error] [--] [PACKAGES ...]

-r --repos REPO     repository to use, or NULL for file [default: getOption]
-l --libloc LIBLOC  location in which to install [default: /usr/local/lib/R/site-library]
-d --deps DEPS      install suggested dependencies as well [default: NA]
-n --ncpus NCPUS    number of processes to use for parallel install [default: getOption]
-e --error          throw error and halt instead of a warning [default: FALSE]
-s --skipinstalled  skip installing already installed packages [default: FALSE]
-h --help           show this help text
-x --usage          show help and short example usage"
opt <- docopt(doc)			# docopt parsing

if (opt$usage) {
  cat(doc, "\n\n")
  cat("where PACKAGES... can be one or more CRAN package names, or local (binary or source)
package files (where extensions .tar.gz, .tgz and .zip are recognised). Optional
arguments understood by R CMD INSTALL can be passed interspersed in the PACKAGES, though
this requires use of '--'.

Examples:
  install_rpkg_alt -l /tmp/lib Rcpp BH                    # install into given library
  install_rpkg_alt -- --with-keep.source drat             # keep the source
  install_rpkg_alt -- --data-compress=bzip2 stringdist    # prefer bz2 compression
  install_rpkg_alt \".\"                                  # install package in current directory
  install_rpkg_alt -n 6 ggplot2                           # parallel install: (6 processes)

install_rpkg_alt is part of littler which brings 'r' to the command-line.
See http://dirk.eddelbuettel.com/code/littler.html for more information.\n")
  q("no")
}

if (opt$deps == "TRUE" || opt$deps == "FALSE") {
  opt$deps <- as.logical(opt$deps)
} else if (opt$deps == "NA") {
  opt$deps <- NA
}

## docopt results are characters, so if we meant NULL we have to set NULL
if (opt$repos == "NULL")  {
  opt$repos <- NULL
} else if (opt$repos == "getOption") {
  ## as littler can now read ~/.littler.r and/or /etc/littler.r we can preset elsewhere
  opt$repos <- getOption("repos")
}
opt$repos[["CRAN"]] <- "https://cran.rstudio.com/"
opt$repos[["RcppCore"]] <- "https://RcppCore.github.io/drat/"
opt$repos[["dipterix"]] <- "https://dipterix.github.io/drat/"
opt$repos <- unlist(opt$repos)

if (opt$ncpus == "getOption") {
  opt$ncpus <- getOption("Ncpus", 1L)
} else if (opt$ncpus == "-1") {
  ## parallel comes with R 2.14+
  opt$ncpus <- max(1L, parallel::detectCores())
}

install_packages2 <- function(pkgs, ..., error = FALSE, skipinstalled = FALSE) {
  e <- NULL
  capture <- function(e) {
    if (error) {
      catch <-
        grepl("package.*(is|are) not available", e$message) ||
        grepl("installation of package.*had non-zero exit status", e$message)
      if (catch) {
        e <<- e
      }
    }
  }
  if (skipinstalled) {
    pkgs <- setdiff(pkgs, installed.packages()[,1])
  }
  if (length(pkgs) > 0) {
    withCallingHandlers(install.packages(pkgs, ...), warning = capture)
    if (!is.null(e)) {
      stop(e$message, call. = FALSE)
    }
  }
}

## helper function to for existing files with matching extension
isMatchingFile <- function(f) (file.exists(f) && grepl("(\\.tar\\.gz|\\.tgz|\\.zip)$", f)) || (f == ".")

## helper function which switches to local (ie NULL) repo if matching file is presented
installArg <- function(f, lib, rep, dep, iopts, error, skipinstalled, ncpus) {
  install_packages2(pkgs=f,
                    lib=lib,
                    repos=if (isMatchingFile(f)) NULL else rep,
                    dependencies=dep,
                    INSTALL_opts=iopts,
                    Ncpus = ncpus,
                    error = error,
                    skipinstalled = skipinstalled)
}

## strip out arguments to be passed to R CMD INSTALL
isArg <- grepl('^--',opt$PACKAGES)
installOpts <- opt$PACKAGES[isArg]
opt$PACKAGES <- opt$PACKAGES[!isArg]

if (length(opt$PACKAGES)==0 && file.exists("DESCRIPTION") && file.exists("NAMESPACE")) {
  ## we are in a source directory, so build it
  message("* installing *source* package found in current working directory ...")
  opt$PACKAGES <- "."
}

sapply(opt$PACKAGES, installArg, opt$libloc, opt$repos, opt$deps,
       installOpts, opt$error, opt$skipinstalled, opt$ncpus)

## clean up any temp file containing CRAN directory information
sapply(list.files(path=tempdir(), pattern="^(repos|libloc).*\\.rds$", full.names=TRUE), unlink)
