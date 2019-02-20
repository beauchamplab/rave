#' Check if a package is installed
#' @param pkg package name
#' @return logical, if the package is installed, return TRUE, otherwise FALSE
package_installed <- function(pkg){
  system.file('', package = pkg) != ''
}

#' Get function from external packages
#' @param f name of function
#' @param pkg package name
#' @param internal use "::" or ":::"
#' @param ifNotFound if package not found, return this value
#' @param check check existence of package?
get_from_package <- function(f, pkg, internal = FALSE, ifNotFound = NULL, check = TRUE){
  if(!check || package_installed(pkg)){
    export_f = ifelse(internal, ':::', '::')
    f = do.call(export_f, list(pkg = pkg, name = f))
    return(f)
  }
  return(ifNotFound)
}

package_version_check <- function(package, version = NULL){
  if(!package_installed(package)){
    return(FALSE)
  }

  if(!is.null(version)){
    v1 = utils::packageVersion(package)
    pass = utils::compareVersion(as.character(v1), as.character(version)) >= 0
    return(pass)
  }
  return(TRUE)

}
