

#' @export
as.character.rave_bytes <- function(x, digit=1, ...){
  sprintf(sprintf('%%.%df %s', digit, attr(x, 'unit')), x)
}

#' @export
print.rave_bytes <- function(x, digit=1, ...){
  re = as.character(x, digit = digit, ...)
  cat(re)
  invisible(re)
}


lower_letters_only <- function(s){
  s <- stringr::str_to_lower(s)
  stringr::str_remove_all(s, '[^a-zA-Z0-9]')
}

openwetware_url <- function(title, module_label, package, type = 'input'){
  ctx <- rave_context()
  if(ctx$context %in% c('rave_running', 'rave_running_local')){
    package %?<-% ctx$package
    module_label %?<-% ctx$instance$module_env$label_name
  }
  
  package <- lower_letters_only(package)
  module_label <- lower_letters_only(module_label)
  title <- lower_letters_only(title)
  sprintf('https://openwetware.org/wiki/RAVE:%s:%s:%s_%s', package, module_label, type, title)
  
}
