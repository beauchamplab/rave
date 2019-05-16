#' @include utils_history.R
#' @include cls_map.R

#' singletons internal use
..setup_env = new.env(parent = emptyenv())
# ..setup_env$setup_func = list()
# ..setup_env$workers = NULL

rave_hist = RAVEHistory$new(use_yaml = TRUE)

