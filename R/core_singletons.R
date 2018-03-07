#' @include utils_history.R
#' #' @include cls_map.R

# singletons

..setup_env = new.env(parent = emptyenv())
..setup_env$setup_func = list()

..setup_env$ff_pointers = Map$new(class_type = 'ff')

#' @export
rave_hist = RAVEHistory$new()

