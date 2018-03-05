#' @include utils_history.R

# singletons

..setup_env = new.env(parent = emptyenv())
..setup_env$setup_func = list()


#' @export
rave_hist = RAVEHistory$new()

