#' @include utils_history

# singletons

..setup_env = new.env(parent = emptyenv())
..setup_env$setup_func = list()


#' @export
rave_hist = RAVEHistory$new()

