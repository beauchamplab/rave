#' @include utils_history.R
#' @include cls_map.R

# singletons

..setup_env = new.env(parent = emptyenv())
..setup_env$setup_func = list()

# ..setup_env$ff_pointers = Map$new(class_type = 'ff', finalize = function(){
#   lapply(..setup_env$ff_pointers$values(), function(x){
#     ff::close.ff(x)
#   })
# })

#' @export
rave_hist = RAVEHistory$new()

