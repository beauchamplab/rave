#' @export
logger = function(..., level = 'DEBUG'){
  tryCatch({
    args = unlist(list(...))
    cat('[', level, ']: ', args, '\n', sep = '')
  }, error = function(e){
    print(list(...))
  })
}
