#' Color console for RAVE
#' @param ... things to print
#' @param level logger level: DEBUG, INFO, WARNING, ERROR, FATAL
#' @import crayon
#' @export
logger <- function(..., level = 'DEBUG'){
  ld = !rave_options('logger_enabled')
  lv = rave_options('logger_level')
  ce = rave_options('crayon_enabled')


  levels = c('DEBUG','INFO','WARNING','ERROR','FATAL')
  if(level != 'FATAL' && !is.null(ld) && ld[[1]]){
    return(invisible())
  }
  if(level != 'FATAL' && !is.null(lv) && lv[[1]] %in% levels){
    id1 = which(levels == lv[[1]])
    id2 = which(levels == level)
    if(id1 > id2){
      return(invisible())
    }
  }

  if('crayon' %in% installed.packages()[,1] && ce){

    switch (
      level,
      'DEBUG' = 'grey80',
      'INFO' = '#1d9f34',
      'WARNING' = '#ec942c',
      'ERROR' = '#f02c2c',
      'FATAL' = '#763053'
    ) ->
      .col

    col = crayon::make_style(.col)

    tryCatch({
      args = unlist(list(...))
      cat('[', level, ']: ')
      cat(col(args), '\n', sep = '')
    }, error = function(e){
      print(list(...))
    })
  }else{
    tryCatch({
      args = unlist(list(...))
      cat('[', level, ']: ')
      cat(args, '\n', sep = '')
    }, error = function(e){
      print(list(...))
    })
  }

  if(level == 'FATAL'){
    stop(shiny::safeError('FATAL error found. Process terminated.'))
  }

}
