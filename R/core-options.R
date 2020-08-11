#' Function to locally save options (deprecated)
#' @export
save_options <- function(){
}

#' Function to change rave-options
#' @param ... Key-Value option pairs
#' @param .save save to disk? ignored most of the time
#' @param launch_gui launch shiny app?
#' @param host IP address of host
#' @param port Port number
#' @export
rave_options <- function(..., .save = TRUE, launch_gui = TRUE,
                         host = '127.0.0.1', port = NULL){
  args = list(...)
  if(length(args) && length(names(args))){
    # set options
    for(nm in names(args)){
      raveio::raveio_setopt(nm, args[[nm]], .save = .save)
    }
  }else if (length(args)){
    # get options
    args = c(...)
    re <- sapply(args, function(nm){
      val <- raveio::raveio_getopt(nm, default = NULL)
      if(nm %in% c('delay_input', 'image_width', 'image_height', 'drive_speed', 'max_worker', 'max_mem')){
        val <- as.numeric(val)
      } else if (nm %in% c('test_mode', 'fast_cache')){
        val <- as.logical(val)
      }
      val
    }, simplify = FALSE, USE.NAMES = TRUE)
    if(length(re) == 1){
      re <- unlist(re)
    }
    return(re)
  } else if(launch_gui){
    return(rave_options_gui(host = host, port = port))
  }
  
  return(invisible())
}



ugly_sample <- function(x, size = length(x), replace = FALSE, ...){
  oldseed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- oldseed)
  sample(x, size = size, replace = replace, ...)
}


rave_setup_workers <- function(n_workers = 0){
  
  # Force n_workers to be rave_option
  if( n_workers <= 0 ){
    n_workers = max(rave_options('max_worker'), 1)
  }
  
  dipsaus::make_forked_clusters(workers = n_workers)
  
  catgl('Number of workers switched to - {n_workers}')
  
}





