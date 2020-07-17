
guess_raw_trace <- function(dat, electrodes = NULL, matrix = FALSE){
  nms = names(dat)
  for(nm in nms){
    x <- dat[[nm]]
    if(!is.numeric(x) || mode(x) != "numeric"){ next }
    
    if(matrix){
      if(!is.matrix(x)){ next }
      dm <- dim(x)
      d1 <- min(dm)
      
      # d2 is the time points, d1 should be electrodes
      if(d1 < length(electrodes)){ next }
      return(nm)
    } else {
      # should be vector
      dm <- dim(x)
      if(is.null(dm)){
        return(nm)
      } else if (length(dm) %in% c(1,2)){
        if(min(dm) == 1){
          return(nm)
        }
      }
    }
    
  }
}


validate_raw_file_lfp <- function(subject_code, blocks, electrodes, format, check_content = TRUE, ...){
  raw_root <- rave_options('raw_data_dir')
  block_paths <- file.path(raw_root, subject_code, blocks)
  if(!all(dir.exists(block_paths))){
    return(structure(FALSE, reason = list(
      'One or more block folder is missing.' = blocks[!dir.exists(block_paths)]
    ), class = 'validate_failure'))
  }
  
  if(missing(electrodes)){
    electrodes <- NULL
  }
  snapshot <- NULL
  
  # format == 1: one file per electrode
  if(format == 1){
    
    finfo <- list()
    
    error_msg <- list()
    for(b in blocks){
      bpath <- file.path(raw_root, subject_code, b)
      files <- list.files(bpath, pattern = '[0-9]+\\.(mat|h5)$', ignore.case = TRUE)
      if(!length(files)){
        error_msg[['Cannot find any mat/h5 file']] = c(
          error_msg[['Cannot find any mat/h5 file']], b
        )
        next
      }
      finfo[[b]] <- list(
        path = bpath,
        files = files
      )
      if(length(electrodes)){
        # also check electrodes
        number <- stringr::str_match(files, '([0-9]+)\\.(mat|h5)$')[,2]
        number <- as.integer(number)
        number <- number[!is.na(number)]
        if(!all(electrodes %in% number)){
          melc <- electrodes[!electrodes %in% number]
          melc <- dipsaus::deparse_svec(melc)
          error_msg[['Electrode file missing']] <- c(
            error_msg[['Electrode file missing']], 
            sprintf('%s (electrode %s)', b, melc)
          )
        }
      }
    }
    
    if(length(error_msg)){
      return(structure(FALSE, reason = error_msg, class = 'validate_failure'))
    }
    
    # if check_content, open all files to check length
    if( check_content && length(electrodes) ){
      progress <- dipsaus::progress2('Check electrode files within block', shiny_auto_close = TRUE, max = length(blocks) + 1)
      for(b in blocks){
        progress$inc(b)
        info <- finfo[[b]]
        files <- info$files
        number <- stringr::str_match(files, '([0-9]+)\\.(mat|h5)$')[,2]
        sel <- number %in% as.character(electrodes)
        elec_bak <- as.integer(number[sel])
        files <- files[sel]
        abspaths <- file.path(info$path, files)
        ii <- 0
        dlen <- dipsaus::lapply_async2(abspaths, function(path){
          tryCatch({
            dat <- read_mat(path)
            nms <- names(dat)
            # guess which one is data?
            for(nm in nms){
              x <- dat[[nm]]
              if(!is.numeric(x)){ next }
              dm <- dim(x)
              if(length(dm) > 2){ next }
              if(dm == 2 && all(dm > 1)){ next }
              if(length(x) < 100){ next }
              return(length(x))
            }
            NA
          }, error = function(e){
            NA
          })
        })
        dlen <- unlist(dlen)
        if(any(is.na(dlen)) || length(unique(dlen)) > 1){
          msg <- sprintf("Block %s: ", b)
          mis_d <- elec_bak[is.na(dlen)]
          if(length(mis_d)){
            msg <- c(msg, sprintf('electrode(s) %s have broken file(s)', dipsaus::deparse_svec(mis_d)))
          }
          tbl <- sort(table(dlen, useNA = 'no'), decreasing = TRUE)
          if(length(tbl) > 1){
            lths <- as.integer(names(tbl))
            ssel <- dlen %in% lths[-1]
            if(any(ssel)){
              msg <- c(msg, sprintf('electrode(s) %s have different lengths', dipsaus::deparse_svec(elec_bak[ssel])))
            }
          }
          msg = paste(msg, collapse = '\n')
          
          error_msg[['Electrode data lengths are not consistent or missing']] <- c(
            error_msg[['Electrode data lengths are not consistent or missing']],
            msg
          )
          error_msg <- sprintf(
            'Electrode data length varies/missing in block %s. Please check raw traces within each file. Each file should contain one dataset with shared length',
            b
          )
        }
        
        if(is.null(snapshot)){
          # provide an snapshot
          path <- abspaths[[1]]
          dat <- read_mat(path)
          nms <- names(dat)
          for(nm in nms){
            x <- dat[[nm]]
            if(!is.numeric(x)){ next }
            dm <- dim(x)
            if(length(dm) > 2){ next }
            if(dm == 2 && all(dm > 1)){ next }
            len <- length(x)
            if(len < 100){ next }
            if(is.null(snapshot)){
              snapshot <- sprintf('A single vector of length %d (time points)', len)
            }
            rm(x)
          }
          NA
          rm(dat)
        }
      }
    }
    
    if(length(error_msg)){
      return(structure(FALSE, reason = error_msg, class = 'validate_failure'))
    }
    
    return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
  }
  
  # format == 2: one big mat/h5 for all electrode
  if(format == 2){
    finfo <- list()
    
    error_msg <- list()
    
    for(b in blocks){
      bpath <- file.path(raw_root, subject_code, b)
      files <- list.files(bpath, pattern = '\\.(mat|h5)$', ignore.case = TRUE)
      if(!length(files)){
        error_msg[['Cannot find any mat/h5 file']] = c(
          error_msg[['Cannot find any mat/h5 file']], b
        )
        next
      } else if(length(files) > 1){
        s <- 'Block contains too many mat/h5 files. Unable to decide which one is the data file. Please either reduce files to one, or try different importing format.'
        error_msg[[s]] = c( error_msg[[s]], b )
        next
      }
      files <- files[[1]]
      finfo[[b]] <- list(
        path = bpath,
        files = files
      )
    }
    
    if(length(error_msg)){
      return(structure(FALSE, reason = error_msg, class = 'validate_failure'))
    }
    
    if( check_content && length(electrodes) ){
      progress <- dipsaus::progress2('Check electrode files within block', shiny_auto_close = TRUE, max = length(blocks) + 1)
      # Need to check content to see whether data is valid
      for(b in blocks){
        progress$inc(b)
        info <- finfo[[b]]
        files <- info$files
        abspath <- file.path(info$path, files)
        tryCatch({
          dat <- read_mat(abspath)
          nms <- names(dat)
          vnm <- list()
          for(nm in nms){
            x <- dat[[nm]]
            if(!is.numeric(x)){ next }
            if(!is.matrix(x)) { next }
            vnm[[nm]] <- dim(x)
          }
          
          if(length(vnm) > 1){
            error_msg[['Block file contains more than one dataset.']] <- c(
              error_msg[['Block file contains more than one dataset.']], b
            )
          } else if(length(vnm) == 0){
            error_msg[['Block file contains no dataset.']] <- c(
              error_msg[['Block file contains no dataset.']], b
            )
          } else{
            dim <- vnm[[1]]
            # Assume min dim is electrodes as time is usually large
            max_elec <- min(dim)
            mis_e <- electrodes[!electrodes %in% seq_len(max_elec)]
            
            if(length(mis_e)){
              error_msg[['Electrode(s) missing']] <- c(
                error_msg[['Electrode(s) missing']],
                sprintf('Found matrix (size: %dx%d) in block %s. Electrode %s are missing (available electrodes: 1-%d)', 
                        dim[1], dim[2], b, dipsaus::deparse_svec(mis_e), max_elec)
              )
            } else {
              if(is.null(snapshot)){
                snapshot <- sprintf(
                  'Variable name is %s, a matrix: %d available electrodes, %d time points.', 
                  sQuote(names(vnm)[[1]]), max_elec, max(dim)
                )
              }
            }
            
            mis_e
          }
          
        }, error = function(e){
          error_msg[['Block file is broken']] <- c(
            error_msg[['Block file is broken']], b
          )
        })
      }
      
    }
    
    if(length(error_msg)){
      return(structure(FALSE, reason = error_msg, class = 'validate_failure'))
    }
    
    return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
  }
  
  # format == 2: EDF format
  if(format == 3){
    finfo <- list()
    
    error_msg <- list()
    for(b in blocks){
      bpath <- file.path(raw_root, subject_code, b)
      files <- list.files(bpath, pattern = '\\.(edf)$', ignore.case = TRUE)
      if(!length(files)){
        error_msg[['Cannot find any EDF file']] = c(
          error_msg[['Cannot find any EDF file']], b
        )
        next
      }
      if(length(files) > 1){
        error_msg[['Found more than one EDF file in one session. Please make sure one EDF file per session']] = c(
          error_msg[['Found more than one EDF file in one session. Please make sure one EDF file per session']], b
        )
        next
      }
      files = files[[1]]
      
      finfo[[b]] <- list(
        path = bpath,
        files = files
      )
    }
    
    
    if( check_content && length(electrodes) ){
      
      srates <- lapply(blocks, function(b){
        info <- finfo[[b]]
        edf_path <- file.path(info$path, info$files)
        
        header <- tryCatch({
          raveio::read_edf_header(edf_path)
        }, error = dipsaus::do_nothing)
        
        if(!length(header)){
          error_msg[['Failed to open EDF file']] <<- c(
            error_msg[['Failed to open EDF file']], b
          )
          return()
        }
        
        has_elec <- electrodes %in% which(!header$isAnnot2)
        if(!all(has_elec)){
          which_elec <- dipsaus::deparse_svec(which(!has_elec))
          error_msg[['Electrode(s) not found in EDF file']] <<- c(
            error_msg[['Electrode(s) not found in EDF file']], sprintf('%s - %s', b, which_elec)
          )
          return()
        }
        srate <- header$sampleRate2[!is.na(header$sampleRate2)]
        if(length(srate) > 1){
          srate[[1]]
        }else{
          NA
        }
        
        
      })
      
      srates <- unlist(srates)
      
      if(any(is.na(srates)) || length(unique(srates)) > 1){
        error_msg[['Found different sample rates across sessions']] = c(
          error_msg[['Found different sample rates across sessions']], 
          sprintf('%s, sample rate - %.1f', blocks, srates)
        )
      } else {
        info <- finfo[[1]]
        edf_path <- file.path(info$path, info$files)[[1]]
        header <- tryCatch({
          raveio::read_edf_header(edf_path)
        }, error = dipsaus::do_nothing)
        units <- header$unit2
        units <- units[!is.na(units)]
        units <- unique(units)
        snapshot = sprintf('EDF format recorded sample rate is %.1f, and %d physical units found: %s',
                           srates[[1]], length(units), paste(units, collapse = ', '))
      }
      
    }
    
    if(length(error_msg)){
      return(structure(FALSE, reason = error_msg, class = 'validate_failure'))
    }
    
    return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
    
  }
  
  
  return(structure(FALSE, reason = list(
    'Unknown format' = character(0)
  ), class = 'validate_failure'))
}
