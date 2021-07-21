#' Tools to load and view brain in 3D viewer
#' @param subject character or `RAVE` subject instance
#' @param surfaces one or more from \code{"pial"}, \code{"white"}, 
#' \code{"smoothwm"}, brain surface types 
#' @param use_141 logical, whether to use standard 141 brain
#' @param compute_template logical whether to compute nearest 141 node. Please 
#' also check \code{freesurfer_brain}.
#' @param usetemplateifmissing whether logical, to display template brain if 
#' subject brain not found, default is false
#' @param recache whether to force cache data, default is false
#' @param clean_before_cache whether to clean cache before redo cache, default 
#' is false
#' @export
rave_brain2 <- function(subject, surfaces = 'pial', use_141 = TRUE, 
                        recache = FALSE, clean_before_cache = FALSE,
                        compute_template = FALSE, usetemplateifmissing = FALSE){
  
  # if subject is NULL, use current loaded subject
  if( is.character( subject ) ){
    subject = as_subject(subject, strict = FALSE)
  }
  
  # To find freesurfer directory, here are the paths to search
  # 1. rave_data/project/subject/rave/fs
  # 2. rave_data/project/subject/fs
  # 3. rave_data/project/subject/
  # 3. if options('rave.freesurfer_dir') is provided, then XXX/subject/
  
  fs_paths = c(
    file.path(subject$dirs$rave_dir, 'fs'),
    file.path(subject$dirs$rave_dir, '../fs'),
    file.path(subject$dirs$rave_dir, '../'),
    file.path(getOption('rave.freesurfer_dir'), subject$subject_code)
  )
  fs_path = NULL
  for( p in fs_paths ){
    if( threeBrain::check_freesurfer_path(fs_subject_folder = p, autoinstall_template = FALSE) ){
      fs_path = p
      break;
    }
  }
  
  # load electrodes
  electrode_table = load_meta('electrodes', 
                              project_name = subject$project_name, 
                              subject_code = subject$subject_code)
  
  if( is.null(fs_path) ){
    if( !usetemplateifmissing ){
      return(invisible())
    }
    
    brain = threeBrain::merge_brain()
    
    brain$set_electrodes(electrodes = electrode_table)
    
  }else{
    # import from freesurfer folder
    if(recache){
      if( clean_before_cache ){
        fs = list.files(file.path(fs_path, 'RAVE'), pattern = '\\.json$',
                        all.files = FALSE, recursive = FALSE, full.names = TRUE, 
                        ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
        lapply(fs, unlink)
      }
      threeBrain::import_from_freesurfer(fs_path, subject_name = subject$subject_code)
    }
    brain = threeBrain::freesurfer_brain2(
      fs_subject_folder = fs_path, subject_name = subject$subject_code, 
      surface_types = surfaces, use_141 = use_141)
    
    brain$set_electrodes(electrodes = electrode_table)
    
    if( compute_template ){
      tf = tempfile() 
      new_table = brain$calculate_template_coordinates(save_to = tf)
      if( file.exists(tf) ){
        brain$electrodes$raw_table_path = NULL
        unlink(tf)
        # need to update meta
        save_meta(new_table, meta_type = 'electrodes', 
                  project_name = subject$project_name, 
                  subject_code = subject$subject_code)
      }
    }
  }
  
  
  brain
}

# calculate_freeserfer_label <- function(crs, atlas_cube){
#   # get fs labels
#   # atlas_cube <- cube
#   lut <- jsonlite::read_json(system.file('FreeSurferColorLUT.json', package='threeBrain'), simplifyVector = TRUE)
#   lut <- lut$`__global_data__FreeSurferColorLUT`
#   cube_idx <- t(which(atlas_cube != 0, arr.ind = TRUE))
#   label_idx <- apply(crs, 2, function(idx){
#     print(idx)
#     dist <- sqrt(colSums((cube_idx - idx)^2))
#     arg <- unname(as.list(c(0, cube_idx[,which.min(dist)])))
#     arg[[1]] <- quote(atlas_cube)
#     id <- do.call('[', arg)
#     lut[[as.character(id)]]$Label
#   })
# }


#' Import \code{.csv} files that contain electrode information
#' @description The table to import must contains a column \code{'Electrode'}
#' that is consistent with the corresponding subject.
#' @param path path to the electrode file to import
#' @param subject 'RAVE' project-subject combination
#' @param use_fs whether to use 'FreeSurfer', default is to auto-detect
#' @param ... passed to \code{\link[utils]{read.csv}}
#' @export
import_electrodes <- function(path, subject, use_fs = NA, ...){
  
  # get subject
  subject <- as_subject(subject, strict = FALSE)
  
  # get channel info
  electrodes <- subject$preprocess_info('channels')
  
  # read from path
  new_tbl <- utils::read.csv(path, stringsAsFactors = FALSE, ...)
  
  # check
  nms <- names(new_tbl)
  if(!'Electrode' %in% nms){
    stop("`import_electrodes` cannot find column `Electrode` (case-sensitive).")
  }
  new_tbl <- new_tbl[new_tbl$Electrode %in% electrodes, ]
  if(nrow(new_tbl) != length(electrodes)){
    stop("`import_electrodes` cannot import from ", path, 
         "\nThe number of electrodes in file does not match with what's in subject [", 
         subject, "].\n  In table file: ", dipsaus::deparse_svec(new_tbl$Electrode),
         "\n  In subject: ", dipsaus::deparse_svec(electrodes))
  }
  
  # Expand electrode.csv
  has_tkrRAS <- all(c('Coord_x', 'Coord_y', 'Coord_z') %in% nms)
  has_T1RAS <- all(c('T1R', 'T1A', 'T1S') %in% nms)
  has_mni305 <- all(c('MNI305_x', 'MNI305_y', 'MNI305_z') %in% nms)
  has_mni152 <- all(c('MNI152_x', 'MNI152_y', 'MNI152_z') %in% nms)
  
  if(!any(has_tkrRAS, has_T1RAS, has_mni305, has_mni152)){
    dipsaus::cat2("`import_electrodes`: No coordinates found. The coordinates are set to the origin. If you want to import RAS information. Please make sure to have at least one of the following coordinates in your file:\n  T1R, T1A, T1S (scanner T1 RAS)\n  Coord_x, Coord_y, Coord_z (FreeSurfer tkrRAS)\n  MNI305_x, MNI305_y, MNI305_z (MNI305 RAS)\n  MNI152_x, MNI152_y, MNI152_z (MNI152 RAS)\nImporting anyway...", level = 'WARNING')
    
    # save meta
    save_meta(data = new_tbl, meta_type = 'electrode', 
              project_name = subject$project_name, 
              subject_code = subject$subject_code)
    raveio::catgl("`import_electrodes`: Done importing {subject$subject_id} - meta/electrodes.csv. However, the coordinates are blank.")
    return(invisible(NULL))
  }
  
  # priority: T1 > Coord > MNI. 
  # However, not overwrite if overwrite_coords=FALSE
  brain <- NULL
  if(!isFALSE(use_fs)){
    brain <- rave_brain2(subject = subject)
  }
  has_brain <- !is.null(brain)
  
  if(!has_brain){
    if(use_fs){
      stop("`import_electrodes`: `use_fs=TRUE` but FreeSurfer is absent.")
    }
    catgl("`import_electrodes`: FreeSurfer files are missing. Save the electrodes with minimal editing", level = 'WARNING')
    # save meta
    save_meta(data = new_tbl, meta_type = 'electrode', 
              project_name = subject$project_name, 
              subject_code = subject$subject_code)
    return(invisible(NULL))
  }
  
  # ---- now brain exists, try to calculate tkrRAS
  
  if(!has_tkrRAS){
    catgl("FreeSurfer tkrRAS are not detected. Trying to calculate using T1, MNI305, or MNI152. \nOrder: T1 > MNI305 > MNI152.\n", level = 'WARNING')
    
    if(has_T1RAS){
      catgl("T1 RAS detected! T1 -> tkrRAS")
      tmp <- new_tbl[, c("T1R", "T1A", "T1S")]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      # ScannerRAS = Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*% tmp)[, c(1,2,3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c('x', 'y', 'z'))] <- tkRAS
    } else if (has_mni305) {
      catgl("MNI-305 detected! MNI-305 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI305_", c('x', 'y', 'z'))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      # MNI305RAS = TalXFM*Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*% solve(brain$xfm) %*% tmp)[, c(1,2,3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c('x', 'y', 'z'))] <- tkRAS
    } else if (has_mni152){
      # must be MNI-152
      catgl("MNI-152 detected! MNI-152 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI152_", c('x', 'y', 'z'))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      # MNI305RAS = TalXFM*Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
      # MNI152RAS = MNI305_to_MNI152 %*% MNI305RAS
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*% solve(brain$xfm) %*%
                   solve(MNI305_to_MNI152) %*% tmp)[, c(1,2,3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c('x', 'y', 'z'))] <- tkRAS
    } else {
      # Shouldn't happen
      stop("Unexpected error: 0001. Please report to https://github.com/beauchamplab/rave/issues")
    }
  }
  
  # Now tkrRAS has been calculated, calculate all other coords
  tkRAS <- t(cbind(data.matrix(new_tbl[, paste0("Coord_", c('x', 'y', 'z'))]), 1))
  # If electrodes are valid, then RAS are not 0 (at least not exactly 0)
  invalids <- colSums(tkRAS == 0) == 3
  
  if(!has_T1RAS){
    # ScannerRAS = Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
    T1 <- t(brain$Norig %*% solve(brain$Torig) %*% tkRAS)[, c(1,2,3)]
    T1[invalids, ] <- 0
    new_tbl[, c("T1R", "T1A", "T1S")] <- T1
    catgl("T1 has been generated from tkrRAS")
  }
    
  if(!has_mni305){
    # MNI305RAS = TalXFM*Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
    mni305 <- t(brain$xfm %*% brain$Norig %*% solve (brain$Torig) %*% tkRAS)[, c(1,2,3)]
    mni305[invalids, ] <- 0
    new_tbl[, c("MNI305_x", "MNI305_y", "MNI305_z")] <- mni305
    catgl("MNI-305 has been generated from tkrRAS")
  }
  
  # MNI 152 is a little bit different, we directly calculate it from 305
  if(!has_mni152) {
    mni305 <- t(cbind(data.matrix(new_tbl[, c("MNI305_x", "MNI305_y", "MNI305_z")]), 1))
    mni152 <- t(MNI305_to_MNI152 %*% mni305)[, c(1,2,3)]
    mni152[invalids, ] <- 0
    new_tbl[, c("MNI152_x", "MNI152_y", "MNI152_z")] <- mni152
    catgl("MNI-152 has been generated from MNI-305")
  }
  
  if(is.null(new_tbl$Label)){
    new_tbl$Label <- "NoLabel"
  }
  brain$set_electrodes(new_tbl)
  new_tbl2 <- brain$calculate_template_coordinates()
  
  # if(is.null(new_tbl2$FreeSurferLabel)){
  #   catgl("Calculating FreeSurfer labels")
  #   
  #   # Does brain have atlas file? (TODO: use native one)
  #   
  #   # check freesurfer labels
  #   n27 <- threeBrain::merge_brain()$template_object
  #   aparc_aseg <- n27$atlases$aparc_aseg$object
  #   cube <- aparc_aseg$get_data('datacube_value_Atlas - aparc_aseg (N27)')
  #   dm <- aparc_aseg$get_data('datacube_dim_Atlas - aparc_aseg (N27)')
  #   dim(cube) <- dm
  #   # calculate CRS
  #   mni305 <- t(data.matrix(new_tbl[, paste0("MNI305_", c('x', 'y', 'z'))]))
  #   invalids <- colSums(mni305 == 0) == 3
  #   
  #   # mni305 is aligned with N27 atlas
  #   crs <- mni305 + dm / 2
  #   crs <- round(crs)
  #   crs[crs < 1] <- 1
  #   
  #   # fs uses 256x256x256 cube
  #   df <- crs - dm
  #   df[df > 0] <- 0
  #   crs <- df + dm
  #   
  #   # crs: 3x elec
  #   # cube: 256x256x256 cube
  #   new_tbl2$FreeSurferLabel <- calculate_freeserfer_label(crs, cube)
  # }
  
  save_meta(new_tbl2, meta_type = 'electrodes', 
            project_name = subject$project_name, 
            subject_code = subject$subject_code)
  invisible()
}


