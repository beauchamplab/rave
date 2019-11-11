#' Tools to load and view brain in 3D viewer
#' @param subject character or `RAVE` subject instance
#' @param surfaces one or more from `pial`, `white`, `smoothwm`, brain surface types
#' @param use_141 logical, whether to use standard 141 brain
#' @param compute_template logical whether to compute nearest 141 node. Please 
#' also check \code{freesurfer_brain}.
#' @export
rave_brain2 <- function(subject, surfaces = 'pial', use_141 = TRUE, compute_template = FALSE){
  
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
  if( is.null(fs_path) ){
    return(invisible())
  }
  
  # import from freesurfer folder
  brain = threeBrain::freesurfer_brain2(
    fs_subject_folder = fs_path, subject_name = subject$subject_code, 
    surface_types = surfaces, use_141 = use_141)
  
  # load electrodes
  electrode_table = load_meta('electrodes', 
                              project_name = subject$project_name, 
                              subject_code = subject$subject_code)
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
  
  brain
}

