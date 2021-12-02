#' Electrode localization
#' @param subject_code 'RAVE' subject code
#' @param freesurfer_path 'FreeSurfer' folder path that points to reconstructed
#' subject brain
#' @param ct_path 'CT' path (in 'Nifti' format). The 'CT' has to be aligned to 
#' 'T1-MRI'. Please check \href{https://dipterix.org/threeBrain/articles/B-electrode-localization.html#ct-co-registration}{this tutorial}.
#' @param ... other parameters passing to \code{\link[threeBrain]{localization_module}}
#' @return This function will launch a shiny application.
#' @export
electrode_localization <- function(subject_code, freesurfer_path, ct_path, ...) {
  module <- threeBrain::localization_module(
    subject_code = subject_code,
    fs_path = freesurfer_path, 
    ct_path = ct_path,
    ...
  )
  print(module$app)
}
