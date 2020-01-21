
get_brain = function(surfaces = 'pial', multiple_subject = FALSE){
  subject = get('subject', envir = rave::getDefaultDataRepository())
  brain = rave::rave_brain2(subject = subject, surfaces = surfaces)
  brain
}
