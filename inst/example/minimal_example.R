require(rafe)


rafe_opts$set_options(
  data_dir = '/Users/jmagnotti/Dropbox/MultisensoryIntegration/ecog/rafe/data'
)
subject_id = 'subject_lij118_ChBr'

attach_virtualenv(subject_id, 70:90, module_path = '/Users/jmagnotti/Dropbox/rafe/inst/modules/condition_explorer.R')

ls(data_env)

data_env$subject$electrodes



# my_data <- apply_to_electrodes(function(content, ...) {
#
#   # need to specify what the output type is
#   apply(content[1:10, 50:150, ], 1, colMeans)
#
# }, rafe:::prophet$find_all_subjects(), list(1:10, 1:10))
#
#
# my_data %>% set_colnames('subj', 'elec', paste0('g', 0:300))
#
# write.csv(my_data, 'over_time_avg.csv')


##########

# data.frame('subj'=LETTERS[1:10], 'elec'=letters[1:5], val=)
