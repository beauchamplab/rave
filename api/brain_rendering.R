
## Sample toy
# values = data.frame(
#   Project = 'demo',
#   subject = 'YAB',
#   Electrode = 1:20,
#   Value1 = 1:10,
#   Value2 = 10:1
# )

project = 'demo'
subject = 'YAB'
value_path = 'path to your data'
save_to = '~/Desktop/junk/'

# csv import
values = read.csv(value_path, stringsAsFactors = FALSE)

# merge with electrodes.csv
merged = merge(values, rave::load_meta(meta_type = 'electrodes', project_name = project, subject_code = subject), by = 'Electrode', suffixes = c('', '_alt'))

# 3d viewer
brain <- rave::rave_brain2(sprintf('%s/%s', project, subject))
brain$set_electrode_values(merged)
wg = brain$plot()
threeBrain::save_brain(wg, save_to)
