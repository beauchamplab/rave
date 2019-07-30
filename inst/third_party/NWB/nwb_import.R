library(pryr)
library(rave)

# Please install rave@dev-0.1.6.1
# You might want to use devtools::install_github('beauchamplab/rave@dev-0.1.6.1')

# Register functions
source(system.file('third_party/NWB/nwb_cls.R', package = 'rave'))
source(system.file('third_party/NWB/nwb_parser.R', package = 'rave'))

# Load files
nwb_file = '~/rave_data/raw_dir/NWBDEMO/EC125_B22.nwb'
nwbfile = NWB_parser(nwb_file)

# -------------------------------- Overview --------------------------------

project_name = 'NWB_DEMO'

# find subject information
nwbsubject = nwbfile$general$subject; nwbsubject

# Get subject code
subject_code = nwbsubject$subject_id[]

directories = get_dir(subject_code, project_name, block_num = block, mkdirs = c(
  'block_dir', 'preprocess_dir', 'channel_dir', 'subject_dir', 'suma_dir', 'suma_out_dir', 'meta_dir'
))
viewer_dir = file.path(directories$rave_dir, 'viewer')
dir.create(viewer_dir, recursive = T, showWarnings = F)

# -------------------------------- Collect session info --------------------------------

# Block info
block = nwbfile$session_description[]

# Electrodes
electrodes = nwbfile$acquisition$ECoG$electrodes[]

# Sample rate
sample_rate = attr(nwbfile$acquisition$ECoG$starting_time, 'rate')


# -------------------------------- Create RAVE subject (preprocess) --------------------------------
env = new.env()
rave_preproc = rave_preprocess_tools(env)
rave_preproc$create_subject(subject_code = subject_code, project_name = project_name)


# Set up basic information
# rave_preproc$set_sample_rate(sample_rate)
env$subject$srate = sample_rate
env$subject$save(action = "Set Sample Rate", message = paste(sample_rate))
rave_preproc$get_sample_rate()

rave_preproc$set_blocks(block)
rave_preproc$get_blocks()

rave_preproc$set_electrodes(electrodes)
rave_preproc$get_electrodes()

# Import analogtraces to RAVE
traces = nwbfile$acquisition$ECoG$data

# Retrieve viltage directory
volt_dir = file.path(directories$preprocess_dir, 'voltage')

lapply_async(seq_along(electrodes), function(ii){
  e = electrodes[[ii]]
  trace = drop(traces[ii, ])
  # write to disk
  save_h5(trace, file.path(volt_dir, sprintf('electrode_%d.h5', e)), name = sprintf('/raw/%s', block))
}, .call_back = function(ii){
  cat('\t\t\t\t\t\rRunning:', ii, 'of', length(electrodes))
})

rave_preproc$save_to_subject(checklevel = 1)
rave_preproc$has_raw_cache()

# -------------------------------- Write Meta --------------------------

# We need electrode, epoch, and reference
# 1. Electrode
tmp_env = new.env(); tmp_env$tbl = data.frame(Electrode = electrodes, stringsAsFactors = FALSE)
elec_table = nwbfile$general$extracellular_ephys$electrodes
msg = sapply(names(elec_table), function(nm){
  col = elec_table[[nm]][]
  if(is.atomic(col)){
    tmp_env$tbl[[nm]] = col
  }
  NULL
})
knitr::kable(head(tmp_env$tbl))

# electrode.csv
elec_table = data.frame(
  Electrode = tmp_env$tbl$Electrode,
  Coord_x = tmp_env$tbl$x,
  Coord_y = tmp_env$tbl$y,
  Coord_z = tmp_env$tbl$z,
  Label = tmp_env$tbl$location
)
save_meta(elec_table, meta_type = 'electrodes', project_name = project_name, subject_code = subject_code)


# 2. reference.csv
ref_table = data.frame(
  Electrode = tmp_env$tbl$Electrode,
  Group = tmp_env$tbl$group_name,
  Reference = 'noref',
  Type = 'No Reference'
)
write.csv(ref_table, file.path(directories$meta_dir, 'reference_nwb.csv'), row.names = FALSE)

# 3. epoch information
trials = nwbfile$intervals$trials
onset = trials$start_time[]
trial_tbl = data.frame(
  Block = block,
  Time = onset,
  Trial = seq_along(onset),
  Condition = seq_along(onset),
  Duration = trials$stop_time[] - onset,
  stringsAsFactors = FALSE
)
write.csv(trial_tbl, file.path(directories$meta_dir, 'epoch_nwb.csv'), row.names = FALSE)


# 4. Surfaces
surfaces = nwbfile$general$subject$cortical_surfaces
# Write lh.pial
vert = t(surfaces$lh_pial$vertices[]); dim(vert)
face = t(surfaces$lh_pial$faces[]); dim(face)

fname = file.path(directories$suma_dir, 'std.141.lh.pial.asc')
f = file(fname, 'w')
writeLines(sprintf('%d %d', nrow(vert), nrow(face)), f)
msg = apply(vert, 1, function(x){
  writeLines(sprintf('%f %f %f', x[1],x[2],x[3]), f)
})
msg = apply(face, 1, function(x){
  writeLines(sprintf('%f %f %f', x[1],x[2],x[3]), f)
})
close(f)

# rh.pial
vert = t(surfaces$rh_pial$vertices[]); dim(vert)
face = t(surfaces$rh_pial$faces[]); dim(face)
fname = file.path(directories$suma_dir, 'std.141.rh.pial.asc')
f = file(fname, 'w')
writeLines(sprintf('%d %d', nrow(vert), nrow(face)), f)
msg = apply(vert, 1, function(x){
  writeLines(sprintf('%f %f %f', x[1],x[2],x[3]), f)
})
msg = apply(face, 1, function(x){
  writeLines(sprintf('%f %f %f', x[1],x[2],x[3]), f)
})
close(f)
# -------------------------- Start preprocess --------------------------
rave::rave_preprocess()
