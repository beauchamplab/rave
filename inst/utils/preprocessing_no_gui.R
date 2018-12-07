library(rave)

# preprocessing with no gui

utils = rave_preprocess_tools()
# settings
subject_info = list(
  subject_code = 'YAB',
  project_name = 'test',
  blocks = c('008'),#, '010', '011', '012'),
  electrodes = c(1:4),
  sample_rate = 2000,
  notch_filter = list(
    default = list(
      centers = c(60,120,180),
      widths = c(1,2,2)
    ),
    # You can specify different notch filters for different blocks
    '008' = list(
      centers = c(60,120,180),
      widths = c(1,2,2)
    )
  ),
  wavelet = list(
    downsample_to = 100,
    frequencies = seq(2, 200, by = 2), # 2, 4, 6, ...., 200
    wave_num = c(3,20)
  ),
  # Important, if you don't set this correctly, your computer might crash!
  # Basically check your RAM (memory size), and CPU cores.
  # if you have >=32GB RAM, ncores = 4
  # If you have 64GB RAM and i7 CPU (8 cores), ncores = 8
  # For small RAM such as 16GB, ncores = 2
  ncores = 4
)

# Step 0: load/create subject
utils$create_subject(subject_code = subject_info$subject_code, project_name = subject_info$project_name)

# Alternatively if you have created subject
# tool$load_subject(subject_code = 'YAB', project_name = 'test')

# Step 1: set subject channels(electrodes), blocks and sample rate
utils$set_blocks(blocks = subject_info$blocks)
utils$set_electrodes(electrodes = subject_info$electrodes)
utils$set_sample_rate(srate = subject_info$sample_rate)


# Step 2: import subject data from matlab if not cached yet
if(!utils$has_raw_cache()){
  utils$collect_raw_voltage(force = F)
}

# Step 3: notch filter and diagnose
# Export voltage diagnostic plots, not necessary, but gives you ideas of notch filter settings
# rave::export_diagnose_voltage(subject = utils$get_subject_id(),
#                               save_dir = file.path('~/Desktop/junk/', subject_info$subject_code),
#                               electrodes = subject_info$electrodes)


utils$apply_notch(bandwidths = subject_info$notch_filter)

# Export disgnostic plots
rave::export_diagnose_voltage(subject = utils$get_subject_id(),
                              save_dir = file.path('~/Desktop/junk/', subject_info$subject_code),
                              electrodes = subject_info$electrodes)

# Step 4: Wavelet
utils$apply_wavelet(electrodes = subject_info$electrodes,
                    target_srate = subject_info$wavelet$downsample_to,
                    frequencies = subject_info$wavelet$frequencies,
                    wave_num = subject_info$wavelet$wave_num, ncores = subject_info$ncores)

# Just in case, if you don't see meta files
# utils$gen_meta()


# Step 5: generate epoch files (Open GUI and this should be fast)
