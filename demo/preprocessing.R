# Demo script for pre-processing
require(rave)
require(miniUI)
require(shiny)
require(rhdf5)
require(plotly)


# Subject info:

project_name = 'Congruency'
subject_code = 'YAB'
blocks = '008'
channels = 1:84
srate = 2000

# # Notch
# rave:::pre_process(
#   process_name = 'notch',
#   project_name = project_name,
#   subject_code = subject_code,
#   blocks = blocks,
#   channels = 1:84,
#   ncores = 1                                   # For windows
# )


# Parallel plot
rave:::pre_process_visual(
  process_name = 'CARinspect',
  project_name = project_name,
  subject_code = subject_code,
  blocks = blocks,
  channels = channels,
  srate = srate
)


# Inspect per channel
rave:::pre_process_visual(
  process_name = 'notch',
  project_name = project_name,
  subject_code = subject_code,
  blocks = blocks,
  channels = channels
)

# After saving the filter result

electrodes = rave:::load_meta(
  'electrodes', project_name, subject_code
)

# CAR
# rave:::pre_process(
#   process_name = 'CAR',
#   project_name = project_name,
#   subject_code = subject_code,
#   blocks = blocks,
#   channels = electrodes$Electrode[!(electrodes$Epichan | electrodes$Badchan)]
# )


# inspect comparison before and after CAR
rave:::pre_process_visual(
  process_name = 'CAR',
  project_name = project_name,
  subject_code = subject_code,
  blocks = blocks,
  channels = channels
)


# wavelet
rave:::pre_process(
  process_name = 'wavelet',
  project_name = project_name,
  subject_code = subject_code,
  blocks = blocks,
  channels = 20#electrodes$Electrode[!(electrodes$Epichan | electrodes$Badchan)]
)




# env = load_preprocess_module('./inst/preprocess/dev_test.R')



