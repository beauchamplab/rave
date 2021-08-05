library(rave)
# Import already preprocessed data into RAVE

# Step 1: subject settings and implementations
# You need to implement some details in this step

# TODO: set sample rate for voltage and wavelet, in addition, set frequencies
subject_info = list(
  subject_code = 'YAB',
  project_name = 'test',
  blocks = c('008'),
  electrodes = c(1:4),
  sample_rate_volt = 2000,
  sample_rate_wave = 100,
  frequencies = 1:100,
  wave_num = c(3,20)  # This one is not that important
)

load_data = function(block, electrode){
  # ---------------------------------------------------------------------------
  # TODO: You should implement this function to read from your data
  # for given block and electrode


  # Return a list, replace NULLs with whatever data you have. If you don't
  # have all data you need, then the reference module won't work, i.e.
  # I would assume that you've already reference the data
  list(
    # Voltage: a vector of length L, or 1xL matrix. If you have 300 seconds
    # while the sample rate of original voltage signal is 1000 Hz,
    # L = 300 x 1000 = 300,000
    voltage = NULL,

    # Power, Phase: matrices of dimension FxD, where F is number of frequecies
    # and D is time points. For example, a block lasts for 300 seconds, and
    # Downsampled rate after wavelet/fieldtrip/hilbert transformations is 100 Hz,
    # then D = 300 x 100 = 30,000

    # Power is the amplitude, for wavelet, it's the wavelet coefficient's 2nd
    # power. (power = coef * coef)
    power = NULL,

    # Phase is from -pi to pi (pi=3.1415926), is the arctan of wavelet
    # coefficients
    phase = NULL
  )
  # ---------------------------------------------------------------------------
}



# ---------------------------------------------------------------------------
# Step 2: create subject
utils = rave_preprocess_tools()

rave:::stopifnot2(
  !utils$has_raw_cache(),
  msg = 'WARNING!!! You already have the subject!')

utils$create_subject(subject_code = subject_info$subject_code, project_name = subject_info$project_name)
utils$set_blocks(blocks = subject_info$blocks)
utils$set_electrodes(electrodes = subject_info$electrodes)
utils$set_sample_rate(srate = subject_info$sample_rate_volt)

# ---------------------------------------------------------------------------
# Step 3: migrate data
dirs = utils$get_from_subject('dirs')
for(e in subject_info$electrodes){
  fname = sprintf('%d.h5', e)
  for(b in subject_info$blocks){
    dat = load_data(block = b, electrode = e)
    if(!is.null(dat$voltage)){
      # write voltage to e.h5
      d = file.path(dirs$cache_dir, 'voltage', fname)
      save_h5(as.vector(dat$voltage), file = d, name = sprintf('/raw/voltage/%s', b), chunk = 1024, replace = T)
      save_h5(as.vector(dat$voltage), file = d, name = sprintf('/ref/voltage/%s', b), chunk = 1024, replace = T)
      save_h5('noref', file = d, name = '/reference')
    }

    if(!is.null(dat$power)){
      # write voltage to e.h5
      d = file.path(dirs$cache_dir, 'power', fname)
      save_h5(as.matrix(dat$power), file = d, name = sprintf('/raw/power/%s', b), replace = T)
      save_h5(as.matrix(dat$power), file = d, name = sprintf('/ref/power/%s', b), replace = T)
      save_h5('noref', file = d, name = '/reference')
    }

    if(!is.null(dat$phase)){
      # write voltage to e.h5
      d = file.path(dirs$cache_dir, 'phase', fname)
      save_h5(as.matrix(dat$phase), file = d, name = sprintf('/raw/phase/%s', b), replace = T)
      save_h5(as.matrix(dat$phase), file = d, name = sprintf('/ref/phase/%s', b), replace = T)
      save_h5('noref', file = d, name = '/reference')
    }

  }
}

utils$save_to_subject(wavelet_log = list(
  list(
    channels = subject_info$electrodes, # soft-depricated, use electrodes instead
    electrodes = subject_info$electrodes,
    target_srate = subject_info$sample_rate_wave,
    frequencies = subject_info$frequencies,
    wave_num = subject_info$wave_num
  )
))

# Mark subject as waveleted
utils$save_to_subject(checklevel = 3)


# save subject meta data
dir.create( file.path(dirs$channel_dir, 'cache'), recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(
  Electrode = subject_info$electrodes,
  Reference = 'noref'
), file.path(dirs$channel_dir, 'cache', 'cached_reference.csv'),
row.names = F)
utils$gen_meta()

# ---------------------------------------------------------------------------
# Step 4: generate epoch files
# Now you have to provide epoch files and save to 'meta' folder as epoch_xyz.csv



