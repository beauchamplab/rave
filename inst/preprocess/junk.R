# This script is testing use for pre-processing

### Load functions
source('./adhoc/Dipterix/preprocess/plot_signal.R')
source('./adhoc/Dipterix/preprocess/pre_import_signal.R')
source('./adhoc/Dipterix/preprocess/pre_load_settings.R')
source('./adhoc/Dipterix/preprocess/pre_resample.R')
source('./adhoc/Dipterix/preprocess/pre_notch_filter.R')
source('./adhoc/Dipterix/preprocess/pre_wavelet.R')

require(rave)
require(tidyverse)
rave_opts$set_options(
  raw_data_dir = './adhoc/Dipterix/Data/ECoG/'
)

subject_code = 'YAB'
block_num = '008'
chl = 70

# Load settings
settings = pre_load_settings(subject_code)
block = settings$blocks[[block_num]]
srate = block[['ecog_srate']]

# import signal

signal = pre_import_matlab(subject_code, block_num, chl)


# Notch filter
filtered = notch_channel(signal, srate)

# Compress data
compressed = compress_signal(filtered, 1,block$compress,1)


# Diagnose
# Raw signal
plot_signal(filtered, srate, boundary = 1000, details = T, xbins = 200)

# spec heat map
signal::specgram(filtered, Fs = srate, overlap = 8, window = 128)

# pwelch
pwelch(filtered, fs = srate, window = 128, noverlap = 8, plot = 1, col = 'blue', log = 'y', xlim = c(0, 300))
pwelch(filtered, fs = srate, window = 128, noverlap = 8, plot = 1, col = 'blue', log = 'xy', xlim = c(0, 2.5))

# Compare
pwelch(filtered, fs = srate, window = 128, noverlap = 8, plot = 1, col = 'red', log = 'y', xlim = c(0, 300))
pwelch(signal, fs = srate, window = 128, noverlap = 8, plot = 2, col = 'blue', log = 'y', xlim = c(0, 300))



compressed = compress_signal(filtered, 1,2000,1)

ss = rbind(
  compressed,
  compressed,
  compressed,
  compressed,
  compressed,
  compressed,
  compressed,
  compressed,
  compressed,
  compressed
)
ss = rbind(ss,ss,ss,ss,ss,ss,ss,ss,ss,ss)

plot_signals(
  ss, sample_rate = 1, col = 1, space = 0.999, plot = 'ggplotly'
)


wd = wavelet(filtered, seq(4,200, 4), srate, 7)

