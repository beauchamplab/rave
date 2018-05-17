context('[Test - rave_preprocess_tools]    ')

require(rave)
require(shiny)
require(magrittr)
require(stringr)
require(assertthat)
require(rhdf5)

utils = rave_preprocess_tools()
subject_code = 'YAB'
project_name = 'Congruency_3'



test_that('[tools] Load subject', {
  expect_false(utils$has_subject())
  expect(utils$get_check_level() == 0, failure_message = 'Check level is not 0 when no subject is loaded.')
  preprocess_dir = get_dir(subject_code, project_name)$preprocess_dir
  if(!dir.exists(preprocess_dir)){
    expect_error(utils$load_subject(subject_code = subject_code, project_name = project_name))
    utils$save_subject(subject_code = subject_code, project_name = project_name)
  }else{
    utils$load_subject(subject_code = subject_code, project_name = project_name)
  }
  expect({utils$has_subject()}, failure_message = 'subject not loaded')
  expect(utils$get_check_level() > 0, failure_message = 'Check level is 0 when subject is loaded')
})

test_that('[tools] Subject attributes', {
  expect(utils$get_subject_id() == project_name %&% '/' %&%subject_code, 'Check get_subject_id')
  expect(utils$get_check_level() %in% 0:4, 'Check level invalid range')

  if(utils$get_check_level() == 1){
    expect_false(utils$set_srate(0))
    expect(utils$set_srate(1), 'Cannot set sample rate to 1')
    expect(utils$set_channels('1-80', name = 'channels'), 'Cannot set channels with text')
    expect(utils$set_channels(1:83, name = 'channels'), 'Cannot set channels with numeric vector')
    expect(utils$set_channels(paste(1:84), name = 'channels'), 'Cannot set channels with string vector')
    expect(utils$set_srate(2000), 'Cannot set sample rate to 2000')
    expect(utils$set_blocks(c('008')), failure_message = 'cannot set blocks')
    expect(utils$set_blocks(c('008', '010')), failure_message = 'cannot set blocks')
    expect(!utils$set_blocks(c('008', '010')))
    expect(!utils$set_blocks(c(NULL)))
  }else{
    expect_false(utils$set_blocks('008'))
    expect_false(utils$set_channels('1,4,6,7,8,10-99', 'channels'))
    expect_false(utils$set_srate(10))
  }
  expect(utils$get_srate() > 0, "Can't get sample rate")

  utils$add_to_channel(10, 2, notification = T)
  expect(utils$get_channel_type(10, as_text = T) == 'Epilepsy', 'Channel 10 set to epi, but detected')
  utils$add_to_channel(13, 3)
  expect(utils$get_channel_type(13) == 3, 'Channel 13 set to bad, but detected')

  expect(utils$get_channel_type(10, as_text = F) %in% 1:4, 'Channel type invalid')

  excl_tbl = utils$get_excluded_time()
  expect(is.data.frame(excl_tbl), 'get_excluded_time not returning data frame')

  ref_table = utils$get_reference_table()
  expect(is.data.frame(ref_table), 'get_reference_table not returning data frame')
})



test_that('[tools] Notch filter', {
  chans = 10:15
  utils$set_channels(chans, 'channels')
  if(!utils$notch_filtered()){
    utils$apply_notch(60 * (1:3), c(1,2,2))
  }

  s = utils$load_signal('008', channel = 14, group = 'notch')
  expect(length(s) > 60000, 'Load voltage signal failed')


  s = utils$load_compressed_notch_signal(block = '008', channels = chans, from = 0, to = 1, compress_level = 1)
  expect_equivalent(dim(s), c(length(chans), utils$get_srate()))
})


test_that('[tools] Wavelet', {
  freqs = seq(2,200,by=20)
  if(!utils$waveleted()){
    utils$apply_wavelet(channels = chans, target_srate = 100, frequencies = freqs, ncores = 7, wave_num = c(3,20))
  }

  wavelet_log = utils$get_wavelet_log()
  expect(wavelet_log$target_srate == 100, 'Cannot get wavelet log - sample rate.')

  s = utils$load_wave_data(block = '008', chl = 10, complex = T, raw = T, time_points = 1:2000)
  expect_equivalent(dim(s), c(length(freqs), 2000))
  expect(is.complex(s), 'Wavelet is not complex?')
})


test_that('[tools] Reference', {
  utils$set_reference_group(g_name = 'Epi', g_channels = 10:12, g_type = 'bipolar')
  utils$set_reference_group(g_name = 'CAR', g_channels = 13:15, g_type = 'car', init = T)

  expect_equivalent(utils$get_channel_type(10:15), c(1,1,1,3,1,1), expected.label = ' - Channel type wrong')

  utils$set_reference_group(g_name = 'Epi', g_channels = 10:12, g_type = 'bipolar', bp_e1 = 10, bp_e2 = 11)
  utils$set_reference_group(g_name = 'Epi', g_channels = 10:12, g_type = 'bipolar', bp_e1 = 11, bp_e2 = 12)

  utils$get_reference_table()

  utils$apply_ref()

})

#rm(utils, subject_code, project_name)




