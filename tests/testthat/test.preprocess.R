# devtools::load_all(); require(testthat)
context("Check preprocess pipeline with YAB")

rave::arrange_data_dir(reset = TRUE)

raw_dir = rave_options('raw_data_dir')
has_yab = FALSE
if(dir.exists(raw_dir)){
  subjects = list.dirs(raw_dir, full.names = FALSE, recursive = FALSE)
  has_yab = 'YAB' %in% subjects
}
skip_if_not(has_yab, message = 'Found raw YAB')


test_with_no_output = function(msg, expr){
  expr = substitute(expr)
  test_that(msg, {
    capture.output({
      force(expr)
    }, type = 'output')
  })
}

# Check the preprocess pipeline
test_that('Preprocess pipeline with YAB', {
  
  capture.output({
    utils = rave_preprocess_tools()
    
    data_dir = rave_options('data_dir')
    raw_dir = rave_options('raw_data_dir')
    on.exit({
      rave_options(data_dir = data_dir, raw_data_dir = raw_dir)
    })
    
    tmp_dir = tempfile()
    rave:::dir_create(file.path(tmp_dir, 'junk'))
    
    rave_options(data_dir = tmp_dir)
    
    
    
    expect_error(utils$load_subject(subject_code = 'YAB11kgkauyfrb', project_name = 'junk'))
    
    expect_success({
      utils$check_load_subject(subject_code = 'YAB11kgkauyfrb', project_name = 'junk')
      expect_true(TRUE)
    })
    
    # create with YAB
    expect_true(utils$create_subject(subject_code = 'YAB', project_name = 'junk'))
    expect_false(utils$create_subject(subject_code = 'YAB', project_name = 'junk'))
    
    # set sample rate, block, and channels
    expect_true(utils$set_blocks('008'))
    expect_false(utils$set_blocks('008'))
    
    expect_true(utils$set_electrodes('14,15,35'))
    expect_true(utils$set_electrodes(14:15))
    expect_false(utils$set_electrodes(14:15))
    
    # set sampling rate
    expect_true(utils$set_sample_rate(2000))
    expect_false(utils$set_sample_rate(2000))
    
    # Check level
    expect_equal(utils$get_check_level(), 0)
    expect_false(utils$has_raw_cache())
    expect_false(utils$notch_filtered())
    expect_false(utils$waveleted())
    
    # load data
    utils$collect_raw_voltage()
    expect_equal(utils$get_check_level(), 1)
    expect_true(utils$has_raw_cache())
    expect_false(utils$notch_filtered())
    expect_false(utils$waveleted())
    
    expect_false(utils$set_blocks('010'))
    expect_false(utils$set_electrodes(14:16))
    expect_true(utils$set_srate(100))
    expect_true(utils$set_srate(2000))
    
    # Notch filter
    utils$apply_notch()
    expect_equal(utils$get_check_level(), 2)
    expect_true(utils$has_raw_cache())
    expect_true(utils$notch_filtered())
    expect_false(utils$waveleted())
    expect_false(utils$set_srate(100))
    
    ## TODO: check file is complete and cache exists
    # dirs = get_dir(subject_code = 'YAB', project_name = 'junk')
    # 
    # file.path(tmp_dir, 'junk', 'YAB')
    
    
    
    # apply wavelet
    test_wavelet = rave_options('test_wavelet')
    skip_if(isFALSE(test_wavelet), message = 'Skipping wavelet flag')
    rave_options('test_wavelet' = FALSE)
    utils$apply_wavelet(electrodes = 14, target_srate = 100, frequencies = 1, wave_num = 1, ncores = 1)
    
    expect_equal(utils$get_check_level(), 3)
    expect_true(utils$has_raw_cache())
    expect_true(utils$notch_filtered())
    expect_true(utils$waveleted())
  })
  
})
