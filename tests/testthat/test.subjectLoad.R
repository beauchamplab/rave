# devtools::load_all(); require(testthat)

context("Check with demo/YAB subject")

rave::arrange_data_dir(reset = TRUE)

utils::capture.output({
  projects = get_projects()
  has_demo = 'demo' %in% projects
  subjects = get_subjects('demo', check_subfolders = FALSE, check_rawdata = FALSE)
  has_yab = 'YAB' %in% subjects
  demo_checks = check_subjects2(project_name = 'demo', subject_code = 'YAB', quiet = FALSE)
})

skip_if_not(has_demo, message = 'Has demo project')
skip_if_not(has_yab, message = 'Found demo/YAB')

check_with_subject = function(expr){
  
  utils::capture.output({
    force(expr)
  }, type = 'output')
  
}



# Check the normal processing pipeline
test_that('Checkers - checks.R', {
  
  check_with_subject({
    res = demo_checks
    
    # Must have exactly the same names
    expect_named(res, c("check","log","epochs","references"))
    
    epochs = res$epochs
    refs = res$references
    
    expect_gt(length(epochs), 0)
    expect_gt(length(refs), 0)
    
    expect_true(check_epoch('demo/YAB', epoch_name = epochs[[1]]))
    er = check_epoch('demo/YAB', epoch_name = 'thisasdafgakrfgik')
    expect_is(er, 'rlang_error')
    
    # check is all logical
    expect_true(all(sapply(res$check, function(x){ length(x) == 1 && is.logical(x) })))
    
    res = check_subjects_old(project_name = 'demo', subject_code = 'YAB')
    expect_true(is.environment(res$adapter))
    
    expect_false(res$error)
  })
  
})


test_that('Class Subject with demo/YAB', {
  check_with_subject({
    reference = demo_checks$references[[1]]
    
    
    subject = Subject$new(project_name = 'demo', subject_code = 'YAB', 
                          reference = reference, strict = FALSE)
    
    expect_is(subject, 'Subject')
    
    subject = as_subject(subject = 'demo/YAB', strict = FALSE, reference = reference)
    
    expect_is(subject, 'Subject')
    
  })
})

test_that('Function load_metas with demo/YAB', {
  check_with_subject({
    epoch = demo_checks$epochs[[1]]
    reference = demo_checks$references[[1]]
    
    d = load_meta(meta_type = 'electrodes', project_name = 'demo', subject_code = 'YAB')
    expect_is(d, 'data.frame')
    
    d = load_meta(meta_type = 'time_points', subject_id = 'demo/YAB')
    expect_is(d, 'data.frame')
    
    d = load_meta(meta_type = 'frequencies', subject_id = 'demo/YAB')
    expect_is(d, 'data.frame')
    
    d = load_meta(meta_type = 'references', subject_id = 'demo/YAB', meta_name = reference)
    expect_is(d, 'data.frame')
    
    d = load_meta(meta_type = 'epoch', subject_code = 'YAB', project_name = 'demo', meta_name = epoch)
    expect_is(d, 'data.frame')
    
  })
})

test_that('Class Electrode with demo/YAB', {
  check_with_subject({
    epoch = demo_checks$epochs[[1]]
    reference = demo_checks$references[[1]]
    # Electrode
    
    e = Electrode$new(subject = 'demo/YAB', electrode = 14)
    expect_is(e, 'Electrode')
    expect_true(length(e$blocks) > 0)
    expect_equivalent(e$electrode, 14)
    
    # Check power
    d = e$epoch(epoch_name = epoch, pre = 1, post = 2, types = 'power', raw = TRUE, hybrid = FALSE)
    
    ep = load_meta(meta_type = 'epoch', subject_code = 'YAB', project_name = 'demo', meta_name = epoch)
    fq = load_meta(meta_type = 'frequencies', subject_id = 'demo/YAB')
    sub = as_subject('demo/YAB', strict = FALSE, reference = reference)
    dm = c(nrow(ep), nrow(fq), sub$sample_rate * 3 + 1, 1)
    
    # hybrid is disabled
    expect_equivalent(dim(d$power$.__enclos_env__$private$.data), dm)
    expect_true(setequal(names(e$raw_power), sub$preprocess_info('blocks')))
    
    # Check phase
    d = e$epoch(epoch_name = epoch, pre = 1, post = 2, types = 'phase', raw = FALSE, hybrid = FALSE)
    expect_equivalent(dim(d$phase$.__enclos_env__$private$.data), dm)
    
    # check voltage
    d = e$epoch(epoch_name = epoch, pre = 1, post = 2, types = 'volt', raw = FALSE, hybrid = FALSE)
    dm1 = dm
    dm1[3] = 1 + 3 * sub$preprocess_info('srate')
    dm1 = dm1[-2]
    # expect_equivalent(dim(d$volt$.__enclos_env__$private$.data), dm1)
    
    # clean e
    e$clean(types = c(
      'power', 'phase', 'volt',
      'raw_power', 'raw_phase', 'raw_volt'
    ), force = TRUE)
    expect_equal(length(e$phase), 0)
    expect_equal(length(e$raw_power), 0)
    
  })
  
  
})

test_that('Class ECoGRepository with demo/YAB', {
  check_with_subject({
    epoch = demo_checks$epochs[[1]]
    reference = demo_checks$references[[1]]
    
    # Get subject
    sub = as_subject('demo/YAB', reference = reference)
    
    # Create repository
    repo = ECoGRepository$new(subject = sub, reference = reference, autoload = FALSE)
    
    # load electrode
    repo$load_electrodes(electrodes = 14:15)
    
    # Epoch data
    repo$epoch(epoch_name = epoch, pre = 1, post = 2, electrodes = 14:15, 
               data_type = 'power', referenced = TRUE)
    
    # list2env(list(epoch_name = epoch, pre = 1, post = 2, electrodes = 14:15, 
    #               data_type = 'power', referenced = TRUE), envir = .GlobalEnv)
    
    # Check if power exists
    expect_is(repo$power, 'ECoGTensor')
    
    # Check under special case, the file size
    expect_length(repo$power$swap_file, 2)
    expect_true(all(file.exists(repo$power$swap_file)))
    
    # Baseline
    bl = baseline(repo$power, from = -0.5, to = -0.2)
    expect_is(bl, 'Tensor')
    bl = bl$subset(Trial = Trial == 1, Time = Time < 0, Frequency = Frequency > 100,
              Electrode = Electrode == 14, data_only = TRUE)
    
    expect_true(length(bl) > 0 && all(!is.na(bl)))
    
    
    # Check voltage
    repo$epoch(epoch_name = epoch, pre = 0.5, post = 1, electrodes = 14:15, 
               data_type = 'volt', referenced = TRUE)
    expect_is(repo$volt, 'Tensor')
    
  })
})

test_that('Function rave_prepare with demo/YAB', {
  check_with_subject({
    epoch = demo_checks$epochs[[1]]
    reference = demo_checks$references[[1]]
    env = new.env(parent = parent.env(globalenv()))
    rave::rave_prepare(
      subject = 'demo/YAB', electrodes = c(14,15),
      time_range = c(1,2), epoch = epoch, 
      reference = reference, attach = FALSE, 
      data_types = c(
        'raw_power', 'raw_phase', 'raw_volt',
        'power', 'phase', 'volt'
      ), data_env = env)
    
    d = env$module_tools$get_electrode(electrode = 14, type = 'power')
    expect_is(d, 'ECoGTensor')
    
    d = env$module_tools$get_meta('trials')
    expect_is(d, 'data.frame')
    expect_true(all(c(
      'Block', 'Time', 'Trial', 'Condition'
    ) %in% names(d)))
    
    d = env$module_tools$get_meta('electrodes')
    expect_is(d, 'data.frame')
    expect_true(all(c(
      'Electrode', paste0('Coord_', c('x','y','z')), 'Label'
    ) %in% names(d)))
    
    expect_gt(env$module_tools$get_sample_rate(original = FALSE), 0)
    expect_equivalent(
      env$subject$sample_rate,
      env$module_tools$get_sample_rate(original = FALSE)
    )
    expect_gt(env$module_tools$get_sample_rate(original = TRUE), 0)
    expect_equivalent(
      env$subject$preprocess_info('srate'),
      env$module_tools$get_sample_rate(original = TRUE)
    )
    
    
    expect_true(env$module_tools$is_loaded(data_type = 'power'))
    
    env$module_tools$get_power(referenced = FALSE)
    expect_true(env$module_tools$is_loaded(data_type = 'raw_power'))
    
    # Because env is not default data repo, rave_checks will just raise errors
    # TODO: fix this
    expect_error(rave:::rave_checks('raw power', 'referenced power'))
    
    # Transfer items to default repo. The trick is to save the items in env and 
    # load it
    tmpfile = tempfile()
    save(list = c('.private', '.module_data', 'subject', 'preload_info', 'data_check'), 
         envir = env, file = tmpfile, eval.promises = TRUE, precheck = TRUE)
    
    data_env = getDefaultDataRepository()
    if(exists('module_tools', envir = data_env, inherits = FALSE)){
      rm('module_tools', envir = data_env)
    }
    load(tmpfile, envir = data_env)
    unlink(tmpfile)
    
    data_env$module_tools = rave_module_tools(data_env = data_env)
    
    expect_null(rave:::rave_checks('raw power', 'referenced power')$quos)
    
    data_env$module_tools$clean(items = 'power')
    expect_null(rave:::rave_checks('raw power')$quos)
    
    data_env$module_tools$clean(items = 'raw_power')
    # No
    expect_error(rave:::rave_checks('raw power'))
    
  })

})


