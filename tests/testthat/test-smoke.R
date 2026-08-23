test_that("the three launchers are exported", {
  for (fn in c("start_rave", "start_rave2", "start_yael")) {
    expect_true(is.function(getExportedValue("rave", fn)), info = fn)
  }
})


test_that("internal helpers remain reachable in the namespace", {
  # `ravecore` calls these via asNamespace("rave")$...
  for (fn in c("download_sample_data", "download_subject_data",
               "rave_options", "save_options", "arrange_data_dir",
               "rave_version", "get_val", "catgl", "%?<-%")) {
    expect_true(is.function(getFromNamespace(fn, "rave")), info = fn)
  }
})

test_that("start_rave is an alias of start_rave2", {
  expect_identical(rave::start_rave, rave::start_rave2)
})

test_that("rave_version returns a version string", {
  expect_true(nzchar(getFromNamespace("rave_version", "rave")()))
})
