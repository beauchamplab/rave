

context("Tensors")


projects = get_projects()



test_that('Check the Tensor', {
  x = 1:12
  d = dim = c(3,2,2)
  dn = list(
    1:3,
    1:2,
    2:3
  )
  vn = c('Trial', 'Frequency', 'Time')
  
  ts = Tensor$new(data = x, dim = d, dimnames = dn, varnames = vn)
  
  expect_length(ts$collapse(keep = c(3,2)), prod(d[c(2,3)]))
  
  expect_identical(ts$dim, dim)
  
  names(dn) = vn
  expect_identical(ts$dimnames, dn)
  
  # swap
  capture.output({
    ts$to_swap_now(use_index = TRUE)
  }, type = 'output')
  
  expect_null(ts$.__enclos_env__$private$.data)
  
  expect_is(ts$subset(Trial = Trial == 1), 'Tensor')
  
  expect_equivalent(dim(ts$subset(Frequency = Frequency == 1, data_only = TRUE)),
                    c(3,1,2))
  
  dn = c(dn, list(Electrode = 1))
  names(dn) = c(vn, 'Electrode')
  ts = Tensor$new(data = x, dim = c(d, 1), dimnames = dn, varnames = names(dn))
  
  capture.output({
    expect_is(baseline(ts, 2, 2), 'Tensor')
  }, type = 'output')
})





