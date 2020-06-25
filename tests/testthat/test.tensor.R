

context("Tensors")


projects = get_projects()



test_that('Check the Tensor', {
  x = 1:12
  d = dim = c(3,2,2,1)
  dn = list(
    1:3,
    1:2,
    2:3,
    'A'
  )
  vn = c('Trial', 'Frequency', 'Time', Electrode)
  
  ts = Tensor$new(data = x, dim = d, dimnames = dn, varnames = vn)
  ts1 = ECoGTensor$new(data = x, dim = d, dimnames = dn, varnames = vn)
  
  expect_length(ts$collapse(keep = c(3,2)), prod(d[c(2,3)]))
  
  expect_identical(ts$dim, dim)
  
  names(dn) = vn
  expect_identical(ts$dimnames, dn)
  
  # subset
  sub <- ts$subset(Frequency ~ Frequency == 1)
  expect_equal(dim(sub$get_data()), c(3,1,2,1))
  
  
  # swap
  capture.output({
    ts$to_swap_now(use_index = TRUE)
  }, type = 'output')
  
  expect_null(ts$.__enclos_env__$private$.data)
  
  expect_is(ts$subset(Trial = Trial == 1), 'Tensor')
  
  
  expect_is(ts1[,,1], 'Tensor')
  
  
  expect_equivalent(dim(ts$subset(Frequency = Frequency == 1, data_only = TRUE)),
                    c(3,1,2,1))
  
  expect_equivalent(dim((ts1[,1,])$get_data()),
                    c(3,1,2,1))
  
  
  # baseline is soft-depricated, use dipsaus baseline instead
  
  # dn = c(dn, list(Electrode = 1))
  # names(dn) = c(vn, 'Electrode')
  # ts = Tensor$new(data = x, dim = c(d, 1), dimnames = dn, varnames = names(dn))
  # 
  # capture.output({
  #   expect_is(baseline(ts1, 2, 2), 'Tensor')
  # }, type = 'output')
  
  
  
  
})





