
context("Module-Devs")

skip_on_cran()
skip_if_not(verify_rstudio_version())

library(shiny)

# test_that('define_input', {
#   rave_contexts = rave:::rave_contexts
#   rave_set_context(rave_contexts$default)
#   define_input(textInput('id', 'label', ''), 'value', {value = 'hehehe'})
#   
#   session = shiny:::createMockDomain()
#   
#   shiny::withReactiveDomain(session, {
#     rave_set_context(rave_contexts$shiny_global)
#     print(define_input(textInput('id', 'label', ''), 'value', {value = 'hehehe'}))
#   })
#   
#   
#   
#   rave:::rave_debug(rave_contexts$debug)
#   
#   
#   
# })





test_that('module configuration info', {
  
  rave_context('default')
  
  expect_error(get_root_dir())
  expect_error(get_package_name())
  expect_error(get_path())
  expect_error(load_pkg_description(check_dependencies = NULL))
  expect_error(get_module_label('power_explorer'))
  
  
  
  local({
    .__rave_package__. = 'ravebuiltins'
    ctx = rave_context('rave_module_debug')
    .__rave_package__. = 'ravebuiltins'
    
    
    expect_true(get_root_dir() %in% c(
      system.file('', package = .__rave_package__.)
    ))
    
    expect_equal(get_package_name(), .__rave_package__.)
    expect_equal(get_path('inst/tools'), system.file('tools', package = .__rave_package__.))
    # expect_equal(get_path('inst/'), system.file('', package = .__rave_package__.))
    expect_equal(get_module_label('power_explorer'), 'Power Explorer')
    
    
    
  })
  
  
})

