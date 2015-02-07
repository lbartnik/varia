context("package evaluation")


test_that("simple evalutation", {
  if (!load_dplyr()) skip('error loading dplyr')
  
  pkg <- structure(list(
    deps   = data_frame(lib = 'base', fun = 'summary'),
    global = data_frame(name = '__entry__', fun = list(function(x)summary(x)), env = list(list()))
  ), class = 'evaluation_package')
  
  expect_equal(pkg_eval(pkg, list(x = iris)), summary(iris))
})


test_that("user-defined evalutation", {
  if (!load_dplyr()) skip('error loading dplyr')
  
  pkg <- structure(list(
    deps   = data_frame(),
    global = data_frame(name = c('fun', '__entry__'),
                        fun  = list(function(x)x*x, function(x)fun(x)),
                        env  = list(list(), list()))
  ), class = 'evaluation_package')
  
  expect_equal(pkg_eval(pkg, list(x = 10)), 100)
})


