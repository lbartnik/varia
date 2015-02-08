context("package evaluation")


test_that("mocked simple evalutation", {
  load_or_skip(dplyr)
  
  pkg <- structure(list(
    deps   = data_frame(lib = 'base', fun = 'summary'),
    global = data_frame(name = '__entry__', fun = list(function(x)summary(x)), env = list(list()))
  ), class = 'eval_pkg')
  
  expect_equal(pkg_eval(pkg, list(x = iris)), summary(iris))
})


test_that("mocked user-defined evalutation", {
  load_or_skip(dplyr)
  
  pkg <- structure(list(
    deps   = data_frame(),
    global = data_frame(name = c('fun', '__entry__'),
                        fun  = list(function(x)x*x, function(x)fun(x)),
                        env  = list(list(), list()))
  ), class = 'eval_pkg')
  
  expect_equal(pkg_eval(pkg, list(x = 10)), 100)
})


test_that('base function', {
  pkg <- package(summary)
  expect_equal(pkg_eval(pkg, list(iris)), summary(iris))
})

test_that('library funtion', {
  load_or_skip(dplyr)
  pkg <- package(dplyr::filter_)
  res <- pkg_eval(pkg, list(iris, .dots = quote(Species == 'virginica')))
  expect_equivalent(res, filter(iris, Species == 'virginica'))
})

test_that('user-defined function', {
  fun <- function(x)x*x
  environment(fun) <- globalenv() # when testing function is in a package
  pkg <- package(fun)
  expect_equal(pkg_eval(pkg, list(10)), 100)
})

test_that('complex user-defined', {
  load_or_skip(dplyr)
  pkg <- package(function(x) {
    filter(iris, Species == x)
  })
  expect_equal(pkg_eval(pkg, list('virginica')), filter(iris, Species == 'virginica'))
})

test_that('pipe', {
  load_or_skip(dplyr)
  pkg <- package(function(x) {
    filter(iris, Species == x)
  })
  expect_equal(pkg_eval(pkg, list('virginica')), filter(iris, Species == 'virginica'))
})



