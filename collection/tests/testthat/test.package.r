context("package")


test_that("dependencies", {
  # standard library
  expect_equal(get_deps(call('mean')), list(base = 'mean'))
  expect_equal(get_deps(call('summary')), list(base = 'summary'))
  
  # some env, not global not namespace
  fun <- function(x)x
  environment(fun) <- globalenv()
  expect_equal(get_deps(call('fun')), list(global = 'fun'))
  
  # function from library
  expect_equal(get_deps(call('dplyr::filter')), list(dplyr = 'filter'))
})


test_that("base funcs", {
  if (!require(devtools, quietly = T)) skip('could not load devtools')
  
  expect_equal(package(mean), mock_eval_pkg('mean', list(base = 'mean'), 'base'))
  expect_equal(package(summary), mock_eval_pkg('summary', list(base = 'summary'), 'base'))
})


test_that("on the fly", {
  if (!require(devtools, quietly = T)) skip('could not load devtools')
  
  deps <- list(global = list(
    `__anonymous__` = list(fun = function(x)x, env = list())
  ))
  pkg <- mock_eval_pkg('__anonymous__', deps, character())
  
  # a hack is needed to simulate function coming from globalenv
  # TODO should be just package(function(x))
  fun <- lazy_(quote(function(x)x), globalenv())
  expect_equal(package_(fun), pkg)
})


# some env, not global not namespace
test_that("user-defined", {
  fun <- function(x)x
  environment(fun) <- globalenv()
  deps <- list(global = list(fun = list(fun = function(x)x, env = list())))
  
  expect_equal(package(fun), mock_eval_pkg('fun', deps, character()))
})


test_that("from a library", {
  # TODO finish
  package(devtools::session_info)
})


test_that("block of code", {
  # code
  code <- quote({ mean(x) })
  expect_equal(package({ mean(x) }), mock_eval_pkg(code, list(base = 'mean'), 'base'))
})


test_that("pipe", {
  # pipe TODO
  package(. %>% mean(x))
})

