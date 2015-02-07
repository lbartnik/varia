context("package")


test_that("dependencies", {
  load_or_skip(dplyr)
  
  # standard library
  expect_equal(get_deps(call('mean')), data_frame(lib = 'base', fun = 'mean'))
  expect_equal(get_deps(call('summary')), data_frame(lib = 'base', fun = 'summary'))
  
  # some env, not global not namespace
  fun <- function(x)x
  environment(fun) <- globalenv()
  expect_equal(get_deps(call('fun')), data_frame(lib = 'global', fun = 'fun'))
  
  # function from library
  expect_equal(get_deps(call('dplyr::filter')),
               data_frame(lib = 'dplyr', fun = 'filter'))
})


test_that("base funcs", {
  load_or_skip(dplyr, devtools)
  
  pkg <- package(mean)
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$global
  expect_equal(glb$name[1], '__entry__')
  expect_equal(glb$fun[[1]], function(x, ...)mean(x, ...))
  expect_equal(glb$env[[1]], list())
})


test_that("from a library", {
  load_or_skip(dplyr, devtools)
  
  pkg <- package(devtools::session_info)
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$deps, data_frame(lib = 'devtools', fun = 'session_info'))
  
  glb <- pkg$global
  expect_equal(glb$name[1], '__entry__')
  expect_equal(glb$fun[[1]], function(include_base = FALSE)devtools::session_info(include_base))
  expect_equal(glb$env[[1]], list())
})


test_that("on the fly", {
  load_or_skip(dplyr, devtools)
  
  # a hack is needed to simulate function coming from globalenv
  # TODO it would be best to test the form "package(function(x))"
  fun <- lazy_(quote(function(x)x), globalenv())
  pkg <- package_(fun)
  check_basic_pkg(pkg, 0, 2)
    
  glb <- pkg$global
  expect_equal(glb$name, c('__user__', '__entry__'))
  expect_equal(glb$fun[[1]], function(x)x) # __user__
  expect_equal(glb$fun[[2]], function(x)`__user__`(x)) # __entry__
  expect_equal(glb$env[[1]], list())
  expect_equal(glb$env[[2]], list())
})


# some env, not global not namespace
test_that("user-defined", {
  fun <- function(x)x
  environment(fun) <- new.env(parent = globalenv())
  
  pkg <- package(fun)
  check_basic_pkg(pkg, 0, 2)
  
  glb <- pkg$global
  expect_equal(glb$name, c('fun', '__entry__'))
  expect_equal(glb$fun[[1]], fun)
  expect_equal(glb$fun[[2]], function(x)fun(x)) # __entry__
  expect_equal(glb$env[[1]], list())
  expect_equal(glb$env[[2]], list())
})


# similar but with an object in the closure's env
test_that("user-defined non-empty env", {
  fun <- function(x)x*y
  e <- new.env(parent = globalenv())
  assign('y', 10, envir = e)
  environment(fun) <- e
  
  pkg <- package(fun)
  check_basic_pkg(pkg, 0, 2)
  
  expect_equal(pkg$global$env[[1]], list(y = 10))
})


test_that("block of code", {
  pkg <- package({ mean(.) })
  check_basic_pkg(pkg, 0, 2)
  
  glb <- pkg$global
  expect_equal(glb$name, c('__user__', '__entry__'))
  expect_equal(glb$fun[[1]], function(.){mean(.)})
  expect_equal(glb$fun[[2]], function(.)`__user__`(.)) # __entry__
  expect_equal(glb$env[[1]], list())
  expect_equal(glb$env[[2]], list())
})


test_that("pipe", {
  load_or_skip(dplyr)
  
  pkg <- package(. %>% mean(x))
  check_basic_pkg(pkg, 0, 2)
  
  glb <- pkg$global
  expect_equal(glb$name, c('__user__', '__entry__'))
  
  expect_equal(class(glb$fun[[1]]), c('fseq', 'function'))
  expect_equal(glb$fun[[1]], . %>% mean(x))
  expect_equal(glb$fun[[2]], function(value)`__user__`(value)) # __entry__
  
  expect_equal(glb$env[[1]], as.list(environment(. %>% mean(x))))
  expect_equal(glb$env[[2]], list())
})

