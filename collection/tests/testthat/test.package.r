context("package")


test_that("base funcs", {
  load_or_skip(dplyr, devtools)
  
  pkg <- package(mean, alist(x=))
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$global
  expect_equal(glb$name[1], '__entry__')
  expect_equal(glb$fun[[1]], function(x)mean(x))
  expect_equal(glb$env[[1]], list())
})


test_that("from a library", {
  load_or_skip(dplyr, devtools)
  
  pkg <- package(devtools::session_info, alist(ib = FALSE))
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$deps, data_frame(lib = 'devtools', fun = 'session_info'))
  
  glb <- pkg$global
  expect_equal(glb$name[1], '__entry__')
  expect_equal(glb$fun[[1]], function(ib = FALSE)devtools::session_info(ib))
  expect_equal(glb$env[[1]], list())
})


test_that("on the fly", {
  load_or_skip(dplyr, devtools)
  
  # a hack is needed to simulate function coming from globalenv
  # TODO it would be best to test the form "package(function(x)x)"
  fun <- lazy_(quote(function(x)x), globalenv())
  pkg <- package_(fun, alist(x=))
  check_basic_pkg(pkg, 0, 1)
    
  glb <- pkg$global
  expect_equal(glb$name, c('__entry__'))
  expect_equal(glb$fun[[1]], function(x)x)
  expect_equal(glb$env[[1]], list())
})


# some env, not global not namespace
test_that("user-defined", {
  fun <- function(x)x
  environment(fun) <- new.env(parent = globalenv())
  
  pkg <- package(fun, alist(x=))
  check_basic_pkg(pkg, 0, 2)
  
  glb <- pkg$globals
  expect_equal(glb$name, c('__entry__', 'fun'))
  expect_equal(glb$fun[[1]], function(x)fun(x))
  expect_equal(glb$fun[[2]], fun)
  expect_equal(glb$env[[1]], list())
  expect_equal(glb$env[[2]], list())
})


# similar but with an object in the closure's env
test_that("user-defined non-empty env", {
  fun <- function(x)x*y
  e <- new.env(parent = globalenv())
  assign('y', 10, envir = e)
  environment(fun) <- e
  
  pkg <- package(fun, alist(x=))
  check_basic_pkg(pkg, 0, 2)
  expect_equal(pkg$globals$name, c('__entry__', 'fun'))
  expect_equal(pkg$globals$env[[1]], list())
  expect_equal(pkg$globals$env[[2]], list(y = 10))
})


test_that("block of code", {
  pkg <- package({ mean(.) }, alist(.=))
  check_basic_pkg(pkg, 0, 1)
  
  glb <- pkg$globals
  expect_equal(glb$name, c('__entry__'))
  expect_equal(glb$fun[[1]], function(.){mean(.)})
  expect_equal(glb$env[[1]], list())
})


test_that("named pipe", {
  load_or_skip(dplyr)
  
  fun <- . %>% mean(x)
  e <- environment(fun)
  parent.env(e) <- globalenv() # to fool package
  
  pkg <- package(fun, alist(.=))
  check_basic_pkg(pkg, 1, 2)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$globals
  expect_equal(glb$name, c('__entry__', 'fun'))
  
  expect_equal(glb$fun[[1]], function(.)fun(.)) # __entry__
  expect_equal(glb$fun[[2]], fun)
  expect_equal(class(glb$fun[[2]]), c('fseq', 'function'))
  
  expect_equal(glb$env[[1]], list())
  expect_equal(glb$env[[2]], as.list(environment(fun)))
})


test_that("pipe on the fly", {
  load_or_skip(dplyr)
  
  pkg <- package(. %>% mean(x))
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$global
  expect_equal(glb$name, c('__entry__'))
  
  expect_equal(class(glb$fun[[1]]), c('fseq', 'function'))
  expect_equal(glb$fun[[1]], . %>% mean(x))
  
  expect_equal(glb$env[[1]], as.list(environment(. %>% mean(x))))
})


test_that('complex user-defined', {
  load_or_skip(dplyr)
  pkg <- package(function(x) {
    a <- filter(iris, Species == x)
    b <- summary(iris)
  })
  
  check_basic_pkg(pkg, 2, 1)
  expect_equal(pkg$deps, data_frame(lib = c('dplyr', 'base'),
                                    fun = c('filter', 'summary')))
  
  expect_equal(pkg$global$name, c('__entry__'))
})


# also tests for a function name in pipe but not a call
test_that('complex pipe', {
  load_or_skip(dplyr)
  pkg <- package(. %>% filter(a == 'x') %>% arrange(b) %>% summary)
  
  check_basic_pkg(pkg, 3, 1)
  expect_equal(pkg$deps, data_frame(lib = c('dplyr', 'dplyr', 'base'),
                                    fun = c('filter', 'arrange', 'summary')))
  
  expect_equal(pkg$global$name, '__entry__')
})


test_that("with formals", {
  pkg <- package(summary, alist(x=))
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$global$name, '__entry__')
  expect_equal(pkg$global$fun[[1]], function(x)summary(x))
})


test_that("errors & warnings", {
  err <- 'do not know how to handle the lazy expression'
  expect_error(package(summary(iris)), err)
  expect_error(package(summary(x)), err)
  
  expect_warning(package(x, alist()), 'could not find function: x')
  expect_warning(package(function(x)zzz(x)), 'could not find function: zzz')
})

