context("package")


test_that("dependencies", {
  load_or_skip(dplyr)
  
  # standard library
  expect_equal(extract_calls(call('mean')), data_frame(lib = 'base', fun = 'mean'))
  expect_equal(extract_calls(call('summary')), data_frame(lib = 'base', fun = 'summary'))
  
  # some env, not global not namespace
  fun <- function(x)x
  environment(fun) <- globalenv()
  expect_equal(extract_calls(call('fun')), data_frame(lib = 'global', fun = 'fun'))
  
  # function from library
  expect_equal(extract_calls(call('dplyr::filter')),
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


test_that("named pipe", {
  load_or_skip(dplyr)
  
  fun <- . %>% mean(x)
  e <- environment(fun)
  parent.env(e) <- globalenv() # to fool package
  
  pkg <- package(fun)
  check_basic_pkg(pkg, 1, 2)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$global
  expect_equal(glb$name, c('fun', '__entry__'))
  
  expect_equal(class(glb$fun[[1]]), c('fseq', 'function'))
  expect_equal(glb$fun[[1]], fun)
  expect_equal(glb$fun[[2]], function(value)fun(value)) # __entry__
  
  expect_equal(glb$env[[1]], as.list(environment(fun)))
  expect_equal(glb$env[[2]], list())
})


test_that("pipe on the fly", {
  load_or_skip(dplyr)
  
  pkg <- package(. %>% mean(x))
  check_basic_pkg(pkg, 1, 2)
  expect_equal(pkg$deps, data_frame(lib = 'base', fun = 'mean'))
  
  glb <- pkg$global
  expect_equal(glb$name, c('__user__', '__entry__'))
  
  expect_equal(class(glb$fun[[1]]), c('fseq', 'function'))
  expect_equal(glb$fun[[1]], . %>% mean(x))
  expect_equal(glb$fun[[2]], function(value)`__user__`(value)) # __entry__
  
  expect_equal(glb$env[[1]], as.list(environment(. %>% mean(x))))
  expect_equal(glb$env[[2]], list())
})


test_that('complex user-defined', {
  load_or_skip(dplyr)
  pkg <- package(function(x) {
    a <- filter(iris, Species == x)
    b <- summary(iris)
  })
  
  check_basic_pkg(pkg, 2, 2)
  expect_equal(pkg$deps, data_frame(lib = c('dplyr', 'base'),
                                    fun = c('filter', 'summary')))
  
  expect_equal(pkg$global$name, c('__user__', '__entry__'))
})


# also tests for a function name in pipe but not a call
test_that('complex pipe', {
  load_or_skip(dplyr)
  pkg <- package(. %>% filter(a == 'x') %>% arrange(b) %>% summary)
  
  check_basic_pkg(pkg, 3, 2)
  expect_equal(pkg$deps, data_frame(lib = c('dplyr', 'dplyr', 'base'),
                                    fun = c('filter', 'arrange', 'summary')))
  
  expect_equal(pkg$global$name, c('__user__', '__entry__'))
})


test_that("with formals", {
  pkg <- package(summary, alist(x=))
  check_basic_pkg(pkg, 1, 1)
  expect_equal(pkg$global$name, '__entry__')
  expect_equal(pkg$global$fun[[1]], function(x)summary(x))
})


test_that("errors & warnings", {
  err <- 'do not know how to handle lazy expression'
  expect_error(package(summary(iris)), err)
  expect_error(package(summary(x)), err)
  
  expect_error(suppressWarnings(package(x)), 'could not find x') # gives error and a warning
  
  expect_warning(package(function(x)zzz(x)), 'could not find function: zzz')
})

