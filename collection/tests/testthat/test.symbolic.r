context("symbolic")

test_that("valid inputs: name", {
  lzy <- prep_lazy(summary)
  usr <- prepare_user_object(lzy, alist(obj=, tags=))
  check_fun(usr, alist(obj=, tags=), summary(obj, tags))
})

test_that("valid inputs: lib::name", {
  lzy <- prep_lazy(base::unique)
  usr <- prepare_user_object(lzy, alist(obj=, tags=))
  check_fun(usr, alist(obj=, tags=), base::unique(obj, tags))
})

test_that("valid inputs: function definition", {
  lzy <- prep_lazy(function(x, y) { summary(x) })
  usr <- prepare_user_object(lzy)
  check_fun(usr, alist(x=, y=), { summary(x) })
})

test_that("valid inputs: pipe definition", {
  load_or_skip(dplyr)
  lzy <- prep_lazy(. %>% summary)
  usr <- prepare_user_object(lzy)
  expect_equal(usr, . %>% summary) # it is just the object
})

test_that("valid inputs: code block", {
  lzy <- prep_lazy({ summary(obj) })
  usr <- prepare_user_object(lzy, alist(obj=, tags=))
  check_fun(usr, alist(obj=, tags=), { summary(obj) })
})

# TODO add the message
test_that('invalid inputs: missing formals', {
  lzy <- prep_lazy({ summary(obj) })
  expect_error(prepare_user_object(lzy))
  
  lzy <- prep_lazy(summary)
  expect_error(prepare_user_object(lzy))
  
  lzy <- prep_lazy(base::unique)
  expect_error(prepare_user_object(lzy))
})

test_that('invalid inputs: formals differ in length', {
  lzy <- prep_lazy(function(x, y)x)
  expect_error(prepare_user_object(lzy, alist(x=)))
})


test_that('immediate_dependencies: function', {
  deps <- immediate_dependencies(function(x)summary(x), environment())
  expect_true(is.data.frame(deps))
  expect_named(deps, c('lib', 'fun'))
  expect_equal(nrow(deps), 1)
  expect_equal(deps$lib, 'base')
  expect_equal(deps$fun, 'summary')
})

test_that('immediate_dependencies: pipe', {
  load_or_skip(dplyr)
  deps <- immediate_dependencies(. %>% unique %>% summary, environment())
  expect_true(is.data.frame(deps))
  expect_named(deps, c('lib', 'fun'))
  expect_equal(nrow(deps), 2)
  expect_equal(deps$lib, c('base', 'base'))
  expect_equal(deps$fun, c('unique', 'summary'))
})



test_that("dependencies", {
  load_or_skip(dplyr)
  
  # standard library
  deps <- extract_dependencies(function(x)mean(x), globalenv())
  expect_equal(deps, data_frame(lib = 'base', fun = 'mean'))
  
  # some env, not global not namespace
  fun <- function(x)x
  environment(fun) <- globalenv()
  deps <- extract_dependencies(function(x)fun(x), environment())
  expect_equal(deps, data_frame(lib = 'global', fun = 'fun'))
  
  # function from library
  deps <- extract_dependencies(function(x)dplyr::filter(x), globalenv())
  expect_equal(deps, data_frame(lib = 'dplyr', fun = 'filter'))
})


