context("package")

test_that("packaging", {
  # standard library
  package(mean)
  package(summary)
  
  # build on-the-fly
  package(function(x)x)
  
  # some env, not global not namespace
  fun <- function(x)x
  package(fun)
  
  # global env
  environment(fun) <- globalenv()
  package(fun)
  
  # function from library
  package(dplyr::filter)
  
  # code
  package({
    mean(x)
  })
})
