test_that("package input", {
  package({ code })
  package(function(x)x)
  package({ code }, fun = function(x)x)
  package({ code }, fun = function(x)x, base::abs, rnorm)
})
