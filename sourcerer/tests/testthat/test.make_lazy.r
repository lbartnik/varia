context("make_lazy")

test_that("simple call works", {
  x <- make_lazy_fun(lm)
  m <- x(Petal.Width ~ Species, data = iris)
  expect_true(inherits(m, 'lm'))
})
