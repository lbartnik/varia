context("general tests")

test_that("single call works", {
  s <- sourcerer('../../tmp-work/iris.rds')
  y <- 1
  r <- do(s, data.frame(x = mean(.[, 1] + y)))
  
  expect_true(is.data.frame(r))
  expect_equal(dim(r), c(1,1))
  expect_equal(names(r), 'x')
  expect_that(r$x, equals(6.84333, tolerance = .001))
})

test_that("complex call works", {
  s <- sourcerer('../../tmp-work/iris.rds')
  r <-
    s %>%
      select(-Sepal.Width) %>%
      filter(Sepal.Height > 1) %>%
      mutate(x = Sepal.Height + Petal.Width) %>%
      do({
        data.frame(x = mean(x))
      })
  
  # TODO finish the test
})
