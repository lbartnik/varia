context("magic")

test_that("data can be re-created from calls", {
  filt <- function(x) subset(x, Sepal.Width != 3.5)
  subs <- function(x) x[-c(1,50,100), ]
  
  data <- subset(iris, Sepal.Width != 3.5)[-c(1,50,100), ]
  attr(data, 'inv-reference') <- 'base'
  attr(data, 'inv-call-1') <- filt
  attr(data, 'inv-call-2') <- subs
  
  x <- iris
  attr(x, 'inv-reference') <- 'base'
  y <- magic(filt(x))
  z <- magic(subs(y))
  
  expect_identical(data, z)
})


test_that("lm works with magic", {
  filt <- function(x) subset(x, Sepal.Width != 3.5)
  
  x <- iris
  attr(x, 'inv-reference') <- 'base'
  y <- magic(filt(x))
  
  z <- lm(y)
})

