context("tags")

test_that("retag, add=F", {
  path <- copy_sample_col()
  col  <- collection(path)
  retag(col, x = 1, .add = F)
  
  lapply(list.files(path, '_tags.rds$', full.names = T, recursive = T),
          function(path) expect_equal(readRDS(path), list(x = 1)))
  
  remove_dir(path)
})

test_that("retag, add=T", {
  path <- copy_sample_col()
  col  <- collection(path)
  retag(col, x = 1)
  
  tg <- lapply(list.files(path, '_tags.rds$', full.names = T, recursive = T), readRDS)
  expect_equal(tg[[1]], list(x = 1, a = 1, b = 2))
  expect_equal(tg[[2]], list(x = 1, a = 1, b = 3))
  expect_equal(tg[[3]], list(x = 1, a = 2, b = 3))
  
  remove_dir(path)
})

