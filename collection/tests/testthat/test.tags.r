context("tags")

test_that("retag, add=F", {
  path <- copy_sample_col()
  col  <- collection(path)
  retag(col, x = 1, .add = F)
  
  lapply(list.files(path, '_tags.rds$', full.names = T, recursive = T),
          function(path) expect_equal(readRDS(path), list(x = 1, class = 'list')))
  
  remove_dir(path)
})

test_that("retag, add=T", {
  path <- copy_sample_col()
  col  <- collection(path)
  retag(col, x = 1)
  
  tg <- read_tag_files(path)
  expect_equal(tg[[1]], list(x = 1, class = 'list', a = 1, b = 2))
  expect_equal(tg[[2]], list(x = 1, class = 'list', a = 1, b = 3))
  expect_equal(tg[[3]], list(x = 1, class = 'list', a = 2, b = 3))
  
  remove_dir(path)
})

test_that('multi-valued tags', {
  path <- create_empty_col()
  col  <- collection(path)
  add_object(col, 1, tag = 1:10)

  tg <- read_tag_files(path)[[1]]
  expect_named(tg, c('tag', 'class', '.date'))
  expect_equal(tg$tag, 1:10)
  
  col <- refresh(col)
  res <- filter(col, 1 %in% tag)
  expect_equal(length(res), 1)
  
  expect_warning(res <- filter(col, 1 == tag))
  expect_equal(length(res), 1)
})
