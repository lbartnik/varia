context('collection')


test_that('filter', {
  col <- collection('sample-collection')
  
  expect_equal(length(filter(col, a == 1)), 2)
  expect_equal(length(filter(col)), 3)
})


test_that('basic operations', {
  path <- file.path(tempdir(), 'col')
  
  expect_false(file.exists(path))
  col <- create_collection(path)
  expect_true(file.exists(path))
  
  col <- add_object(col, 1)
  col <- add_object(col, list(a = 1))
  f <- list.files(path, '*.rds', recursive = T)
  expect_equal(length(f), 4) # (object + tags) x2
  
  col <- refresh(col)
  x <- filter(col) # no criteria, select all
  expect_true(is.character(x))
  expect_equal(length(x), 2)
  
  y <- objects(x) # load objects associated with this selection
  expect_true(1 %in% y)
  expect_true(list(a = 1) %in% y)
  
  # clean up
  unlink(path, recursive = T, force = T)
})


test_that('adding with tag', {
  path <- file.path(tempdir(), 'col')
  col  <- create_collection(path)
  col  <- add_object(col, 1, day = as.Date('2011-02-02'))
  
  x <- filter(col, day == as.Date('2011-02-02'))
  expect_true(is.character(x))
  expect_equal(length(x), 1)
  
  y <- objects(x)
  expect_equal(length(y), 1)
  expect_true(1 %in% y)
  
  # clean up
  unlink(path, recursive = T, force = T)
})


test_that('collection print', {
  col <- collection('sample-collection')
  expect_output(print(col), "3 object\\(s\\), 129 bytes")
})


test_that('symmary', {
  path <- file.path(tempdir(), 'col')
  col  <- create_collection(path)
  
  col <- refresh(add_object(col, list(a = 999), x = 1))
  expect_equal(length(filter(col)), 1)
  
  s <- summary(col)
  expect_equal(s$path, path)
  expect_equal(s$sizes, c(`1` = 72))
  
  expect_equal(s$tags, list(x = 1))
  
  retag(col, x = 1, y = 2)
  expect_equal(summary(col)$tags, list(x = 1, y = 2))
  
  retag(col, x = 1, y = 2, z = a)
  expect_equal(summary(col)$tags, list(x = 1, y = 2, z = 999))
  
  retag(col, x = 1, y = 2, z = .$a)
  expect_equal(summary(col)$tags, list(x = 1, y = 2, z = 999))
  
  # clean up
  unlink(path, recursive = T, force = T)
})


test_that('summary tags', {
  s <- summary(collection('sample-collection'))
  expect_equal(s$tags, list(a = c(1, 1, 2), b = c(2, 3, 3))) # [] = drop attributes
})


test_that('removing objects', {
  path <- copy_sample_col()
  col  <- collection(path)
  
  expect_equal(length(col), 3)
  expect_equal(length(list.files(path, recursive = T)), 6)
  remove_objects(col)
  
  col <- refresh(col)
  expect_equal(length(col), 0)
  expect_equal(length(list.files(path, recursive = T)), 0)
  
  remove_dir(path)
})
