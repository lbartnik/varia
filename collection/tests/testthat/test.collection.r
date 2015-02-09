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
  
  col <- add(col, 1)
  col <- add(col, list(a = 1))
  f <- list.files(path, '*.rds', recursive = T)
  expect_equal(length(f), 4) # (object + tags) x2
  
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
  col  <- add(col, 1, day = as.Date('2011-02-02'))
  
  x <- filter(col, day == as.Date('2011-02-02'))
  expect_true(is.character(x))
  expect_equal(length(x), 1)
  
  y <- objects(x)
  expect_equal(length(y), 1)
  expect_true(1 %in% y)
  
  # clean up
  unlink(path, recursive = T, force = T)
})


test_that('reading tags', {
  col <- collection('sample-collection')
  expect_equal(tags(col, a)[], list(a = c(1, 1, 2))) # [] = drop attributes
  expect_equal(tags(col, b)[], list(b = c(2, 3, 3)))
  expect_error(tags(col, c), 'tags not found: c')
})


test_that('collection print', {
  col <- collection('sample-collection')
  expect_output(print(col), "collection: 3 object\\(s\\), 129 bytes")
})


test_that('retag', {
  
})


test_that('tags print', {
  path <- file.path(tempdir(), 'col')
  col  <- create_collection(path)
  
  col <- add(col, list(a = 999), x = 1)
  expect_equal(length(filter(col)), 1)
  expect_equal(tags(col)[], list(x = 1))
  
  retag(col, x = 1, y = 2)
  expect_equal(tags(col)[], list(x = 1, y = 2))
  
  retag(col, x = 1, y = 2, z = a)
  expect_equal(tags(col)[], list(x = 1, y = 2, z = 999))
  
  retag(col, x = 1, y = 2, z = .$a)
  expect_equal(tags(col)[], list(x = 1, y = 2, z = 999))
  
  # clean up
  unlink(path, recursive = T, force = T)
})

