context('repository')

test_that('creating a collection', {
  repo <- create_empty_repo()
  
  collection(repo, 'sample-collection', 'col', .create = T)
  
  path <- file.path(repo, hash32('sample-collection'))
  expect_true(file.exists(path))
  
  name_path <- paste0(path, '.rds')
  expect_true(file.exists(name_path)) # RDS file with collection name
  expect_equal(readRDS(name_path), 'sample-collection')
})

test_that('creating collection - error', {
  repo <- create_empty_repo()
  expect_error(collection(repo, 'sample-collection', 'col'),
               'no such collection in repository and .create is FALSE')
})


