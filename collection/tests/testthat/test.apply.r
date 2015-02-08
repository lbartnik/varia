context('apply')

test_that('simple case', {
  res <-
    collection('./sample-collection') %>%
    oply(summary) %>%
    to_ram

  files <- list.files('./sample-collection', '^[^_]+.rds$',
                      recursive = T, full.names = T)
  cmp <- lapply(files, function(p) summary(readRDS(p)) )
  
  expect_equal(res, cmp)
})
