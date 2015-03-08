context('apply')

test_that('simple case', {
  res <-
    collection('./sample-collection') %>%
    cply(summary) %>%
    locally

  files <- list.files('./sample-collection', '^[^_]+.rds$',
                      recursive = T, full.names = T)
  cmp <- lapply(files, function(p) summary(readRDS(p)) )
  
  expect_equivalent(res, cmp)
})


test_that("tply", {
  col <- collection('sample-collection')
  tsk <- tply(col, function (tags) {
    as.data.frame(tags)
  })
  tgs <- do.call(rbind, locally(tsk))

  exp <- data.frame(a = c(1,1,2), b = c(2,3,3),
                    class = 'numeric',
                    .id = c('1234ab', '1234cd', '1234ef'))
  expect_equivalent(tgs, exp)
})


test_that('to_collection', {
  repo <- copy_sample_repo()
  src  <- collection(repo, 'sample collection')
  dest <- collection(repo, 'destination', .create = T)
  
  task <- cply(src, summary)
  suppressMessages(res <- to_collection(task, dest))
  
  # basic checks
  expect_true(inherits(res, 'collection'))
  expect_true(inherits(res, 'ply_result'))
  expect_true('path' %in% names(attributes(res)))
  expect_equal(attr(res, 'path'), attr(dest, 'path'))
  
  # refresh and check contents
  dest <- refresh(dest)

  # compare serialized forms of the objects
  expt <- vapply(obj_files(src), function(x)toString(summary(readRDS(x))), character(1))
  actl <- vapply(obj_files(dest), function(x)toString(readRDS(x)), character(1))
  
  # ordering is unknown (based on hashed ids)
  expect_true(setequal(expt, actl))
  
  remove_dir(repo)
})


test_that('to_collection, fun returns NULL', {
  repo <- copy_sample_repo()
  src  <- collection(repo, 'sample collection')
  dest <- collection(repo, 'destination', .create = T)
  
  task <- cply(src, function(o,t)NULL)
  suppressMessages(res <- to_collection(task, dest))
  
  expect_equivalent(attr(res, 'errors'), list())  
  expect_equal(length(refresh(dest)), 0)

  remove_dir(repo)
})


test_that('simple deferred execution', {
  # simulated via makePSOCKcluster
  load_or_skip(parallel)
  
  col <- collection('sample-collection')
  tsk <- cply(col, function(o,t)summary(o)) %>% deferred
  
  res <- run_via_psock(tsk)
  expect_equal(res, lapply(read_obj_files('sample-collection'), summary))
})


test_that('complex deferred execution', {
  skip('defer has been changed - update needed')
  
  # simulated via makePSOCKcluster
  load_or_skip(parallel)
  
  col <- collection('sample-collection')
  fun <- function(x)summary(x)
  foo <- function(y)fun(y)
  bar <- function(z)foo(z)
  
  # tricking pack(), otherwise fun foo & bar are in the 'collection'
  # namespace; eval(quote(), new.env()) is required to keep top-level
  # function's environment empty
  environment(fun) <- environment(foo) <- environment(bar) <- globalenv()
  tsk <- eval(quote(cply(col, function(o,t)bar(o)) %>% deferred), new.env())
  
  res <- run_via_psock(tsk)
  expect_equal(res, lapply(read_obj_files('sample-collection'), summary))
})

