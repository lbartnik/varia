context("general tests")

test_that("full flow", {
  skip('turned off for now')
  
  if (!require(dplyr, quietly = T)) skip('dplyr not found')
  
  repository('./repo') %>%
    collection("col") %>%
    select(flag == 1) %>%
    run(summary) %>%
    save_to('col_result')
})

test_that('evaluating tags', {
  dots <- lazy_dots(x = a + b)
  expect_true(eval_tags(dots, list(a = 1, b = 2)))
  expect_false(eval_tags(dots, list(a = 0, b = 0)))
})

test_that("selection", {
  col <- structure(list(path = file.path('repo', '8ba6f6f8')), class = 'collection')
  expect_equal(select(col, a == 1, b == 2)$id, '1')
  expect_equal(select(col, a == 1)$id, c('1', '2'))
  expect_equal(select(col, b == 3)$id, c('2', '3'))
})

