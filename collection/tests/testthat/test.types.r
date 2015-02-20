context('types')

test_that('auto_tags list', {
  tgs <- auto_tags(list(a = 1))
  expect_equal(tgs, list(`class` = 'list', `length` = 1, `names` = 'a'))
})
