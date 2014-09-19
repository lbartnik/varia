context("with_mock test")

test_that("with_mock works", {
  with_mock(`plyr::adply` = function()2, { f() })
})
