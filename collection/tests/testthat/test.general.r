context("general tests")

test_that("full flow", {
  skip('turned off for now')
  
  if (!require(dplyr, quietly = T)) skip('dplyr not found')
  
  collection("col") %>%
    select(flag == 1) %>%
    ccply(summary) %>%
    save_to('col_result')
})
