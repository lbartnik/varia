context("general tests")

test_that("full flow", {
  if (!require(dplyr, quietly = T)) skip('dplyr not found')
  
  repository('./repo') %>%
    collection("col") %>%
    select(flag == 1) %>%
    run(summary) %>%
    save_to('col_result')
})
