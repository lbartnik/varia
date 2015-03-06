run_via_psock <- function (task) {
  if (!require(parallel, warn.conflicts = F)) skip('could not load parallel')
  
  cl  <- makePSOCKcluster(1)
  res <- parLapply(cl, list(task), function(t) {
    library(collection)
    execute_deferred(t)
  })
  stopCluster(cl)
  res[[1]]
}
