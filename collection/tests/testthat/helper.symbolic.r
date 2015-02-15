prep_lazy <- function (expr) {
  if (!require(lazyeval, quietly = TRUE)) skip('could not load lazyeval')
  expr <- substitute(expr)
  env  <- parent.frame()
  lazy_(expr, env)
}

check_fun <- function (fun, frmls, bdy) {
  expect_true(is.function(fun))
  expect_equal(formals(fun), as.pairlist(frmls)) # strange but alist is NOT a pairlist like formals()
  expect_equal(body(fun), substitute(bdy))
}