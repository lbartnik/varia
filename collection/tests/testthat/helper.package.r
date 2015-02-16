check_basic_pkg <- function (pkg, ndeps, nglb) {
  expect_equal(names(pkg), c('deps', 'globals'))
  
  if (ndeps) {
    expect_equal(nrow(pkg$deps), ndeps)
    expect_named(pkg$deps, c('lib', 'fun'))
  }
  
  if (nglb) {
    expect_equal(nrow(pkg$globals), nglb)
    expect_named(pkg$globals, c('name', 'fun', 'env'))
    expect_true(is.character(pkg$globals$name))
    expect_true(is.list(pkg$globals$fun))
    expect_true(is.list(pkg$globals$env))
  }
}