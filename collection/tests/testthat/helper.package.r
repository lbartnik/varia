check_basic_pkg <- function (pkg, ndeps, nglb) {
  expect_equal(names(pkg), c('deps', 'global'))
  
  if (ndeps) {
    expect_equal(nrow(pkg$deps), ndeps)
    expect_named(pkg$deps, c('lib', 'fun'))
  }
  
  if (nglb) {
    expect_equal(nrow(pkg$global), nglb)
    expect_named(pkg$global, c('name', 'fun', 'env'))
    expect_true(is.character(pkg$global$name))
    expect_true(is.list(pkg$global$fun))
    expect_true(is.list(pkg$global$env))
  }
}