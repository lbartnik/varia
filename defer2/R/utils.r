is_error <- function (x) {
  inherits(x, 'try-error')
}

is_package <- function (x) {
  inherits(x, 'package')
}
