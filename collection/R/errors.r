as_errors <- function (x) {
  stopifnot(is.list(x))
  class(x) <- 'errors'
  x
}

#' @importFrom assertthat has_attr
has_errors <- function (x) {
  has_attr(x, 'errors')
}

#' @export
errors <- function (x) {
  stopifnot(has_errors(x))
  attr(x, 'errors')
}

# removes errors attribute
without_errors <- function (x) {
  attr(x, 'errors') <- NULL
  x
}

#' @export
print.errors <- function (x, n = 1) {
  if (!length(x)) {
    cat('*ply result: no errors\n')
    return(invisible(x))
  }
  
  cat('*ply result - with', length(x), 'error(s):\n')
  cat('1: ', x[[1]]$message, 'in', deparse(x[[1]]$call), '\n') # TODO call can be huge...
  
  if (length(x) > 1) cat('...\n')
}
