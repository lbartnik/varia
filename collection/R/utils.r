#' Forwarding generic \code{\link[dplyr]{select}}
#' @importFrom dplyr select
#' @name select
#' @export
NULL

#' @importFrom digest digest
md5 <- function (x) digest(x, 'md5')

#' @importFrom digest digest
hash32 <- function (x) digest(x, 'xxhash32')

#' @importFrom assertthat has_attr
path <- function (x) {
  stopifnot(has_attr(x, 'path'))
  attr(x, 'path')
}

is_error <- function (x) inherits(x, 'error')

is_lazy <- function (x) inherits(x, 'lazy')

format_size <- function (x) {
  format(structure(as.numeric(x), class = 'object_size'), 'auto')
}

# split path
split_path <- function (x) {
  stopifnot(is.character(x))
  splt <- function (x) { 
    if (dirname(x) != x)
      c(Recall(dirname(x)), basename(x))
  }
  splt(x)
}

# last n parts of the path as name
path_to_name <- function (x, last_n) {
  path <- split_path(x)
  if (length(path) <= 2) return(x)
  
  path <- c(rev(path)[seq(last_n)], '....')
  do.call(file.path, as.list(rev(path)))
}


