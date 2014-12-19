#' @export
#' @importFrom digest digest
id <- function (x) structure(digest(x, 'md5'), class = 'id')

#' @export
is_id <- function (x) inherits(x, 'id')

#' @export
#' @importFrom assertthat has_attr
get_id <- function (x) {
  stopifnot(has_attr(x, 'id'))
  attr(x, 'id')
}

#' @export
id_to_file <- function (x) {
  if (!is_id(x)) x <- get_id(x)
  paste0(x, '.rds')
}

#' @export
id_to_tags <- function (x) {
  if (!is_id(x)) x <- get_id(x)
  paste0(x, '_tags.rds')
}


#' @importFrom assertthat has_attr
with_id <- function (object) {
  if (has_attr(object, 'id')) return(object)
  attr(object, 'id') <- id(object)
  object
}


