#' Forwarding generic \code{\link[dplyr]{select}}
#' @importFrom dplyr select
#' @name select
#' @export
NULL

#' @importFrom digest digest
md5 <- function (x) digest(x, 'md5')

#' @importFrom digest digest
hash32 <- function (x) digest(x, 'xxhash32')

path <- function (x) attr(x, 'path')

is_error <- function (x) inherits(x, 'error')