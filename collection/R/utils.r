#' Forwarding generic \code{\link[dplyr]{select}}
#' @importFrom dplyr select
#' @name select
#' @export
NULL

#' @importFrom digest digest
md5 <- function (x) digest(x, 'md5')

path <- function (x) attr(x, 'path')
