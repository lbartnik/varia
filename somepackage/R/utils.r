#' @importFrom assertthat see_if
see <- function (..., quiet = F) {
  r <- see_if(..., env = parent.frame())
  if (!r && !quiet) warning(attr(r, 'msg'), call. = F)
  r
}

`%nin%` <- function (x, y) (!(x %in% y))

