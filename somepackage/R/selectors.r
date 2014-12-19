#' @export
#' @importFrom lazyeval all_dots
selector <- function (...) {
  dots <- lazy_dots(..., .follow_symbols = TRUE)
  structure(list(tags = dots), class = 'selector')
}

is_selector <- function (x) inherits(x, 'selector')

get_tag <- function (s, tag) {
  stopifnot(is_selector(s))
  if (tag %nin% names(s$tags))
    stop('no such tag: ', tag, call. = F)
  tag <- s$tags[[tag]]$expr
  if (is.language(tag))
    stop('tag is a language element', call. = F)
  tag
}


