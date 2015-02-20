# --- generic & default API --------------------------------------------

#' @export
auto_tags <- function (x) UseMethod('auto_tags')

#' @export
auto_tags.default <- function (x) {
  list(`class` = class(x), `length` = length(x))
}


print_class <- function (x, tags) UseMethod('print_class')

# 
print_class.default <- function (x, tags) {
  paste0(class(x)[1], ' [', ifelse(is.null(tags$length), '?', tags$length), ']')
}

# --- list -------------------------------------------------------------

auto_tags.list <- function (x) {
  tags <- list(`class` = class(x), `length` = length(x))
  if (length(names(x))) tags$names <- names(x)
  tags
}

print_class.list <- function (x, tags) {
  paste0('list [', tags$len, ']')
}

# --- data.frame -------------------------------------------------------

auto_tags.data.frame <- function (x) {
  list(`class` = class(x), `nrow` = nrow(x), `ncol` = ncol(x))
}

print_class.data.frame <- function (x, tags) {
  paste0("data.frame [", tags$nrow, 'x', tags$ncol, ']')
}

# --- lm ---------------------------------------------------------------

auto_tags.lm <- function (x) {
  list(`class` = class(x), `coef`  = names(x$coefficients))
}

