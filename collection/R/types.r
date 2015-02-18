# --- generic & default API --------------------------------------------

#' @export
auto_tags <- function (x) UseMethod('auto_tags')

#' @export
auto_tags.default <- function (x) {
  list(`class` = class(x))
}


print_line <- function (x, tags) UseMethod('print_line')

print_line.default <- function (x, tags) {
  paste0("'", class(x)[1], "' object")
}

# --- data.frame -------------------------------------------------------

auto_tags.data.frame <- function (x) {
  list(`class` = class(x), `nrow` = nrow(x), `ncol` = ncol(x))
}

print_line.data.frame <- function (x, tags) {
  paste0("'data.frame' ", tags$nrow, 'x', tags$ncol)
}



