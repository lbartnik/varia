is_lazy <- function(x) inherits(x, "lazy")

is_fundef <- function(x) {
  is.call(x) && identical(x[[1]], quote(`function`))
}


# --- begin: magrittr --------------------------------------------------

# Check whether expression is enclosed in curly braces.
#
# @param  expr An expression to be tested.
# @return logical - TRUE if expr is enclosed in `{`, FALSE otherwise.
is_funexpr <- function(expr)
{
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}


# Check whether expression has double or triple colons
#
# @param  expr An expression to be tested.
# @return logical - TRUE if expr contains `::` or `:::`, FALSE otherwise.
is_colexpr <- function(expr)
{
  is.call(expr) &&
    (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
}


# Check whether a symbol is a simple magrittr pipe
#
# The original implementation is TRUE also for %<>%, %T>% and %$%
#
# @param pipe A quoted symbol
# @return logical - TRUE if a valid magrittr pipe, FALSE otherwise.
is_pipe <- function(pipe)
{
  identical(pipe, quote(`%>%`))
}

# --- end: magrittr ----------------------------------------------------
