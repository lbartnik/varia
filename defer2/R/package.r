#' Determine classes of expressions.
#'
#' The rules are:
#' \itemize{
#'   \item Function definitions and blocks of code will be serialized;
#'   \item Colon expressions are library dependencies;
#'   \item Single-name symbols need to be examined: those who are
#'         accessible only from package namespaces are library dependencies,
#'         the rest are user-defined functions that need to be serialized
#' }
#'
#' @importFrom lazyeval lazy_dots
#' @export
package <- function (..., .inherit = TRUE) {
  dots <- lazy_dots(...)
  calling_env <- parent.frame()

  # determine their class of every expression
  class <- vapply(dots, classify_lazy_dot, character(1))

  if (any(i <- (class == 'unsupported'))) {
    stop('expressions in dots are not supported: ', toString(dots[i]))
  }

  # determine which symbols refer library functions
  i_lf  <- vapply(dots[class == 'symbol'], is_library_function, logical(1))
  dots[class == 'symbol'][i_lf] <- 'library_symbol'

  # functions to be serialized are defined by this condition
  i_fun <- class %in% c('fun_definition', 'code_block', 'symbol')
  user_objs <- process_user_objects(dots[i_fun], class[i_fun])

  # global dependencies are defined by this conditions
  i_dep <- class %in% c('col_expr', 'library_symbol')
  dependencies <- process_global_dependencies(dots[i_dep], class[i_dep])

  # TODO detect other library and user-defined dependencies by examining
  #      the body of each function which is to be serialized

  #package_(dots, calling_env, .inherit)
}




# check where do the functions belong - if they belong in a package, then they
# are just library dependencies and all we need to do is write down their name
# and their package name;
# if they belong in the global environment or and environment that is a
# descendant of the global environment - then they are function dependencies
# that need to be serialized
classify_lazy_dot <- function (lazy_obj)
{
  stopifnot(is_lazy(lazy_obj))
  expr <- lazy_obj$expr

  if (is_colexpr(expr))   return('col_expr')
  if (is_codeblock(expr)) return('code_block')
  if (is_fundef(expr))    return('fun_definition')
  if (is.symbol(expr))    return('symbol')

  'unsupported'
}

is_library_function <- function(lazy_obj)
{
  stopifnot(is_lazy(lazy_obj))
  e <- get_containing_env(as.character(lazy_obj$expr), lazy_obj$env)
  environmentName(e) %in% search()[-1]
}

get_containing_env <- function (name, env) {
  if (identical(env, emptyenv())) return(env)
  if (exists(name, envir = env, mode = 'function', inherits = FALSE))
    return(env)
  Recall(name, parent.env(env))
}

turn_block_into_fdef <- function (lazy_dot) {
  lazy_dot$expr <- substitute(function(.)x, list(x = lazy_dot$expr))
  lazy_dot
}

#' All objects must be named
#'
#' By default the only one allowed unnamed object must be the
#' entry object. There must be exactly one object named "entry".
assert_named <- function (dots)
{
  empty_names <- !nchar(names(dots))
  if (sum(empty_names) > 1)
    stop('there can be only one unnamed object')

  if (any(empty_names) && any(names(dots) == 'entry'))
    stop('unnamed function and "entry" function cannot be present at the same time')

  if (any(empty_names)) {
    warning('marking the unnamed function the "entry" function')
    names(dots)[empty_names] <- 'entry'
  }

  if (sum(names(dots) == 'entry') != 1)
    stop('exactly one user function must be named as "entry"')

  dots
}




#' Extracts: function definitions, blocks of code, functions referred
#' by name and present in GlobalEnvironment or one of its descendants
#' (that is, execution environments).
#'
#' @importFrom lazyeval lazy_eval
process_user_objects <- function (dots, class)
{
  # replace blocks of code with function definitions
  i_code <- (class == 'code_block')
  dots[i_code] <- lapply(dots[i_code], turn_block_into_fdef)

  # make sure code objects are all named
  i_obj <- class %in% c('fun_definition', 'code_block')
  dots[i_obj] <- assert_named(dots[i_obj])

  # name global symbols
  i_sym <- (classes == 'symbol')
  exprs <- lapply(dots[i_sym], `[[`, i = 'expr')
  names(dots[i_sym]) <- as.character(exprs)

  lazy_eval(dots)
}


process_global_dependencies <- function (dots, class)
{
  if (any(names(dots) != "")) {
    stop('library functions referred by name cannot be renamed')
  }

  exprs <- lapply(dots, function(lazy) as.character(lazy$expr))

  # colon expressions go first
  d1 <- data.frame(
    package = vapply(exprs[class == 'col_expr'], `[[`, i = 2, character(1)),
    fnction = vapply(exprs[class == 'col_expr'], `[[`, i = 3, character(1))
  )

  # function names
  d2 <- data.frame(
    package = vapply(lazy_eval(dots[j]), function(f) environmentName(environment(f)),
                     character(1)),
    fnction = unlist(exprs[j])
  )

  rbind(d1, d2)
}














# will detect only the library dependencies not the globally accessible
# user-defined functions
package_ <- function (dots, libraries, .detect_libs) {
  types <- get_type(dots)
  names(dots) <- get_name(dots, types)

  # only find library dependencies, do not add function objects to serialize
}










#' Does an expression refer to a function.
#'
#'  Returns \code{TRUE} for expression which is a \code{symbol}
#'  or one of the colon operators, \code{::} or \code{:::}.
#'
#' @param expr Expression to check.
is_funref <- function (expr)
{
  is.symbol(expr) || is_colexpr(expr)
}


#' Check whether expression has double or triple colons
#'
#' @param  expr An expression to be tested.
#' @return logical - TRUE if expr contains `::` or `:::`, FALSE otherwise.
is_colexpr <- function(expr) {
  is.call(expr) &&
    (identical(expr[[1L]], quote(`::`)) || identical(expr[[1L]], quote(`:::`)))
}


#' Check whether expression is enclosed in curly braces.
#'
#' @param  expr An expression to be tested.
#' @return logical - TRUE if expr is enclosed in `{`, FALSE otherwise.
is_codeblock <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}

is_fundef <- function(x) {
  is.call(x) && identical(x[[1]], quote(`function`))
}
