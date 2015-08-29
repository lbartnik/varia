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

  # --- 1. process the user input -------------------------------------
  #
  # the user input are the objects to be serialized + any explicit
  # dependencies that the user decided to provide

  # determine their class of every object passed from the user and
  # check for unsupported types of objects
  classes <- vapply(dots, classify_lazy_dot, character(1))

  if (any(i <- (classes == 'unsupported'))) {
    stop('expressions in dots are not supported: ', toString(dots[i]))
  }

  # (1) extract functions that need to be serialized and make
  #     sure they are all named
  # (2) extract explicit dependencies from the list provided by the
  #     user
  to_serialize <- extract_function_objects(dots, classes)
  dependencies <- extract_explicit_dependencies(dots, classes)

  # TODO detect other library and user-defined dependencies by examining
  #      the body of each function which is to be serialized

  #package_(dots, calling_env, .inherit)
}


#' Extracts: function definitions, blocks of code, functions referred
#' by name and present in GlobalEnvironment or one of its descendants
#' (that is, execution environments).
extract_function_objects <- function (dots, classes)
{
  # put names on user symbols
  i_usym <- (classes == 'user_symbol')
  exprs <- lapply(dots, `[[`, i = 'expr')
  names(dots[i_usym]) <- as.character(exprs[i_usym])

  # turn blocks of code into function definitions
  i_code <- (classes == 'code_block')
  dots[i_code] <- turn_blocks_into_fdefs(dots[i_code])

  # pick objects to be serialized
  i_fdef <- (classes == 'fun_definition')

  # make sure names are consistent
  to_serialize <- dots[i_fdef | i_code | i_usym]
  to_serialize <- assert_names_for_serialized(to_serialize)

  # evaluate all (lazy) objects to process them for dependencies
  # and then serialize
  lazy_eval(to_serialize)
}


turn_blocks_into_fdefs <- function (code_dots) {
  lapply(code_dots, function (lazy_dot) {
    lazy_dot$expr <- substitute(function(.)x, list(x = lazy_dot$expr))
    lazy_dot
  })
}


#' All objects must be named
#'
#' By default the only one allowed unnamed object must be the
#' entry object. There must be exactly one object named "entry".
assert_names_for_serialized <- function (definition_dots)
{
  empty_names <- !nchar(names(definition_dots))
  if (sum(definition_dots) > 1)
    stop('there can be only one unnamed object')

  if (any(empty_names) && any(names(definition_dots) == 'entry'))
    stop('unnamed function and "entry" function cannot be present at the same time')

  if (any(empty_names)) {
    warning('marking the unnamed function the "entry" function')
    names(definition_dots)[empty_names] <- 'entry'
  }

  if (sum(names(definition_dots) == 'entry') != 1)
    stop('exactly one user function must be named as "entry"')

  definition_dots
}



extract_explicit_dependencies <- function (dots, classes)
{
  env_name <- function(f)    environmentName(environment(f))
  expr2chr <- function(lazy) as.character(lazy$expr)

  exprs <- lapply(dots, expr2chr)
  i <- classes == 'col_expr'
  j <- classes == 'library_symbol'

  if (any(names(exprs)[i | j] != "")) {
    stop('library functions referred by name cannot be renamed')
  }

  data.frame(
    package = c(vapply(lazy_eval(dots[j]), env_name, character(1)),
                vapply(exprs[i], `[[`, i = 2, character(1))),
    fnction = c(unlist(exprs[j]),
                vapply(exprs[i], `[[`, i = 3, character(1)))
  )
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

  if (is_colexpr(expr)) return('col_expr')
  if (is_codeblock(expr)) return('code_block')
  if (is_fundef(expr)) return('fun_definition')

  if (is.symbol(expr)) {
    # determine which symbols are in libraries and which are in session
    e <- get_containing_env(as.character(expr), lazy_obj$env)
    # TODO it might be necessary to trace the environment's parents
    #      if a function is accessible in an execution environment only
    #      its ancestor will have a name - the GlobalEnv
    if (environmentName(e) %in% search()[-1]) return('library_symbol')
    return('user_symbol')
  }

  'unsupported'
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





get_containing_env <- function (name, env) {
  if (identical(env, emptyenv())) return(env)
  if (exists(name, envir = env, mode = 'function', inherits = FALSE))
    return(env)
  get_containing_env(name, parent.env(env))
}



# will detect only the library dependencies not the globally accessible
# user-defined functions
package_ <- function (dots, libraries, .detect_libs) {
  types <- get_type(dots)
  names(dots) <- get_name(dots, types)

  # only find library dependencies, do not add function objects to serialize
}
