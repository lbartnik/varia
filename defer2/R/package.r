#' @importFrom lazyeval lazy_dots
#' @export
package <- function (..., .inherit = TRUE) {
  dots <- lazy_dots(...)
  calling_env <- parent.frame()

  # 1. there must be names, and one of the must be entry
  # 2. there can be several classes of objects:
  #      - code_block: { some ; code }
  #      - func_def:   function(x)x
  #      - name:       fun, rnorm, base::abs
  # 3. we must turn object into lazy object first to examine their class
  #    then we separate function names from function/code objects
  # 4. function/code objects are examined for dependencies while function
  #    names are just remembered as pointers to dependencies


  # 1. lazyfile objects
  # 2. make sure all objects have names
  # 2. determine which are function definitions, code blocks - and which are names
  # 3. function defs/code blocks must all have names - there can be one object without
  #    a name and that becomes the entry point
  # 4. determine which names refer to objects accessible from the calling environment
  #    and add them to user objects

  # this divides the input into two categories: (1) definitions of functions/code
  # blocks and (2) function names;
  # category (1) will be parsed for dependencies and serialized
  #
  # category (2) needs to be divided into (2a.) global/user functions and
  # (2b.) library functions
  # (2a.) is added to group (1); (2b.) is recorded in a table - package
  # name + function name

  # extract expressions and determine their classes
  classes <- determine_classes(dots)

  # extract functions that need to be serialized and make sure they are all
  # named
  serialized_dots <- prepare_serialized(dots, classes)
  dependency_dots <- prepare_dependencies(dots, classes)


  # check where do the functions belong - if they belong in a package, then they
  # are just library dependencies and all we need to do is write down their name
  # and their package name;
  # if they belong in the global environment or and environment that is a
  # descendant of the global environment - then they are function dependencies
  # that need to be serialized



  #package_(dots, calling_env, .inherit)
}

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
determine_classes <- function (dots)
{
  classes <- vapply(dots, classify_lazy, character(1))

  # check for unsupported types of objects
  if (any(i <- (classes == 'unsupported'))) {
    exprs <- lapply(dots[i], `[[`, i = 'expr')
    stop('expressions in dots are not supported: ', toString(exprs))
  }

  classes
}


prepare_serialized <- function (dots, classes)
{
  # put names on user symbols
  i_usr <- (classes == 'user_symbol')
  exprs <- lapply(dots, `[[`, i = 'expr')
  names(dots[i_usr]) <- as.character(exprs[i_usr])

  # pick objects to be serialized
  serialized <- dots[classes %in% c('fun_definition', 'code_block', 'user_symbol')]
  serialized <- name_serialized(serialized)

  # TODO turn code blocks into function

  # TODO evaluate all (lazy) objects to be able to process them for dependencies

  serialized
}


#' All objects must be named
#'
#' By default the only one allowed unnamed object must be the
#' entry object. There must be exactly one object named "entry".
name_serialized <- function (serialized_dots)
{
  empty_names <- !nchar(names(serialized_dots))
  if (sum(serialized_dots) > 1)
    stop('there can be only one unnamed object')

  if (any(empty_names) && any(names(serialized_dots) == 'entry'))
    stop('unnamed function and "entry" function cannot be present at the same time')

  if (any(empty_names)) {
    warning('marking the unnamed function the "entry" function')
    names(serialized_dots)[empty_names] <- 'entry'
  }

  if (sum(names(serialized_dots) == 'entry') != 1)
    stop('exactly one user function must be named as "entry"')

  serialized_dots
}



prepare_dependencies <- function (dots, classes)
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


classify_lazy <- function (lazy_obj)
{
  stopifnot(is_lazy(lazy_obj))
  expr <- lazy_obj$expr

  if (is_colexpr(expr)) return('col_expr')
  if (is_codeblock(expr)) return('code_block')
  if (is_fundef(expr)) return('fun_definition')

  if (is.symbol(expr)) {
    # determine which symbols are in libraries and which are in session
    e <- get_containing_env(as.character(expr), lazy_obj$env)
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




package_ <- function (dots, calling_env, .inherit) {
  types <- get_type(dots)
  names(dots) <- get_name(dots, types)

  # only find library dependencies, do not add function objects to serialize
}
