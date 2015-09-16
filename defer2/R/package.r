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
#' @importFrom lazyeval lazy_dots lazy_
#' @export
package <- function (entry, ..., .inherit = TRUE)
{
  dots <- lazy_dots(...)
  if (any(names(dots) == 'entry')) {
    stop('dependencency cannot have name "entry"', call. = FALSE)
  }

  dots$entry <- lazy_(substitute(entry), parent.frame())
  class <- classify_lazy_dots(dots)

  # print expression which are not supported
  if (any(i <- (class == 'unsupported'))) {
    stop('expressions in dots are not supported: ', toString(dots[i]),
         call. = FALSE)
  }

  # functions to be serialized are defined by this condition
  # and global dependencies by its negation
  i_fun <- class %in% c('fun_definition', 'code_block', 'user_symbol')

  user_objects <- process_user_objects(dots[i_fun], class[i_fun])
  dependencies <- process_global_dependencies(dots[!i_fun], class[!i_fun])

  # TODO detect other library and user-defined dependencies by examining
  #      the body of each function which is to be serialized

  structure(list(objects = user_objects, dependencies = dependencies),
            class = 'evaluation_package')
}




# check where do the functions belong - if they belong in a package, then they
# are just library dependencies and all we need to do is write down their name
# and their package name;
# if they belong in the global environment or and environment that is a
# descendant of the global environment - then they are function dependencies
# that need to be serialized
classify_lazy_dots <- function (dots)
{
  stopifnot(all(vapply(dots, is_lazy, logical(1))))

  # determine class of expression
  class <- vapply(dots, function (lazy_obj) {
    if (is_colexpr(lazy_obj$expr))   return('col_expr')
    if (is_codeblock(lazy_obj$expr)) return('code_block')
    if (is_fundef(lazy_obj$expr))    return('fun_definition')
    if (is.symbol(lazy_obj$expr))    return('symbol')
    'unsupported'
  }, character(1))

  # determine which symbols refer to library functions
  i_lf  <- vapply(dots[class == 'symbol'], is_library_function, logical(1))
  class[class == 'symbol'][i_lf]  <- 'library_symbol'
  class[class == 'symbol'][!i_lf] <- 'user_symbol'

  class
}


is_library_function <- function(lazy_obj)
{
  stopifnot(is_lazy(lazy_obj))
  e <- get_containing_env(as.character(lazy_obj$expr), lazy_obj$env)

  environmentName(e) %in% search()[-1] || identical(e, baseenv())
}


get_containing_env <- function (name, env)
{
  if (identical(env, emptyenv())) return(env)
  if (exists(name, envir = env, mode = 'function', inherits = FALSE))
    return(env)
  Recall(name, parent.env(env))
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
  if (any(names(dots) == ""))
    stop('all functions and code blocks must be named', call. = FALSE)

  # name global symbols
  i_sym <- (class == 'symbol')
  exprs <- lapply(dots[i_sym], `[[`, i = 'expr')
  names(dots[i_sym]) <- as.character(exprs)

  lapply(dots, lazy_eval)
}


turn_block_into_fdef <- function (lazy_dot)
{
  lazy_dot$expr <- substitute(function(.)x, list(x = lazy_dot$expr))
  lazy_dot
}


#' @importFrom lazyeval lazy_eval
process_global_dependencies <- function (dots, class)
{
  if (any(names(dots) != "")) {
    stop('library functions cannot be renamed', call. = FALSE)
  }

  exprs <- lapply(dots, function(lazy) as.character(lazy$expr))
  i <- (class == 'col_expr')

  # colon expressions go first
  d1 <- data.frame(
    package = vapply(exprs[i], `[[`, i = 2, character(1)),
    fnction = vapply(exprs[i], `[[`, i = 3, character(1))
  )

  # function names
  d2 <- data.frame(
    package = vapply(lapply(dots[!i], lazy_eval), function(f) environmentName(environment(f)),
                     character(1)),
    fnction = unlist(exprs[!i])
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
#' @return logical - TRUE if expr is enclosed in \code{\{}, FALSE otherwise.
is_codeblock <- function(expr) {
  is.call(expr) && identical(expr[[1L]], quote(`{`))
}

is_fundef <- function(x) {
  is.call(x) && identical(x[[1]], quote(`function`))
}
