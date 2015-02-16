
is_fseq <- function (x) inherits(x, 'fseq')

#' Should be able to handle:
#'   * a block of code
#'   * a function definition
#'   * a pipe definition
#'   * a name (global or library function)
#'   * a name with the namespace specified (colon-expr)
#'
#' For each a single function object is returned, which,
#' when called, will perform the user task.
#'
#' If the number of formal arguments of the user object and the
#' suggested formals do not agree, an exception is thrown.
#'
#' @param lazy_obj A \code{\link[lazyeval]{lazy}} object; the expression is
#'        interpreted and turned into the \emph{user object}.
#' @param frmls Suggested formal arguments for the returned object.
#' @return A \code{function} object.
#' 
#' @importFrom lazyeval lazy_eval
prepare_user_object <- function (lazy_obj, frmls) {
  stopifnot(is_lazy(lazy_obj))
  
  make_fun <- function (frmls, bdy, env) {
    fun <- function(){}
    body(fun) <- bdy
    formals(fun) <- frmls
    # we want access global/user objects when running in locally()
    # or in to_collection(), but we don't want them directly here
    # if we are serializing this function in deferred()
    environment(fun) <- new.env(parent = env)
    fun
  }

  # examine this
  expr <- lazy_obj$expr
  
  # function referred to by name or by library::function
  if (is.name(expr) || is_colexpr(expr)) {
    if (missing(frmls)) {
      stop("'frmls' cannot be missing when handling a function name",
           call. = FALSE)
    }
    # call the function and just pass it the expected formals
    expr <- as.call(c(expr, lapply(names(frmls), as.name)))
    return(make_fun(frmls, expr, lazy_obj$env))
  }
  
  # function or pipe definition
  # the formals must be of the same length
  if (is_fundef(expr) || is_pipe(expr)) {
    fun   <- lazy_eval(lazy_obj)
    if (!missing(frmls)) {
      if (length(frmls) != length(formals(fun)))
        stop('formal arguments do not match', call. = FALSE) # TODO make that more verbose
    }
    return(fun)
  }  
  
  # block of code
  # TODO could be just any expression, so - the default....
  if (is_funexpr(expr)) {
    if (missing(frmls)) {
      stop("'frmls' cannot be missing when handling a block of code",
           call. = FALSE)
    }
    return(make_fun(frmls, expr, lazy_obj$env))
  }

  # TODO maybe we can handle more; if not, make it more explicit
  #      what is supported
  stop('do not know how to handle the lazy expression',
       call. = FALSE)
}


# --- dependencies -----------------------------------------------------

#' @importFrom dplyr bind_rows anti_join data_frame filter arrange
extract_dependencies <- function (entry_point, env) {
  stopifnot(is.function(entry_point))
  
  processing <- immediate_dependencies(entry_point, env)
  processed  <- data_frame(lib=character(), fun=character())
  
  # iterate until all globals are processed
  while(nrow(processing)) {
    # pop the first element
    examine <- processing[1, ]; processing <- processing[-1, ]
    # if already processed, skip
    if (!nrow(anti_join(examine, processed, by = c('lib', 'fun')))) next
    # if not there, add this one to processed
    processed <- bind_rows(processed, examine)
        
    # if global search for more dependencies
    if (examine$lib == 'global') {
      # it has to be accessible if immediate_dependencies returned 'global'
      more <- immediate_dependencies(get(examine$fun, envir = env), env)
      more <- anti_join(more, processed, by = c('lib', 'fun'))
      if (nrow(more))
        processing <- bind_rows(processing, more)
    }
  }
  
  # empty case
  if (is.null(processed) || !nrow(processed))
    return(data_frame(lib = character(), fun = character()))
  
  # remove primitives; there should be no empty package names
  deps <- filter(processed, lib != 'primitive' & !is.na(lib))
  stopifnot(all(nchar(deps$lib) > 0))
  
  # return everything else
  arrange(deps, lib, fun)
}

#' Works on function objects.
#' 
#' @importFrom plyr ldply llply
#' @importFrom dplyr as_data_frame data_frame %>%
immediate_dependencies <- function (fun, env) {
  if (is_fseq(fun)) {
    calls <- llply(functions(fun), function(f) find_calls(body(f))) %>% unlist
  }
  else if (is.function(fun)) {
    calls <- find_calls(body(fun))
  }
  else
    stop('do not know how to handle: ', class(fun)[1], call. = FALSE)
  
  # TODO locate_function might return NA - handle this case!
  calls <- lapply(calls, locate_function, env = env)
  calls <- calls[!vapply(calls, function(e)is.na(e$lib), logical(1))]
  
  if (!length(calls)) return(data_frame(lib=character(0), fun=character(0)))
  ldply(calls, as_data_frame, .id = NULL)
}

# from testthat:mock.r
pkg_rx <- ".*[^:]"
colons_rx <- "::(?:[:]?)"
name_rx <- ".*"
pkg_and_name_rx <- sprintf("^(?:(%s)%s)?(%s)$", pkg_rx, colons_rx, name_rx)

#' Build a function description.
#' 
#' @param fun_name Function name.
#' @param env Start searching in this \code{environment}.
#' @return A \code{list} with two elements: \emph{pkg} package name
#'         and function \emph{name}.
locate_function <- function (fun_name, env) {
  stopifnot(is.character(fun_name))
  
  # find a named environment: necessary when dealing with closures
  get_env <- function (fun) {
    e <- environment(fun)
    while(nchar(environmentName(e)) == 0) {
      e <- parent.env(e)
    }
    e
  }
  
  # extract library name and function name
  pkg_name <- gsub(pkg_and_name_rx, "\\1", fun_name)
  name <- gsub(pkg_and_name_rx, "\\2", fun_name)
  
  # if package is given explicitely
  if (pkg_name != '') return(list(lib = pkg_name, fun = name))
  
  # try to access function and determine its environment
  # start searching in 'env'
  f <- tryCatch(get(name, envir = env), error = function(x)'not-accessible')
  
  # error
  if (!is.function(f)) {
    warning('could not find function: ', fun_name)
    return(list(lib = NA, fun = name))
  }
  
  # primitive
  if (is.primitive(f)) return(list(lib = 'primitive', fun = name))
  
  # not primitive; if the top-env is global (simple case) or identical
  # closure exists in global env (e.g. result of plyr::each) then assume
  # it should be stored in the package

  # TODO to make sure that we get the environment right start looking
  #      in the environment where that name is _called_; that way there
  #      is no risk of finding an object named in the same way in the
  #      global environment, which is the case right now;
  #      we might need to track environments somewhere near where
  #      find_calls() is being called and pass it here as `env`
  
  e <- get_env(f)
  g <- try(get(name, envir = globalenv(), inherits = F, mode = 'function'), silent = T)
  if (identical(e, globalenv()) || identical(f, g))
    return(list(lib = 'global', fun = name))
  
  if (!nchar(environmentName(e)))
    warning('could not determine environment name for: ', fun_name)
  
  # some package
  list(lib = environmentName(e), fun = name)
}


# from https://stackoverflow.com/questions/14276728/finding-the-names-of-all-functions-in-an-r-expression/14295659#14295659
find_calls <- function(x) {
  # Base case
  if (!is.recursive(x)) return(character())
  
  recurse <- function(x) {
    sort(unique(as.character(unlist(lapply(x, find_calls)))))
  }
  
  if (!is.call(x)) return(recurse(x))
  
  # x[[1]] is a name
  if (!is.call(x[[1]])) {
    f_name <- as.character(x[[1]])
    return(c(f_name, recurse(x[-1])))
  }
  
  # x[[1]] is a call
  y <- x[[1]]
  if (identical(y[[1]], as.name('::')) || identical(y[[1]], as.name(':::'))) { # TODO use is_colexpr
    f_name <- deparse(y)
    return(c(f_name, recurse(x[-1])))
  }
  
  # default from the original version; something other than :: and :::
  c(as.character(x[[1]]), recurse[-1])
}


# --- globals ----------------------------------------------------------

is_global_env <- function (x) is.environment(x) && identical(x, globalenv())


#' Gathers global objects present in dependencies.
#' 
#' Finds all global functions and enclosures and prepares them for
#' serialization.
#' 
#' @importFrom magrittr extract2
#' @importFrom dplyr filter data_frame %>%
#' @importFrom plyr ldply
extract_globals <- function (deps, env) {
  names <-
    deps %>%
    filter(lib == 'global') %>%
    extract2('fun')
  funs <-
    ldply(names, function (name) {
      fun <- get(name, envir = env)
      decompose_function(fun)
    })
  cbind(data_frame(name = names), funs)
}


#' @importFrom dplyr data_frame
decompose_function <- function (fun) {
  # now determine what should be the function's environment
  # TODO make sure that the environment does not contain function
  #      which in turn have dependencies outside of this env
  e <- environment(fun)
  e <- if (is_global_env(e)) list() else as.list(e)
  environment(fun) <- emptyenv()
  data_frame(fun = list(fun), env = list(e))
}


# --- packaging user code ----------------------------------------------

#' Creates an evaluation package.
#' 
#' Packages \code{what} and its dependencies, adds info on
#' packages required to call \code{what}.
#' 
#' @param what A \code{function} object or name, a function definition
#'             or, a \code{\link{magrittr}[pipe]} definition, or a block
#'             of code.
#' @param frmls Result of calling either \code{\link{formals}} or
#'             \code{\link{alist}}.
#' 
#' @return An evaluation package object.
#'
#' @export
#' @importFrom lazyeval lazy_
#' @examples
#' 
#' package(mean)
#' package(summary)
#' package(function(x)x*x)
#' package(stats::acf)
#' package({
#'   mean(x)
#' })
#' 
package <- function (what, frmls) {
  # expression can be passed down the call stack
  x <- substitute(what)
  e <- parent.frame()
  package_(lazy_(x, e), frmls)
}


#' @param lazy_obj Lazy object; see \code{\link[lazyeval]{lazy}}
#' @export
#' @rdname package
#' 
#' @importFrom devtools session_info
#' @importFrom dplyr filter select %>%
#' @importFrom plyr alply
#' @importFrom magrittr extract2 functions
#
# TODO what about name conflicts?
package_ <- function (lazy_obj, frmls) {
  stopifnot(is_lazy(lazy_obj))
  
  user <- prepare_user_object(lazy_obj, frmls)
  deps <- extract_dependencies(user, lazy_obj$env)
  
  glbl <- bind_rows(
    cbind(name = '__entry__', decompose_function(user)),
    extract_globals(deps, lazy_obj$env)
  )
  
  pkg  <- structure(list(deps = filter(deps, lib != 'global'),
                         globals = glbl),
                    class = 'eval_pkg')
  
  # TODO pick packages... versions?...
  #   pkgs <-
  #     session_info(include_base = T)$packages %>%
  #     filter(package %in% names(deps)) %>% # maybe all loaded packages?
  #     select(package, version)
  
  pkg
}


#' @param x An object to be tested.
#' @export
#' @rdname package
is_package <- function (x) inherits(x, 'eval_pkg')


#' @export
print.eval_pkg <- function (x) {
  cat('eval-package')
  
  # entry formals
  entry <- x$global[x$global$name == '__entry__', ]
  a <- formals(entry$fun[[1]])
  n <- names(a); v <- as.character(a)
  a <- paste0(n, ifelse(nchar(v), " = ", ""), v, collapse = ', ')
  cat(paste0('(', a, ')'))
}

