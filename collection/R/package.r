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
  
  expr <- lazy_obj$expr
  if (!is_funexpr(expr) && !is_fundef(expr) && !is_pipe(expr) && !is.name(expr) && !is_colexpr(expr))
    stop('do not know how to handle lazy expression')
  
  # `deps`  will contain the dependencies
  # `user`  the user-define object to be called (if not a library function)
  # `obj`   the symbol of the user-define object (`__user__`) or the name
  #         of the library function (symbol or call if given as a::b)
  # `frmls` will contain the formal arguments of the `user` object to be
  #         called in `__entry__`
  
  # block of code
  # default formals is just a dot `.`
  if (is_funexpr(expr)) {
    deps <- get_deps(expr, lazy_obj$env)
    if (missing(frmls))
      frmls <- alist(. =)
    user <- code_to_user(expr, frmls)
    obj  <- as.name('__user__')
  }
  
  # function definition & function defined as pipe
  # default formals are the function's formals
  if (is_fundef(expr)) {
    user <- lazy_eval(lazy_obj)
    deps <- get_deps(body(user), lazy_obj$env)
    if (missing(frmls)) frmls <- formals(user)
    user <- fun_to_global('__user__', user)
    obj  <- as.name('__user__')
  }
  
  # function referred to by name
  # default formals are the function formals; `user` can be set to NULL
  # because the function was referred to by its name
  if (is.name(expr) || is_colexpr(expr)) {
    deps <- get_deps(as.call(c(expr)), lazy_obj$env)
    if (missing(frmls)) {
      search <- if (is_colexpr(expr)) deparse(expr[[3]]) else deparse(expr)
      # user provided object might not be define at all
      fun <- tryCatch(get(search, envir = lazy_obj$env),
                      error = function(e)stop(paste('could not find', search), call. = F))
      frmls <- formals(fun)
    }
    obj  <- expr
    user <- NULL
  }
  
  # pipe expression
  # it is crucial to preserve the environment! calls are defined
  # in the list of functios
  if (is_pipe(expr)) {
    user <- lazy_eval(lazy_obj)
    deps <- ldply(functions(user), function (f) {
      get_deps(body(f), lazy_obj$env)
    })
    if (missing(frmls)) frmls <- formals(user)
    user <- fun_to_global('__user__', user, environment(user))
    obj  <- as.name('__user__')
  }
  
  # load all global functions and enclosures
  global <-
    filter(deps, lib == 'global') %>%
    extract2('fun') %>%
    ldply(function(name) {
      fun <- get(name, envir = lazy_obj$env)
      # TODO make sure that the environment does not contain function
      #      which in turn have dependencies outside of this env
      env <- (if (identical(environment(fun), globalenv()))
                list()
              else
                as.list(environment(fun)))
      
      environment(fun) <- emptyenv()
      data_frame(name = name, fun = list(fun), env = list(env))
    })

  # add user object and the entry point
  global <- bind_rows(as_data_frame(global), user, create_entry(obj, frmls))
  
  # deps contain only libraries
  deps <- filter(deps, lib != 'global')
  
  # pick packages
#   pkgs <-
#     session_info(include_base = T)$packages %>%
#     filter(package %in% names(deps)) %>% # maybe all loaded packages?
#     select(package, version)
  
  # return the evaluation package
  structure(list(deps = deps, global = global), class = 'eval_pkg')
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



code_to_user <- function (code, frmls) {
  stopifnot(is_funexpr(code))
  fun          <- function(){}
  body(fun)    <- code
  formals(fun) <- frmls
  fun_to_global('__user__', fun)
}

create_entry <- function (call_it, frmls) {
  stopifnot(is.name(call_it) || is.call(call_it)) # a or a::b
  fun <- function(){}
  body(fun) <- as.call(c(call_it, lapply(names(frmls), as.name)))
  formals(fun) <- frmls
  fun_to_global('__entry__', fun)
}

#' @importFrom dplyr data_frame
fun_to_global <- function (name, fun, env = emptyenv()) {
  stopifnot(is.function(fun))
  environment(fun) <- emptyenv()
  data_frame(name = name, fun  = list(fun), env = list(as.list(env)))
}


#' Find all dependencies.
#' 
#' Search recursively for all dependencies of the given expression.
#' The expression can be a \code{call}, a \code{function} \code{\link{body}},
#' a block of code. Primitives (see \code{\link{is.primitive}}) are excluded
#' from the dependency list.
#'
#' @param A \code{language} element.
#' @return Dependencies in a \code{list}.
#'
#' @importFrom dplyr filter bind_rows
#' @importFrom plyr dlply laply .
#' @importFrom magrittr functions
#' 
#' @examples
#' 
#' get_deps(call('mean'))
#' get_deps(quote({ mean(x) }))
#' get_deps(body(function(x)x*x))
get_deps <- function (expr, env = parent.frame()) {
  stopifnot(is.language(expr))
  
  processing <- Queue$new(elements = find_calls(expr))
  processed  <- Queue$new()
  
  fns <- NULL
  while(!processing$empty()) {
    name <- processing$pop_front()
    if (processed$contains(name)) next
    
    processed$push_back(name)
    
    # search for the function in the environment enclosing the expression
    tmpf <- descr_fun(name, env)
    
    # function not found
    if (is.na(tmpf$lib)) next
    
    # if global search for more dependencies
    if (tmpf$lib == 'global') {
      fun <- get(name, envir = env) # it has to be accessible if descr_fun returned 'global'
      more <- (
        if (inherits(fun, 'fseq'))
          laply(functions(fun), function(f) find_calls(body(f)))
        else
          find_calls(body(fun))
      )
      
      more <- more[!processed$contains(more)]
      processing$push_back(more)
    }
    
    fns <- bind_rows(fns, as_data_frame(tmpf))
  }
  
  # empty case
  if (is.null(fns) || !nrow(fns)) return(data_frame(lib = character(), fun = character()))
  
  # remove primitives; there should be no empty package names
  fns <- filter(fns, lib != 'primitive' & !is.na(lib))
  stopifnot(all(nchar(fns$lib) > 0))
  
  # make a list for each package; drop all attributes but names
  fns
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
descr_fun <- function (fun_name, env = globalenv()) {
  stopifnot(is.character(fun_name))

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
  e <- get_env(f)
  g <- try(get(name, envir = globalenv(), inherits = F, mode = 'function'), silent = T)
  if (identical(e, globalenv()) || identical(f, g))
    return(list(lib = 'global', fun = name))
  
  if (!nchar(environmentName(e)))
    warning('could not determine environment name for: ', fun_name)
  
  # some package
  list(lib = environmentName(e), fun = name)
}


# find a named environment: necessary when dealing with closures
get_env <- function (fun) {
  e <- environment(fun)
  while(nchar(environmentName(e)) == 0) {
    e <- parent.env(e)
  }
  e
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

