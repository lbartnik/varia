#' Creates an evaluation package.
#' 
#' Packages \code{what} and its dependencies, adds info on
#' packages required to call \code{what}.
#' 
#' @param what A \code{function} object or name, a function
#' definition or, a \code{\link{magrittr}[pipe]} definition,
#' or a block of code.
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
package <- function (what) {
  # expression can be passed down the call stack
  x <- substitute(what)
  e <- parent.frame()
  package_(lazy_(x, e))
}


#' @param lazy_obj Lazy object; see \code{\link[lazyeval]{lazy}}
#' @export
#' @rdname package
#' 
#' @importFrom devtools session_info
#' @importFrom dplyr filter select %>%
# TODO what about name conflicts?
# TODO what about pipes? the environments there contain crucial elements
package_ <- function (lazy_obj) {
  stopifnot(is_lazy(lazy_obj))
  
  expr <- lazy_obj$expr
  if (!is_funexpr(expr) && !is_fundef(expr) && !is_pipe(expr) && !is.name(expr) && !is_colexpr(expr))
    stop('do not know how to handle lazy expression')
  
  # block of code
  if (is_funexpr(expr)) {
    # get all calls recursively, set expr as 'exec'
    obj  <- expr
    deps <- get_deps(obj, lazy_obj$env)
  }
  
  # function definition & function defined as pipe
  if (is_fundef(expr) || is_pipe(expr)) {
    # evaluate the function, get all calls recursively, set function obj as 'exec'
    obj <- lazy_eval(lazy_obj)
    assign('__anonymous__', obj, envir = lazy_obj$env)
    deps <- get_deps(call('__anonymous__'), lazy_obj$env)
    obj  <- '__anonymous__'
  }
  
  # function referred to by name
  if (is.name(expr) || is_colexpr(expr)) {
    obj  <- deparse(expr)
    deps <- get_deps(call(obj), lazy_obj$env)
  }
  
  # load all global functions and enclosures
  if ('global' %in% names(deps)) {
    nms         <- deps$global
    deps$global <- lapply(deps$global, function (name) {
      fun <- get(name, envir = lazy_obj$env)
      # TODO make sure that the environment does not contain function
      #      which in turn have dependencies outside of this env
      env <- if (identical(environment(fun), globalenv()))
                list() else as.list(environment(fun))
      environment(fun) <- emptyenv()
      list(fun = fun, env = env)
    })
    names(deps$global) <- nms
  }

  # pick packages
  pkgs <-
    session_info(include_base = T)$packages %>%
    filter(package %in% names(deps)) %>% # maybe all loaded packages?
    select(package, version)
  
  # return the package
  structure(list(exec = obj, deps = deps, packages = pkgs),
            class = 'evaluation_package')
}


#' @param x An object to be tested.
#' @export
#' @rdname package
is_package <- function (x) inherits(x, 'evaluation_package')


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
#' @importFrom plyr dlply .
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
    
    if (tmpf$pkg == 'global') {
      more <- find_calls(call(name))
      more <- more[!processed$contains(more)]
      processing$push_back(more)
    }
    
    fns <- bind_rows(fns, as_data_frame(tmpf))
  }
  
  # empty case
  if (is.null(fns) || !nrow(fns)) return(list())
  
  # remove primitives; there should be no empty package names
  fns <- filter(fns, pkg != 'primitive')
  stopifnot(all(fns$pkg != ''))
  
  # make a list for each package; drop all attributes but names
  deps <- dlply(fns, .(pkg), function(x)unique(x$name))
  attributes(deps) <- list(names = names(deps))
  deps
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
  if (pkg_name != '') return(list(pkg = pkg_name, name = name))
  
  # try to access function and determine its environment
  # start searching in 'env'
  f <- tryCatch(get(name, envir = env), error = function(x)'not-accessible')
  
  # error
  if (!is.function(f)) {
    warning('could not find function: ', fun_name)
    return(c(pkg = NA, name = name))
  }
  
  # primitive
  if (is.primitive(f)) return(list(pkg = 'primitive', name = name))
  
  # not primitive; determine the environment
  e <- get_env(f)
  if (identical(e, globalenv())) return(list(pkg = 'global', name = name))
  
  if (!nchar(environmentName(e)))
    warning('could not determine environment name for: ', fun_name)
  
  # some package
  list(pkg = environmentName(e), name = name)
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


#' Evaluate a package.
#' 
#' @param pkg Package to be avaluated.
#' 
#' @export
#' @examples
#' \dontrun{
#' pkg <- package(mean)
#' eval_pkg(pkg)
#' }
eval_pkg <- function (pkg) {
  stopifnot(is_package(pkg))  
  
  errors <- Queue$new()
  e <- new.env(parent = globalenv())
  
  # 1. load required packages
  for (name in pkg$packages$package) {
    if (!require(name, quietly = T))
      errors$push_back(paste('could not load package', name))
  }
  
  if (!errors$empty()) return(errors) # how to handle errors? result object?
  
  # 2. assign global functions
  if ('global' %in% names(pkg$deps)) {
    glb <- pkg$deps$global
    for (name in names(glb)) {
      assign(name, glb[[name]], envir = e)
    }
  }
  
  # 3. make sure all other functions are available
  for (pkg_name in setdiff(names(pkg$deps), 'global')) {
    for (func_name in pkg$deps[[pkg_name]]) {
      res <- tryCatch(`::`(pkg_name, func_name), error = toString)
      if (!is.function(res)) {
        errors$push_back(paste0('function ', pkg_name, '::',
                                func_name, ' not available'))
      }
    }
  }
  
  # 4. evaluate code/call function
  e$obj <- list() # object
  e$tag <- list() # tag(s)
  
  f <- function(obj, tag){}
  environment(f) <- new.env(parent = e)
  
  if (is.character(pkg$exec))
    body(f) <- as.call(c(as.name(pkg$exec), quote(obj), quote(tag)))
  else if (is_funexpr(pkg$exec))
    body(f) <- pkg$exec
  
  e$f <- f
  eval(as.call(quote(f), quote(obj), quote(tag)), e)
  
  # TODO finish
  # 5. process results
}







