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
  
  # block of code
  # default formals is just a dot `.`
  if (is_funexpr(expr)) {
    if (missing(frmls)) frmls <- alist(. =)
    calls  <- extract_calls(expr, lazy_obj$env)
    global <- bind_rows(extract_global(calls, lazy_obj$env),
                        code_to_user(expr, frmls),
                        create_entry(as.name('__user__'), frmls))
  }
  
  # function referred to by name || library::function
  #
  # default formals are the function formals; `user` can be set to NULL
  # because the function was referred to by its name
  if (is.name(expr) || is_colexpr(expr)) {
    # name might not be defined at all
    fun <- tryCatch(lazy_eval(lazy_obj),
                    error = function(e)stop('could not find ', deparse(expr), call. = F))
    
    if (missing(frmls)) frmls <- formals(fun)
    calls  <- extract_calls(as.call(list(expr)), lazy_obj$env)
    global <- bind_rows(extract_global(calls, lazy_obj$env),
                        create_entry(expr, frmls))
  }
  
  # function definition
  # default formals are the function's formals
  if (is_fundef(expr)) {
    fun   <- lazy_eval(lazy_obj)
    calls <- extract_calls(body(fun), lazy_obj$env)
    if (missing(frmls)) frmls <- formals(fun)
    
    global <- bind_rows(extract_global(calls, lazy_obj$env),
                        fun_to_global('__user__', fun),
                        create_entry(as.name('__user__'), frmls))
  }
    
  # pipe expression
  # it is crucial to preserve the environment! calls are defined
  # in the list of functios
  if (is_pipe(expr)) {
    fun   <- lazy_eval(lazy_obj)
    calls <- ldply(functions(fun), function (f) {
      extract_calls(body(f), lazy_obj$env)
    })
    if (missing(frmls)) frmls <- formals(fun)
    
    global <- bind_rows(extract_global(calls, lazy_obj$env),
                        fun_to_global('__user__', fun, environment(fun)),
                        create_entry(as.name('__user__'), frmls))
  }
  
  # pick packages
#   pkgs <-
#     session_info(include_base = T)$packages %>%
#     filter(package %in% names(deps)) %>% # maybe all loaded packages?
#     select(package, version)
  
  # return the evaluation package
  return(structure(list(deps   = filter(calls, lib != 'global'),
                        global = global),
                   class = 'eval_pkg'))
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


#' @importFrom magrittr extract2
#' @importFrom dplyr filter data_frame %>%
#' @importFrom plyr ldply
extract_global <- function (calls, env) {
  # find & load all global functions and enclosures
  filter(calls, lib == 'global') %>%
    extract2('fun') %>%
    ldply(function(name) {
      fun <- get(name, envir = env)
      # TODO make sure that the environment does not contain function
      #      which in turn have dependencies outside of this env
      env <- (if (identical(environment(fun), globalenv()))
                list()
              else
                as.list(environment(fun)))
      
      environment(fun) <- emptyenv()
      data_frame(name = name, fun = list(fun), env = list(env))
    }) %>%
    as_data_frame
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
#' extract_calls(call('mean'))
#' extract_calls(quote({ mean(x) }))
#' extract_calls(body(function(x)x*x))
extract_calls <- function (obj, env = parent.frame()) {
  stopifnot(is.language(obj) || is.function(obj))
  
  # internal
  process_fun <- function (fun) {
    if (inherits(fun, 'fseq'))
      laply(functions(fun), function(f) find_calls(body(f)))
    else
      find_calls(body(fun))
  }
  
  # queues
  processing <- Queue$new()
  processed  <- Queue$new()
  
  # initialize
  if (is.language(obj))
    processing$push_back(find_calls(obj))
  else
    processing$push_back(process_fun(fun))

  # iterate until all globals are processed
  fns <- NULL
  while(!processing$empty()) {
    name <- processing$pop_front()
    if (processed$contains(name)) next
    
    processed$push_back(name)
    
    # search for the function in the environment enclosing the expression
    tmpf <- locate_function(name, env)
    
    # function not found
    if (is.na(tmpf$lib)) next
    
    # if global search for more dependencies
    if (tmpf$lib == 'global') {
      fun  <- get(name, envir = env) # it has to be accessible if locate_function returned 'global'
      more <- process_fun(fun)
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
