# --- application: internal functions ----------------------------------

# apply a function to both object and its tags
c_apply <- function (path, fun) {
  obj <- readRDS(paste0(path, '.rds'))
  tgs <- read_tags(path)
  tryCatch(do.call(fun, list(obj, tgs)),  error = function(e) e)
}

# apply a function only to tags
t_apply <- function (path, fun) {
  tgs <- read_tags(path)
  tryCatch(do.call(fun, list(tgs)),  error = function(e) e)
}

# apply to a group of objects and tags
c_apply_grouped <- function (path, fun) {
  obj <- lapply(paste0(path, '.rds'), readRDS)
  tgs <- lapply(path, read_tags)
  tryCatch(do.call(fun, list(obj, tgs)),  error = function(e) e)
}

# apply to a group of tags
t_apply_grouped <- function (path, fun) {
  tgs <- lapply(path, read_tags)
  tryCatch(do.call(fun, list(tgs)),  error = function(e) e)
}


# apply function to a list of paths
run_function <- function (col, wrapper, user, cores) {
  files <- file.path(path(col), make_path(col))
  res   <- run_in_parallel(files, wrapper, user, cores)
  names(res) <- as.character(col) # ids as names but remove attributes from col
  res
}

#' @importFrom dplyr do
run_function_grouped <- function (col, wrapper, user, cores) {
  # read and group paths
  grp <- attr(col, 'grouped')
  grp$.file <- file.path(path(col), make_path(col))
  files <- do(grp, .files = .$.file)$.files # a list of vectors
  
  # execute
  res <- run_in_parallel(files, wrapper, user, cores)
  
  # merge grouping labels, separated with a dot
  lb <- attr(attr(col, 'grouped'), 'labels')
  names(res) <- do.call(paste, c(as.list(lb), list(sep = '.')))

  res
}

#' run function in parallel; hide the backend
#' @importFrom parallel mclapply
run_in_parallel <- function (inputs, wrapper, user, cores) {
  if (cores > 1)
    res <- mclapply(inputs, wrapper, fun = user, mc.cores = cores)
  else
    res <- lapply(inputs, wrapper, fun = user)
  
  names(res) <- basename(inputs)
  res
}


is_ply_result <- function (x) inherits(x, 'ply_result')

# assumes `outputs` to be a list of valid resutls mixed with error objects
as_ply_result <- function (outputs) {
  # separate errors & correct results
  I <- vapply(outputs, is_error, logical(1))
  
  err <- if (any(I)) outputs[I] else list()
  res <- if (any(!I)) outputs[!I] else list()
  
  list(res = res, err = err)
}

#' @export
print.ply_result <- function (x) {
  err <- attr(x, 'errors')
  
  # no errors case
  if (!length(err)) {
    cat('*ply result: no errors\n')
  }
  else {
    # there are errors
    cat('*ply result - with', length(err), 'error(s):\n')
    cat('1: ', err[[1]]$message, 'in', deparse(err[[1]]$call), '\n')
    if (length(err) > 1)
      cat('...\n')
  }
  
  attr(x, 'errors') <- NULL
  print(`class<-`(x, class(x)[-1])) # remove ply_result class
}


# --- application: public API ------------------------------------------

#' Create apply task.
#' 
#' Make ready to run the given expressino on every object in the
#' collection. This function creates a task which consists of the
#' collection itself and a \code{\link[lazyeval]{lazy}} object
#' containing the expression.
#' 
#' The task can be evaluated with either of: \code{\link{locally}},
#' \code{\link{to_collection}} or \code{\link{deferred}} combined
#' with \code{\link{run_deferred}} run on a remote host.
#' 
#' \code{cply} applies \code{expr} to every object-tags pair.
#' 
#' @param col Collection to apply \code{expr} to.
#' @param expr Expression to be run; see \code{\link{package}}.
#' @return A \emph{ply_task} object.
#' 
#' @export
#' @importFrom lazyeval lazy_
cply <- function (col, expr) {
  expr <- substitute(expr)
  env  <- parent.frame()
  cply_(col, lazy_(expr, env))
}

#' @param lazy_obj Lazy object with the unevaluated \code{expr}.
#' @rdname cply
#' @export
cply_ <- function (col, lazy_obj) {
  make_ply(col, lazy_obj, 'cply')
}

#' \code{tply} applies \code{expr} on all tags.
#' 
#' @rdname cply
#' @export
#' @importFrom lazyeval lazy_
tply <- function (col, expr) {
  expr <- substitute(expr)
  env  <- parent.frame()
  tply_(col, lazy_(expr, env))  
}

#' @rdname cply
#' @export
tply_ <- function (col, lazy_obj) {
  make_ply(col, lazy_obj, 'cply')
}

make_ply <- function (col, lazy_obj, task_class) {
  stopifnot(is_collection(col))
  stopifnot(is_lazy(lazy_obj))
  structure(list(collection = col, lazy_obj = lazy_obj),
            class = c(task_class, 'ply_task'))
}

is_ply_task <- function (x) inherits(x, 'ply_task')

#' @export
print.ply_task <- function (x) {
  cat('*ply task')
  cat('\n collection: ', collection_name(x$col))
  cat('\n function  : ', deparse(body(x$fun)))
}


# --- application: execute the task ------------------------------------


# TODO the next step will be to move parts of package() here
extract_user_code <- function (lazy_obj) {
  if (is_funexpr(lazy_obj$expr))
    stop('block of code is not supported yet', call. = FALSE)
  lazy_eval(lazy_obj)
}

#' @export
locally <- function (task, cores = getOption('cores', 1)) {
  stopifnot(is_ply_task(task))
  
  user <- extract_user_code(task$lazy_obj)
  
  if (is_grouped(task$col)) {
    fun <- if (inherits(task, 'cply')) c_apply_grouped else t_apply_grouped
    res <- run_function_grouped(task$col, fun, user, cores)
  }
  else {
    fun <- if (inherits(task, 'cply')) c_apply else t_apply
    res <- run_function(task$col, fun, user, cores)
  }
  
  y <- as_ply_result(res)
  structure(y$res, 'errors' = y$err, class = 'ply_result')
}


#' Apply task and store results.
#'
#' Applies \code{task} to collection specified there and stores results
#' in another collection \code{dest}.
#' 
#' If the user-provided function returns a \code{NULL} value, then the
#' output object is not stored.
#' 
#' @param task Object crate by \code{\link{cply}}.
#' @param dest Destination \code{\link{collection}}.
#' @param .parallel A positive \code{numeric} means the number of cores to be used.
#' @return A \code{\link{collection}} consisting of the newly created objects.
#'
#' @seealso \code{\link{cply}} \code{\link{locally}} \code{\link{with_tags}}
#' @export
#' @importFrom plyr defaults
to_collection <- function (task, dest, .parallel = getOption('cores', 1)) {
  # it has to be a collection-level task, not a tag-level one, bc
  # the user function is expected to return objects _with_ tags, not
  # tags only
  stopifnot(is_ply_task(task))
  stopifnot(inherits(task, 'cply'))
  stopifnot(is_collection(dest))
  
  # TODO remove in future but for now dest must be different from src
  stopifnot(!identical(task$col, dest))
  
  # local case
  if (is.numeric(.parallel)) {
    inner_fun <- if (is_grouped(task$col)) c_apply_grouped else c_apply
    user_fun  <- extract_user_code(task$lazy_obj)
    
    # outer function saves objects to `dest` and returns identifiers
    outer_fun <- function(path, fun) {
        res <- inner_fun(path, fun)
        if (is_error(res) || is.null(res)) return(res)
        
        tags <- list(`.src_id` = basename(path), `.src_col` = path(task$col))
        if (has_tags(res))
          tags <- defaults(tags, attr(res, 'tags'))
        
        as.character(add_object_(dest, no_tags(res), .tags = tags)) # return the id
      }
    
    if (is_grouped(task$col))
      res <- run_function_grouped(task$col, outer_fun, user_fun, .parallel)
    else
      res <- run_function(task$col, outer_fun, user_fun, .parallel)
    
    # notify user
    message("collection has changed, refresh the 'dest' object")
    
    # separate OKs from errors
    y <- as_ply_result(res)
    
    # remove NULLs
    ids <- y$res[!vapply(y$res, is.null, logical(1))]
    ids <- unlist(ids)
    
    # TODO maybe add ids to whatever is in dest; or make sure that dest is empty!
    #      it does not make semantical sense any other way
    return(structure(ids, path = path(dest), errors = y$err,
                     class = c('ply_result', 'collection')))
  }
  
  # TODO other cases
  stop('only numeric .parallel is supported now')
}


#' @export
deferred <- function (task) {
  # TODO should be carried with task
  env <- parent.frame()
  # TODO add dependencies to task$lazy_obj; user extract_user_code as the first step,
  #      the call get_dependencies on its result
  # pkg <- package_(...)
  #
  #structure(list(col = task$col, pkg = pkg), class = c('deferred_task', 'ply_task'))
  stop('not supported yet', call. = FALSE)
}


# --- application: shortcuts -------------------------------------------

# TODO add ... to cxply and to cply; it will have to be stored in ply_task (???)
# TODO add .progress

#' @importFrom dplyr %>%
cxply <- function (col, fun, cores = getOption('cores', 1)) {
  res <- cply(col, fun) %>% locally(cores)
  err <- attr(res, 'errors')
  res <- plyrfun(res)
  
  if (is_ply_result(res))
    return(res)
  
  structure(res, errors = err, class = c('ply_result', class(res)))
}

create_ply <- function(plyrfun) {
  repl <- substitute(plyrfun)  
  expr <- substitute(substitute(zzz, list(plyrfun = repl)), list(zzz = body(cxply)))
  as.function(c(formals(cxply), eval(expr)), envir = parent.frame())
}

#' Apply a \code{function} to every object/tagset in collection.
#' 
#' @export
#' @importFrom plyr laply ldply llply l_ply
#' 
#' @examples
#' \dontrun{
#' col <- collection('sample-col')
#' cdply(select(col), function (obj, tag) {
#'   c(dim(obj), length(tag))
#' })
#' }
caply <- create_ply(laply)

#' @export
#' @rdname caply
cdply <- create_ply(ldply)

#' @export
#' @rdname caply
clply <- create_ply(llply)

#' @export
#' @rdname caply
c_ply <- create_ply(l_ply)


#' @export
objects <- function (col) {
  clply(col, function(o, t) o)
}
