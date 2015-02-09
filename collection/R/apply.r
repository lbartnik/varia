# accompanied with: locally (to list)
#                   deferred,
#                   to_collection(.parallel = getOption('cores', 1) | cluster)
#' @export
cply <- function (col, fun) {
  stopifnot(is_collection(col))
  stopifnot(is.function(fun))
  
  structure(list(collection = col, fun = fun), class = 'ply_task')
}

is_ply_task <- function (x) inherits(x, 'ply_task')


#' @export
#' @importFrom parallel mclapply
#' @importFrom dplyr %>%
locally <- function (task, cores = getOption('cores', 1)) {
  stopifnot(is_ply_task(task))
  apply_fun(task$col, function(path)run_fun(task$fun, path), cores)
}


#' @export
deferred <- function (task) {
  # TODO should be carried with task
  env <- parent.frame()
  # TODO won't work with library funs referred to by name
  pkg <- package_(lazy_(task$fun, env))
  
  structure(list(col = task$col, pkg = pkg), class = c('deferred_task', 'ply_task'))
}


#' @export
#' @importFrom plyr defaults
to_collection <- function (task, dest, .parallel = getOption('cores', 1)) {
  stopifnot(is_ply_task(task))
  stopifnot(is_collection(dest))
  stopifnot(!identical(task$col, dest)) # TODO remove in future but for now dest must be different from src
  
  # local case
  if (is.numeric(.parallel)) {
    # replace the function: save results to dest
    fun <-
      function(path) {
        res <- run_fun(task$fun, path)
        if (is_error(res)) return(res)
        
        # add tags if present in the result
        tags <- c(src_id = basename(path), src_col = path(task$col))
        if (has_tags(res))
          tags <- defaults(tags, attr(res, 'tags'))
        
        # add object to dest and return its id
        id <- add_(dest, no_tags(res), .dots = tags)
        as.character(id)
      }
    
    # apply the modified function
    new_col <- apply_fun(task$col, fun, .parallel)
    
    # it is a ply_result but a collection too
    return(structure(new_col,
                     class = c(class(new_col), 'collection'),
                     path = path(col)))
  }
  
  # TODO other cases
  stop('only numeric .parallel is supported now')
}


# just load the data and run user function
run_fun <- function (fun, path) {
  obj <- readRDS(paste0(path, '.rds'))
  tgs <- readRDS(paste0(path, '_tags.rds'))
  tryCatch(
    do.call(fun, list(obj, tgs)),
    error = function(e) e
  )
}


#' @import parallelFrom mclapply
apply_fun <- function (col, fun, cores) {
  files <- file.path(path(col), make_path(col))
  
  # apply in parallel
  res <-
    if (cores > 1)
      mclapply(files, fun, mc.cores = cores)
    else
      lapply(files, fun)
  
  # ids as names but remove attributes from col
  names(res) <- as.character(col)
  
  # separate errors & correct results
  I <- vapply(res, is_error, logical(1))
  
  err <- if (any(I)) vapply(res[I], function(e)e$message, character(1)) else character(0)
  res <- if (any(!I)) res[!I] else list()
  
  # move errors to an attribute
  attr(res, 'errors') <- err
  attr(res, 'source') <- path(col)
  class(res) <- c('ply_result', class(res))
  
  res
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
    cat('1: ', err[1], if(length(err)>1)'\n...', '\n')
  }
  
  attr(x, 'errors') <- NULL
  print(`class<-`(x, class(x)[-1])) # remove ply_result class
}




# --- old ply ----------------------------------------------------------

#' @importFrom parallel mclapply
#' @importFrom dplyr %>%
cxply <- function (col, fun, cores = getOption('cores', 1), plyrfun) {
  stopifnot(is_collection(col))
  stopifnot(is.function(fun))
  
  res <-
    file.path(path(col), make_path(col)) %>%
    mclapply(function (path) {
      obj <- readRDS(paste0(path, '.rds'))
      tgs <- readRDS(paste0(path, '_tags.rds'))
      tryCatch(
        do.call(fun, list(obj, tgs)),
        error = function(e) e
      )
    }, mc.cores = cores)
  names(res) <- col[] # remove attributes
  
  # move errors to attribute
  I <- vapply(res, function(e)inherits(e, 'error'), logical(1))
  
  err <- if (any(I)) vapply(res[I], function(e)e$message, character(1)) else character(0)
  res <- if (any(!I)) plyrfun(res[!I])[] else list()
  
  attr(res, 'errors') <- err
  class(res) <- c('ply_result', class(res))
  res
}


create_ply <- function(plyrfun) {
  plyrfun <- deparse(substitute(plyrfun))
  fun <- substitute(
          function (col, fun, cores = getOption('cores', 1))
            cxply(col, fun, cores, x),
          list(x = as.name(plyrfun))
         )
  fun <- parse(text = deparse(fun)) # eval(substitute(...)) doesn't work without it
  eval(fun, envir = parent.frame())
}

#' Apply a \code{function} to every object/tagset in collection.
#' @export
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



# --- experimental -----------------------------------------------------

if (F) {

#' @export
oply <- function (col, what) {
  w <- substitute(what)
  e <- parent.frame()
  l <- lazy_(w, e)
  oply_(col, l)
}

#' @export
oply_ <- function (col, lazy_obj) {
  p <- package_(lazy_obj, alist(object=))
  p$data <- col
  class(p) <- c('oply', 'ply_pkg', class(p))
  p
}


is_ply_pkg <- function (x) inherits(x, 'ply_pkg')

#' @export
print.ply_pkg <- function (x) {
  cat('data-apply package\n')
  cat('* data: ', path(x$data), '\n')
  print(x$data)
  cat('\n* code:\n')
  print(`class<-`(x, 'eval_pkg'))
}

run_ply <- function (x, path) UseMethod('run_ply')

run_ply.default <- function (x, path) {
  stop('do not know how to run object of class: ',
       paste(class(x), collapse = ' '),
       call. = FALSE)
}

run_ply.oply <- function (x, path) {
  obj <- readRDS(paste0(path, '.rds'))
  pkg_eval(x, list(object = obj))
}
run_ply.tply <- function (x, path) {
  tgs <- readRDS(paste0(path, '_tags.rds'))
  pkg_eval(x, list(tags = tgs))
}
run_ply.bply <- function (x, path) {
  obj <- readRDS(paste0(path, '.rds'))
  tgs <- readRDS(paste0(path, '_tags.rds'))
  pkg_eval(x, list(object = obj, tags = tgs))
}



#' @export
#' @importFrom parallel mclapply
#' @importFrom dplyr %>%
to_ram <- function (pkg, cores = 1) {
  stopifnot(is_ply_pkg(pkg))

  res <-
    file.path(path(pkg$data), make_path(pkg$data)) %>%
    mclapply(function(path)run_ply(pkg, path),
             mc.cores = cores)
  
  res
}

} # if (F)