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







#' @importFrom plyr llply
#' @importFrom tools file_path_sans_ext
cxply <- function (col, fun, plyrfun) {
  stopifnot(is_collection(col))
  stopifnot(is.function(fun))
  
  file.path(path(col), make_path(col)) %>%
    plyrfun(function (path) {
      obj <- readRDS(paste0(path, '.rds'))
      tgs <- readRDS(paste0(path, '_tags.rds'))
      do.call(fun, list(obj, tgs))
    })
}


#' Apply a \code{function} to every object/tagset in collection.
#' @export
#' @examples
#' \dontrun{
#' col <- collection('sample-col')
#' sdply(select(col), function (obj, tag) {
#'   c(dim(obj), length(tag))
#' })
#' }
caply <- function (col, fun) cxply(col, fun, laply)

#' @export
#' @rdname caply
cdply <- function (col, fun) cxply(col, fun, ldply)

#' @export
#' @rdname caply
clply <- function (col, fun) cxply(col, fun, llply)

#' @export
#' @rdname caply
c_ply <- function (col, fun) cxply(col, fun, l_ply)


#' @export
objects <- function (col) {
  clply(col, function(o, t) o)
}
