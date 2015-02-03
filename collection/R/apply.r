
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
