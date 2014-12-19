# 0. move collection-related code here
# 1. start with a collection (forget about reposiories)
# 2. enable adding and search objects in a collection (tags!) + TDD with testthat
# 3. implement run_pipe method for a selection of objects/whole collection
# 4. implement storing results back to a collection
# 5. add repositories

#' Create a collection.
#' 
#' Collection is a named container for objects. Internally it points
#' to a directory in the file system and stores object in rds files.
#' The files are organized in a multi-level directory tree so that
#' there number of files in one directory does not exceed limits of
#' any given file system.
#' 
#' Multiple collections can be stored in the same directory.
#' 
#' @export
collection <- function (name = 'base', path = 'my-collection', create = F) {
  if (!file.exists(path)) {
    if (!create)
      stop('collection directory does not exist: ', path, call. = F)
    dir.create(path, recursive = T)
  }
  structure(list(path = path, selector = selector(name = name)),
            class = 'collection')
}

#' @export
is_collection <- function (x) inherits(x, 'collection')

#' @export
add_object <- function (c, x, ...) {
  tags <- lazy_dots(...)
  add_object_(c, x, .tags = tags)
}

#' @export
add_object_ <- function (c, x, ..., .tags) {
  stopifnot(is_collection(c))
  x <- with_id(x)
  p <- file.path(c$path, expand_name(id_to_file(x)))
  if (file.exists(p))
    stop('object already in collection', call. = F)
  
  tags <- all_dots(.tags, ..., all_named = T)
  tags <- lazy_eval(tags, x)
  tags$id <- id(x)
  tags$collection_name <- get_tag(c$selector, 'name')
  
  dir.create(dirname(p), recursive = T, showWarnings = F)
  saveRDS(x, p)
  p <- file.path(c$path, expand_name(id_to_tags(x)))
  saveRDS(tags, p)
}






