#' @export
create_repo <- function (path, create = F) {
  if (file.exists(path)) {
    if (!file.info(path)$isdir)
      stop('path points to a file not directory')
    if (!create)
      stop('directory already exists and create == F')
    if (!is_repo_path(path, quiet = T))
      stop(path, ' is not a repo')
  }
  else {
    dir.create(path, showWarnings = F, recursive = T)
  }
  
  repo <- structure(list(path = path), class = 'repo')
  create_collection(repo, 'default')
  
  repo
}

#' @export
is_repo <- function (x) {
  inherits(x, 'repo')
}

#' @export
is_repo_path <- function (path, quiet = F) {
  see(file.exists(path),
      file.info(path)$isdir,
      quiet = quiet)
}

#' @export
#' @importFrom lazyeval lazy_dots
with_tags <- function (object, ...) {
  with_tags_(object, .dots = lazy_dots(...))
}

#' @export
#' @importFrom lazyeval all_dots lazy_eval
#' @importFrom assertthat has_name
with_tags_ <- function (object, ..., .dots) {
  dots <- all_dots(.dots, ..., all_named = TRUE) # TODO maybe tags with no names?
  tags <- lazy_eval(dots, list(. = object))
  if (has_name(tags, 'id')) stop('cannot overwrite object id', call. = F)
  attributes(object) <- c(attributes(object), tags)
  object
}


#' @export
has_object <- function (...) UseMethod('has_object')

#' @export
has_object.repo <- function (repo, object, col_name = 'default') {
  if (!has_collection(repo, col_name)) return(F)
  has_object(as_collection(repo, col_name), object)
}

#' @importFrom digest digest
has_object.collection <- function (collection, object) {
  object <- with_id(object)
  cp <- digest(collection$collection, 'md5')
  file.exists(file.path(collection$path, cp, id_name(object)))
}


#' @export
select <- function (.where, ...) {
  select_(.where, .dots = lazy_dots(...))
}

#' @export
select_ <- function (.where, ..., .dots) UseMethod('select_')

#' @importFrom lazyeval all_dots lazy_eval
select_.repo <- function(repo, ..., .dots) {
  dots <- all_dots(.dots, ..., named = T)
  dots <- lazy_eval(dots)
}

select_.collection <- function(collection, ..., .dots) {
  
}

