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
create_collection <- function (repo, name) {
  stopifnot(is_repo(repo))
  cid <- compute_id(name)
  p <- file.path(repo$path, cid)
  if (file.exists(p)) warning('collection already exists')
  else {
    dir.create(p)
    saveRDS(name, paste0(p, '.rds'))
  }
  as_collection(repo, name)
}

#' @export
as_collection <- function (repo, col_name) {
  stopifnot(is_repo(repo))
  if (is_collection(repo)) repo$collection <- col_name
  else {
    if (!has_collection(repo, col_name))
      stop('collection ', col_name, ' does not exist', call. = F)
    class(repo) <- c('collection', class(repo))
    repo$collection <- name  
  }
  repo
}

#' @export
in_collection <- as_collection

#' @export
in_default_collection <- function (repo) in_collection(repo, 'default')

#' @export
is_collection <- function (x) inherits(x, 'collection')



#' @importFrom digest digest
compute_id <- function (x) digest(x, 'md5')

#' @importFrom assertthat has_attr

with_id <- function (object) {
  if (has_attr(object 'id')) return(object)
  attr(object, 'id') <- compute_id(object)
  object
}

#' @importFrom assertthat has_attr
id <- function (x) {
  stopifnot(has_attr(x, 'id'))
  attr(x, 'id')
}

id_name <- function (x) {
  paste0(id(x), '.rds')
}


#' @export
add_object <- function (..., .dots) UseMethod('add_object')

#' @importFrom lazyeval all_dots
add_object.repo <- function (repo, object, ..., .dots) {
  if (!has_collection(repo, col_name))
    cl <- create_collection(repo, col_name)
  else
    cl <- as_collection(repo, col_name)
  
  add_object_.collection(cl, object, ..., .dots = dots)
}

#' @importFrom digest digest
add_object.collection <- function (collection, object, ..., .dots) {
  object <- with_id(object)
  if (has_object(collection, object)) {
    warning('object already exists', call. = F)
    return(id(object))
  }
  
  object <- with_tags_(object, ..., .dots = dots)
  cid <- compute_id(collection$collection)
  saveRDS(object, file.path(collection$path, cid, id_name(object)))
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

#'
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

