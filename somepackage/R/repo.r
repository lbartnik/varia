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
  
  structure(list(path = path), class = 'repo')
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
#' @importFrom digest digest
create_collection <- function (repo, name) {
  stopifnot(is_repo(repo))
  h <- digest(name, 'md5')
  p <- file.path(repo$path, h)
  if (file.exists(p)) warning('collection already exists')
  else {
    dir.create(p)
    saveRDS(name, paste0(p, '.rds'))
  }
  as_collection(repo, name)
}

#' @export
as_collection <- function (repo, col_name = 'default') {
  stopifnot(is_repo(repo))
  if (is_collection(repo)) repo$collection <- col_name
  else {
    class(repo) <- c('collection', class(repo))
    repo$collection <- name  
  }
  repo
}

#' @export
is_collection <- function (x) {
  inherits(x, 'collection')
}


#' @export
add_object <- function (...) UseMethod('add_object')

#' @export
add_object.repo <- function (repo, object, col_name = 'default') {
  if (!has_collection(repo, col_name))
    cl <- create_collection(repo, col_name)
  else
    cl <- as_collection(repo, col_name)
  add_object.collection(cl, object)
}

#' @export
#' @importFrom digest digest
add_object.collection <- function (collection, object) {
  id <- digest(object, 'md5')
  if (has_object(collection, object)) {
    warning('object already exists', call. = F)
    return(id)
  }
  cp <- digest(collection$collection, 'md5')
  on <- paste0(id, '.rds')
  saveRDS(object, file.path(collection$path, cp, on))
}


#' @export
has_object <- function (...) UseMethod('has_object')

#' @export
has_object.repo <- function (repo, object, col_name = 'default') {
  if (!has_collection(repo, col_name)) return(F)
  has_object(as_collection(repo, col_name), object)
}

#' @export
#' @importFrom digest digest
has_object.collection <- function (collection, object) {
  cp <- digest(collection$collection, 'md5')
  on <- paste0(digest(object, 'md5'), '.rds')
  file.exists(file.path(collection$path, cp, on))
}

