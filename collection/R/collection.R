#' @export
collection <- function (x, ...) UseMethod('collection')


#' @export
collection.default <- function (x, ...) {
  stop('cannot read/create collection from ', class(x)[1], call. = FALSE)
}


# TODO add .create parameter
#' @export
collection.character <- function (path) {
  stopifnot(is_dir(path))
  
  col <- structure(list_ids(path), path = path, class = 'collection')
  obj <- obj_files(col)
  tgs <- tag_files(col)
  
  if (length(obj)) stopifnot(all(file.exists(obj)))
  if (length(tgs)) stopifnot(all(file.exists(tgs)))
  
  comment <- file.path(path, 'comment.rds')
  if (!file.exists(comment)) comment <- NULL
  
  files <- list.files(path, recursive = T, full.names = T)
  stopifnot(setequal(files, c(obj, tgs, comment)))
  
  col
}


#' @export
create_collection <- function (path, comment) {
  stopifnot(!file.exists(path))         # dir does not exist
  stopifnot(file.exists(dirname(path))) # up-dir must exist
  
  dir.create(path)
  
  # if cared to comment
  if (!missing(comment))
    saveRDS(as.character(comment), file.path(path, 'comment.rds'))
  
  structure(character(), path = path, class = 'collection')
}


from_collection <- function (ids, col) {
  structure(ids, class = 'collection', path = path(col))
}

is_collection <- function (x) inherits(x, 'collection')


#' @export
#' @importFrom plyr laply
print.collection <- function (col, ...) {
  # if created with a comment
  comment_path <- file.path(path(col), 'comment.rds')
  comment <- if (file.exists(comment_path)) readRDS(comment_path)
  if (!is.null(comment) && nchar(comment)) comment <- paste0('(comment: ', comment, ')')
  
  # if there are no objects yet
  if (!length(col)) {
    return(cat('empty collection', comment))
  }

  sizes <- laply(obj_files(col), function(path) file.info(path)$size)
  size  <- structure(sum(sizes), class = 'object_size')
  cat(length(sizes), 'object(s),', format(size, 'auto'), comment)
}


#' @export
list_objects <- function (col, .n = 10) {
  stopifnot(is_collection(col))
  # TODO
  stop('not implemented yet')
}





#' @export
#' @importFrom dplyr data_frame filter rename filter_ %>%
#' @importFrom plyr mdply
#' @importFrom magrittr extract2
#' @importFrom lazyeval lazy_eval
filter_.collection <- function (col, .dots) {
  stopifnot(is_collection(col))
  
  tags <-
    data_frame(id = as.character(col), path = tag_files(col)) %>%
    mdply(function (id, path) {
      data <- readRDS(path)
      lazy_eval(.dots, data) %>%
        as.logical %>%
        all
    }) %>%
    rename(match = V1)
  
  ids <-
    tags %>%
    filter(match == T) %>%
    extract2('id')
  
  from_collection(ids, col)
}


#' @export
`[.collection` <- function (col, i) {
  from_collection(as.character(col)[i], col)
}


# TODO rename to add_object
#' Add object to collection
#' 
#' @return Single-element collection containing the newly added object.
#' 
#' @export
#' 
#' @importFrom lazyeval lazy_dots
add <- function (col, obj, ..., .force = F) {
  dots <- lazy_dots(...)
  add_(col, obj, .dots = dots, .force = .force)
}


# TODO rename to add_object
#' @export
add_ <- function (col, obj, .dots, .force = F) {
  # TODO if called directly from globalenv print message to refresh collection object
  stopifnot(is_collection(col))
  
  id   <- md5(obj)
  file <- make_path(id)
  file <- file.path(path(col), file)
  
  if (file.exists(paste0(file, '.rds'))) {
    if (!.force) stop('object already exists', call. = F)
    warning('object already exists, forced to overwrite', call. = F)
  }
  else
    dir.create(dirname(file), recursive = T, showWarnings = F)
  
  tags <- eval_tags(.dots, obj)
  
  saveRDS(obj, paste0(file, '.rds'))
  saveRDS(tags, paste0(file, '_tags.rds'))
  
  from_collection(id, col)
}
