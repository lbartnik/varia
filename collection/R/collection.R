
#' @export
create_collection <- function (path) {
  stopifnot(!file.exists(path))         # dir does not exist
  stopifnot(file.exists(dirname(path))) # up-dir must exist
  
  dir.create(path)
  structure(character(), path = path, class = 'collection')
}

#' @export
collection <- function (path) {
  stopifnot(is_dir(path))
  
  col <- structure(list_ids(path), path = path, class = 'collection')
  obj <- obj_files(col)
  tgs <- tag_files(col)

  stopifnot(all(file.exists(obj)))
  stopifnot(all(file.exists(tgs)))
  
  files <- list.files(path, recursive = T, full.names = T)
  stopifnot(setequal(files, c(obj, tgs)))

  col
}


from_collection <- function (ids, col) {
  structure(ids, class = 'collection', path = path(col))
}

is_collection <- function (x) inherits(x, 'collection')

#' @export
#' @importFrom plyr laply
print.collection <- function (col, ...) {
  if (!length(col)) {
    cat('empty collection')
    return(invisible())
  }
  
  sizes <- laply(obj_files(col), function(path) file.info(path)$size)
  size  <- structure(sum(sizes), class = 'object_size')
  cat('collection:', length(sizes), 'object(s),', format(size, 'auto'))
}


#' @export
#' @importFrom dplyr data_frame filter rename select %>%
#' @importFrom plyr mdply
#' @importFrom magrittr extract2
select_.collection <- function (col, .dots) {
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


#' @export
#' 
#' @importFrom lazyeval lazy_dots
add <- function (col, obj, ..., force = F) {
  stopifnot(is_collection(col))
  
  id   <- md5(obj)
  file <- make_path(id)
  file <- file.path(path(col), file)
  
  if (file.exists(paste0(file, '.rds'))) {
    if (!force) stop('object already exists', call. = F)
    warning('object already exists, forced to overwrite', call. = F)
  }
  else
    dir.create(dirname(file), recursive = T, showWarnings = F)
  
  dots <- lazy_dots(...)
  tags <- eval_tags(dots, obj)
  
  saveRDS(obj, paste0(file, '.rds'))
  saveRDS(tags, paste0(file, '_tags.rds'))
  
  from_collection(c(col, id), col)
}


#' @importFrom lazyeval lazy_eval
eval_tags <- function (dots, obj) {
  data <- list(. = obj)
  if (is.list(obj))
    data <- c(data, obj)
  
  lazy_eval(dots, data)
}
