
# simple helpers
is_dir <- function (path) (file.exists(path) && file.info(path)$isdir)

#' @importFrom digest digest
md5 <- function (x) digest(x, 'md5')

#' @importFrom digest digest
hash32 <- function (x) digest(x, 'xxhash32')


#' Constructor for a repository.
repository <- function (path) {
  stopifnot(file.exists(path) && is_dir(path))
  structure(list(path = path), class = 'repository')
}

is_repository <- function (x) inherits(x, 'repository')


#' Constructor for a collection.
collection <- function (repository, name) {
  stopifnot(is_repository(repository))
  path <- file.path(repository$path, md5(name))
  structure(list(path = path), class = 'collection')
}

is_collection <- function (x) inherits(x, 'collection')


#' Selects objects.
#' 
#' Finds all objects whose tags match the given criteria.
#' 
#' @return \code{character} object IDs.
#' 
#' @importFrom lazyeval lazy_dots
#' @importFrom dplyr filter select %>%
#' @importFrom plyr ldply
#' @importFrom magrittr extract2
select <- function (collection, ...) {
  stopifnot(is_collection(collection))
  
  dots <- lazy_dots(...)
  ids <-
    list.files(collection$path, '[a-z0-9]+_tags.rds', recursive = T, full.names = T) %>%
    ldply(function (path) {
      c(path, eval_tags(dots, readRDS(path)))
    }) %>%
    filter(V2 == T) %>%
    extract2('V1') %>%
    basename %>%
    {gsub('_tags.rds', '', .)}
  
  structure(list(id = ids), class = 'selection')
}

is_selection <- function (x) inherits(x, 'selection')


#' @importFrom lazyeval lazy_eval
#' @importFrom dplyr %>%
eval_tags <- function (dots, data) {
  lazy_eval(dots, data) %>%
    unlist %>%
    as.logical %>%
    all
}


run <- function (s, f) {
  stopifnot(is_selection(s))
  stopifnot(is.function(f))
}




