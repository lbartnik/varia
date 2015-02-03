is_dir <- function (x) file.exists(x) && file.info(x)$isdir

#' @importFrom dplyr %>%
list_ids <- function (path) {
  list.files(path, '^[a-z0-9]+.rds$', recursive = T, full.names = F) %>%
    basename %>%
{gsub('.rds$', '', .)}
}

list_files <- function (col, sfx) {
  if (!length(col)) return(character())
  file.path(path(col), paste0(make_path(col), sfx))
}

tag_files <- function (col) list_files(col, '_tags.rds')
obj_files <- function (col) list_files(col, '.rds')


make_path <- function (id) {
  stopifnot(all(nchar(id) >= 4))
  file.path(substr(id, 1, 2), substr(id, 3, 4), id)
}
