is_dir <- function (x) file.exists(x) && file.info(x)$isdir

#' @importFrom dplyr %>%
list_ids <- function (path) {
  list.files(path, '^[a-z0-9]+.rds$', recursive = T, full.names = F) %>%
    basename %>%
    setdiff('comment.rds') %>%
    {gsub('.rds$', '', .)}
}

list_files <- function (col, sfx) {
  if (!length(col)) return(character())
  paths <- file.path(path(col), paste0(make_path(col), sfx))
  normalizePath(paths)
}

tag_files <- function (col) list_files(col, '_tags.rds')
obj_files <- function (col) list_files(col, '.rds')


make_path <- function (id) {
  stopifnot(all(nchar(id) >= 4))
  file.path(substr(id, 1, 2), substr(id, 3, 4), id)
}

short_id <- function (id) {
  paste0(
    substr(as.character(id), 1, 4),
    substr(as.character(id), 29, 32)
  )
}

long_id <- function (col, ids, .first = TRUE) {
  col <- as.character(col)
  x   <- substr(ids, 1, 4)
  if (.first) {
    y <- substr(col, 1, 4)
    col[match(x, y)]
  }
  else {
    r <- lapply(paste0('^', x, '.*', substr(ids, 5, 8), '$'), grep, x = col, value = T)
    r[vapply(r, length, integer(1)) == 0] <- NA_character_
    r
  }
}

