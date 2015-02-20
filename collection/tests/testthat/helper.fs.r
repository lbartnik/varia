# --- utils ------------------------------------------------------------

remove_dir <- function (path) {
  stopifnot(file.exists(path)) # make sure there is no error in the test
  unlink(path, T, T)
}

# --- collection -------------------------------------------------------

create_empty_col <- function () {
  path <- file.path(tempdir(), as.character(round(runif(1)*1e6)))
  expect_false(file.exists(path))
  dir.create(path)
  path
}

copy_sample_col <- function () {
  path <- create_empty_col()
  file.copy(list.files('sample-collection', full.names = T), path, recursive = T)
  path
}

read_tag_files <- function (path) {
  lapply(list.files(path, '_tags.rds$', full.names = T, recursive = T), readRDS)
}

read_obj_files <- function (path) {
  lapply(list.files(path, '^[^_]+.rds', full.names = T, recursive = T), readRDS)
}

# --- repository -------------------------------------------------------

create_empty_repo <- function () {
  path <- file.path(tempdir(), as.character(round(runif(1)*1e6)))
  expect_false(file.exists(path))
  dir.create(path)
  repository(path)
}

copy_sample_repo <- function () {
  path <- file.path(tempdir(), as.character(round(runif(1)*1e6)))
  expect_false(file.exists(path))
  dir.create(path)
  file.copy(list.files('sample-repository', full.names = T), path, recursive = T)
  repository(path)
}
