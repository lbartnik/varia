#' @export
sourcerer <- function (path)
{
  stopifnot(file.exists(path))
  structure(list(path = path), class = 'sourcerer')
  # structure(list(files = list.files(path)), class = 'sourcerer')
  # TODO: make sure all files in the directory contain data frames
  #       that can be passed to dplyr functions
  # TODO: read objects from archivist - need a way to speficy a
  #       collection of objects
}


#' @importFrom dplyr do
#' @export
do.sourcerer <- function (.data, ...)
{
  # 1. if one file, data set - run the regular data.frame version
  #    of do
  # 2. if more than one file - run in parallel; read the file
  #    contents, run regular do on each data.frame
  real_data <- readRDS(.data$path)

  # mock parent.frame because there is no env param in do.data.frame
  parent_env <- parent.frame()

  orig.pf <- parent.frame
  b <- asNamespace('base')
  
  unlockBinding(as.symbol('parent.frame'), b)
  b$parent.frame <- function(...) parent_env
  lockBinding(as.symbol('parent.frame'), b)
  
  ret <- do(real_data, ...)
  
  unlockBinding(as.symbol('parent.frame'), b)
  b$parent.frame <- orig.pf
  lockBinding(as.symbol('parent.frame'), b)
  
  ret
}

