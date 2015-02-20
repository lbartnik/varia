# --- adding tags to object --------------------------------------------

#' Add tags as an attribute.
#' 
#' When returning object in \code{\link{to_collection}} you can add tags
#' to be written to the destination collection.
#' 
#' \code{no_tags} removes the tags added with \code{with_tags}.
#' 
#' \code{has_tags} informs whether an object has tags assigned with
#' \code{with_tags}
#'
#'
#' @param x Object to assign tags to.
#' @param ... Named expressions producing single-element, atomic values;
#'        they have precedence before \code{.tags}.
#' @param .tags Already computed tags.
#' @return Object \code{x} with tags added as an attribute.
#'
#'
#' @export
#' @importFrom lazyeval lazy_dots
#' 
#' @examples
#' \dontrun{
#'   task <- cply(src, function(obj, tags)with_tags(obj, .tags = tags))
#'   to_collection(task, dest)
#' }
with_tags <- function (x, ..., .tags) {
  dots <- lazy_dots(...)
  with_tags_(x, .dots = dots, .tags = .tags)
}


#' @importFrom plyr defaults
#' @rdname with_tags
#' @export
with_tags_ <- function (x, .dots, .tags) {
  .tags <- if (!missing(.tags)) assert_tags(.tags) else list()
  new_tags <- eval_tags(.dots, x)
  
  attr(x, 'tags') <- defaults(new_tags, .tags)
  x
}

#' @rdname with_tags
#' @export
no_tags <- function (x) {
  if (has_tags(x)) attr(x, 'tags') <- NULL
  x
}

#' @rdname with_tags
#' @export
#' @importFrom assertthat has_attr
has_tags <- function (x) has_attr(x, 'tags')


# --- eval, read, check ------------------------------------------------

RESERVED_TAGS <- c('.date', '.id', 'class')


#' Combine all possible and provided tags.
#' 
#' The priority of tags is as follows:
#' \itemize{
#'   \item tags evaluated from \code{dots} (see \code{\link[lazyeval]{lazy_eval}})
#'   \item standard per-class tags (see \code{\link{auto_tags}}),
#'   \item explicit tags provided in \code{tags}
#' }
#' 
#' Tags from the object itself (see \code{\link{with_tags}}) are not
#' processed in this function; if present, a warning message is printed.
#' 
#' @param obj Object.
#' @param dots A \code{\link[lazyeval]{lazy_dots}} object.
#' @param tags A \code{list} of tags.
#' @param .on_error \code{\link{stop}}, \code{\link{warning}}, or \code{NULL}
#' 
#' @return A \code{list} of evaluated tags.
#' 
#' @importFrom plyr defaults
all_tags <- function (obj, dots, tags, .on_error = NULL) {
  # error message handling
  on_error <- if (is.null(.on_error)) function(...)NULL else function(...) .on_error(..., call. = FALSE)
  
  check_reserved_tags <- function (tags) {
    name <- deparse(substitute(tags))
    indx <- RESERVED_TAGS %in% names(tags)
    if (any(indx)) on_error(name, ': tag names:', paste(stdn[indx], sep = ', '))
  }
  
  # with_tags
  if (has_tags(obj)) on_error('with_tags is not supported; pass values via tags params')
  
  # lazy tags
  dots <- if (!missing(dots)) eval_tags(dots, obj) else list()
  if (!check_tags(dots)) on_error('dots have to evaluate to a named list of atomic values')
  check_reserved_tags(dots)

  # add auto tags & .date
  new_tags <- defaults(dots, auto_tags(obj))
  new_tags$.date <- Sys.time()
  
  # list tags
  if (!missing(tags)) {
    if (!check_tags(tags)) on_error('tags have to be a named list of atomic values')
    check_reserved_tags(tags)
    new_tags <- defaults(new_tags, tags)
  }
  
  # return a complete list
  new_tags
}

# tags must be a named list of atomic values
check_tags <- function (tags) {
  if (!length(tags)) return(TRUE)
  if (!is.list(tags)) return(FALSE)
  if (!all(nchar(names(tags)))) return(FALSE)
  if (!all(vapply(tags, is.atomic, logical(1)))) return(FALSE)
  T
}


#' @importFrom lazyeval lazy_eval
eval_tags <- function (dots, obj) {
  data <- list(. = obj)
  if (is.list(obj))
    data <- c(data, obj)
  lazy_eval(dots, data)
}


# reads tags, adds the `.id` tag
read_tags <- function (path) {
  tags <- readRDS(paste0(path, '_tags.rds'))
  tags$`.id` <- basename(path)
  tags
}


# --- various helpers --------------------------------------------------

extract_tags <- function (tags, ...) {
  
}


# --- re-computing tags ------------------------------------------------

#' Re-compute tags.
#' 
#' @param col collection object
#' @param ... Named tag expressions
#' @param .add If \code{TRUE} will merge old and new tags.
#' 
#' @export
#' @importFrom lazyeval lazy_dots
#' @importFrom plyr l_ply defaults
#' @importFrom tools file_path_sans_ext
retag <- function (col, ..., .add = T, .cores = getOption('cores', 1)) {
  stopifnot(is_collection(col))
  
  dots <- lazy_dots(...)
  
  # process all tag files 
  files <- file.path(path(col), make_path(col))
  res <-
    run_in_parallel(files, c_apply, function (obj, old_tags) {
      # split reserved
      rsvd <- old_tags[RESERVED_TAGS]
      # if .add keep old_tags
      if (.add) old_tags[RESERVED_TAGS] <- NULL else old_tags <- NULL
      
      # recompute, restore date
      tags <- all_tags(obj, dots, old_tags)
      tags$.date <- rsvd$.date
      
      # save, return T for success
      saveRDS(tags, file.path(path(col), paste0(make_path(rsvd$.id), '_new_tags.rds')))
      TRUE
    }, .cores)
  
  # separate OKs from errors
  res <- as_ply_result(res)
  
  # these files should exist now
  paths    <- file.path(path(col), make_path(col))
  new_tags <- paste0(paths, '_new_tags.rds')

  # if there were errors or some do not exist, remove all new tag files
  # and leave the collection unchanged
  if (!all(vapply(res$res, isTRUE, logical(1))) || !all(file.exists(new_tags))) {
    unlink(new_tags[file.exists(new_tags)], force = T)
    return(structure(character(), path = path(col), errors = res$err,
                     class = c('ply_result', 'collection')))
  }
  else {
    file.rename(new_tags, paste0(paths, '_tags.rds'))
    return(structure(names(res$res), path = path(col), errors = res$err,
              class = c('ply_result', 'collection')))
  }
}
