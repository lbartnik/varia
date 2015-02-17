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

#' @importFrom lazyeval lazy_eval
eval_tags <- function (dots, obj) {
  data <- list(. = obj)
  if (is.list(obj))
    data <- c(data, obj)
  
  tags <- lazy_eval(dots, data)
  assert_tags(tags, 'all tags must be named and evaluate to single-element, atomic values')
}

# reads tags, adds the `.id` tag
read_tags <- function (path) {
  tags <- readRDS(paste0(path, '_tags.rds'))
  tags$`.id` <- basename(path)
  tags
}

# tags must be a named list of single-element, atomic values
check_tags <- function (tags) {
  if (!length(tags)) return(TRUE)
  if (!all(nchar(names(tags)))) return(FALSE)
  if (!all(vapply(tags, is.atomic, logical(1)))) return(FALSE)
  if (any(vapply(tags, length, numeric(1)) != 1)) return(FALSE)
  T
}

check_standard_tags <- function (tags) {
  name <- deparse(substitute(tags))
  stdn <- c('.id', '.date')
  indx <- stdn %in% names(tags)
  if (any(indx)) {
    stop("tag names reserved in '", name, "': ",
         paste(stdn[indx], sep = ', '), call. = FALSE)
  }
}

add_standard_tags <- function (tags) {
  tags[['.date']] <- Sys.time()
  tags
}

# If tags are incorrect, stops execution.
# If tags are OK, returns them unchanged.
#
# The default message is used in a few places.
assert_tags <- function (tags, message = 'tags have to be a named list with single-element, atomic values')
{
  if (!check_tags(tags)) stop(message, call. = FALSE)
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
  stopifnot(all(nchar(names(dots)))) # all must have names
  
  # process all tag files 
  files <- file.path(path(col), make_path(col))
  res <- run_in_parallel(files, c_apply, function (obj, old_tags) {
    id <- old_tags$.id; old_tags$.id <- NULL
    tags <- eval_tags(dots, obj)
    if (.add)
      tags <- defaults(tags, old_tags)
    saveRDS(tags, file.path(path(col), paste0(make_path(id), '_new_tags.rds')))
    T
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


# --- auto tags --------------------------------------------------------

#' @export
auto_tags <- function (x) UseMethod('auto_tags')

#' @export
auto_tags.default <- function (x) {
  stop('class ', paste(class(x), collapse = ' '), ' is not supported',
       call. = FALSE)
}

# TODO what about multiple-element tag values?

#' @export
# auto_tags.lm <- function (x) {
#   list(
#     `class` = class(x),
#     `coef`  = x$coefficients,
#     `
#   )
# }

