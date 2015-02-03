#' @export
#' @importFrom lazyeval lazy_dots
tags <- function (col, ...) {
  stopifnot(is_collection(col))
  
  dots <- lazy_dots(...)
  if (length(dots)) {
    # from dplyr/R/select-vars.r
    is_name <- vapply(dots, function(x) is.name(x$expr), logical(1))
    if (!all(is_name)) {
      stop("Arguments must be unquoted variable names. ",
           "Arguments ", paste0(names(args)[!is_name], collapse =", "), " are not.",
           call. = FALSE
      )
    }
  }
  
  tags_(col, dots)
}


#' Show tags present in a collection.
#' 
#' @export
#' @importFrom plyr ldply llply
#' @importFrom dplyr as_data_frame
#' @importFrom lazyeval all_dots
#' 
#' @rdname tags
tags_ <- function (col, .dots) {
  stopifnot(is_collection(col))
  
  dots <- all_dots(.dots)
  nms <- vapply(dots, function(x) as.character(x$expr), character(1))
  
  # read tags
  tgs <-
    tag_files(col) %>%
    ldply(function(x) readRDS(x) %>% as_data_frame) %>%
    llply(function(x) x[!is.na(x)])
  
  if (!length(tgs)) {
    warning('no tags found', call. = F)
    return(invisible(NULL))
  }
  
  if (length(nms)) {
    if(!all(nms %in% names(tgs)))
      stop('tags not found: ', paste(setdiff(nms, names(tgs)), sep = ', '),
           call. = FALSE)
    tgs <- tgs[nms]
  }
  
  structure(tgs, class = 'tags')
}


#' @export
#' @importFrom plyr laply
#' @importFrom dplyr %>%
print.tags <- function (x) {
  # format names
  nms <-
    paste0(names(x), '(', laply(x, length), ')') %>%
    {format(., width = max(nchar(.)), justify = 'left')}
  names(x) <- nms

  # print
  width <- getOption('width') - nchar(nms[1]) - 4
  for (n in names(x)) {
    vp <-
      x[[n]] %>%
      unique %>%
      paste(collapse = ', ')
    if (nchar(vp) > width)
      vp <- paste0(substr(vp, 1, width-3), '...')
    cat(n, ':', vp, '\n')
  }
}


#' @export
#' @importFrom lazyeval lazy_dots
#' @importFrom plyr l_ply
#' @importFrom tools file_path_sans_ext
retag <- function (col, ...) {
  stopifnot(is_collection(col))
  
  dots <- lazy_dots(...)
  obj_files(col) %>%
    l_ply(function(path) {
      tags <- eval_tags(dots, readRDS(path))
      path <- paste0(file_path_sans_ext(path), '_tags.rds')
      saveRDS(tags, path)
    })
}
