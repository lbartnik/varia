# --- collection: constructors -----------------------------------------

#' Read or create a new collection.
#' 
#' @param path Path to a directory with collection.
#' @param comment Used only if \code{.create} is set to \code{TRUE}.
#' @param .create Create a new collection if the directory does not exist.
#' 
#' @return A collection object.
#' 
#' @export
collection <- function (x, comment, ..., .create = F) UseMethod('collection')


#' @rdname collection
#' @export
collection.default <- function (x, ...) {
  stop('cannot read/create collection from ', class(x)[1], call. = FALSE)
}


#' @rdname collection
#' @export
collection.character <- function (path, comment, .create = F) {
  # create a collection
  if (!file.exists(path)) {
    if (!.create) stop('directory does not exist and .create is FALSE', call. = FALSE)
    return(create_collection(path, comment))
  }
  
  # read from an existing collection
  col <- structure(list_ids(path), path = path, class = 'collection')
  verify_files(col, FALSE)
  
  col
}

# creates a new collection
create_collection <- function (path, comment) {
  stopifnot(!file.exists(path))         # dir does not exist
  stopifnot(file.exists(dirname(path))) # up-dir must exist
  
  dir.create(path)
  
  # if cared to comment
  if (!missing(comment))
    saveRDS(as.character(comment), file.path(path, 'comment.rds'))
  
  structure(character(), path = path, class = 'collection')
}


#' Read all objects from directory pointed to in \code{col}.
#' 
#' @param col Collection object.
#' @return A new collection object with all identifiers.
#' 
#' @export
refresh <- function (col) {
  stopifnot(is_collection(col))
  collection(path(col))
}

# --- collection: helpers ----------------------------------------------

is_collection <- function (x) inherits(x, 'collection')

# return either name or the last two directories of the path
collection_name <- function (col) {
  p <- normalizePath(path(col))
  if (file.exists(fp <- paste0(p, '.rds'))) # if in a repository
    return(readRDS(fp))
  path_to_name(p, 2)
}

from_collection <- function (ids, col) {
  # TODO 'grouped' attr is not copied; shout it be?
  structure(ids, class = 'collection', path = path(col))
}

#' verify that tag and object files exist
#' 
#' If \code{subcol} is set to \code{TRUE} then do not assert that no
#' other RDS files exist.
#' 
#' @param col Collection object.
#' @param subcol Is that a subset of a bigger colletion?
verify_files <- function (col, subcol) {
  stopifnot(is_collection(col))
  
  obj <- obj_files(col)
  tgs <- tag_files(col)
  
  if (length(obj)) stopifnot(all(file.exists(obj)))
  if (length(tgs)) stopifnot(all(file.exists(tgs)))

  # if a sub-collection, this is all we can check
  if (subcol) return(T)

  comment <- file.path(path(col), 'comment.rds')
  if (!file.exists(comment)) comment <- NULL
  
  files <- list.files(path(col), recursive = T, full.names = T)
  stopifnot(setequal(normalizePath(files), normalizePath(c(obj, tgs, comment))))
  
  T
}


# --- collection: operators --------------------------------------------

#' Operators for \emph{collection} class.
#' 
#' @name collections_ops
#' @export
`[.collection` <- function (col, i) {
  # empty result
  if (!length(i))
    return(from_collection(character(0), col))
  
  # numeric ids
  if (is.numeric(i))
    return(from_collection(as.character(col)[i], col))
  
  # short or long character ids
  if (is.character(i)) {
    l <- nchar(i)
    if (!all(l == 32) && !all(l == 8)) {
      stop('only short (8 characters) or long (32) identifiers allowed',
           call. = FALSE)
    }
    # determine actual ids
    if (l[1] == 8) {
      ids <- long_id(col, i, .first = FALSE) # find all
      ina <- vapply(ids, is.na, logical(1))
      inf <- i[ina]
      ids <- unlist(ids[!ina])
    }
    else {
      ids <- match(i, unclass(col))
      ina <- is.na(ids)
      inf <- i[ina]
      ids <- i[!ina]
    }
    # report missing ids
    if (length(inf)) {
      warning('identifiers not found: ', paste(inf, collapse = ', '),  call. = FALSE)
    }
    
    return(from_collection(ids, col))
  }
  
  # other id types
  stop('do not how to handle index of class ', class(i)[1], call. = FALSE)
}


#' @rdname collections_ops
#' @export
`.DollarNames.collection` <- function (x, pattern) {
  if (length(x) == 0) return(character(0))
  grep(pattern, names(summary(x, .all = TRUE)$tags), value = TRUE)
}

#' @rdname collections_ops
#' @export
`$.collection` <- function (x, name) {
  if (length(x) == 0) {
    stop('collection empty', call. = FALSE)
  }
  
  vls <- locally(tply(x, function(tags) tags[[name]] ))
  inl <- vapply(vls, is.null, logical(1))
  if (all(inl))
    stop(name, ' tag name is not present', call. = FALSE)
  
  `names<-`(unlist(vls[!inl], recursive = FALSE), short_id(as.character(x)[!inl]))
}



# --- collection: summary & printing -----------------------------------

#' Summarise objects in a collection.
#' 
#' @param col Collection object.
#' @param .all Show all tags, also beginngin with a dot `.`
#' 
#' @export
#' @importFrom dplyr as_data_frame
#' @importFrom assertthat has_attr
summary.collection <- function (col, .all = FALSE) {
  # if created with a comment
  comment_path <- file.path(path(col), 'comment.rds')
  comment <- if (file.exists(comment_path)) readRDS(comment_path) else ''
  
  # TODO handle removed objects
  
  # sizes
  sizes <- file.info(obj_files(col))
  sizes <- `names<-`(sizes$size, as.character(col))
  
  # tags
  tags <- lapply(tag_files(col), readRDS)
  tnms <- unique(unlist(lapply(tags, names)))
  tags <- lapply(tnms, function(name) {
    # preserve object class
    # TODO what if the classes are different? maybe return a list
    #      of values instead?
    v <- lapply(tags, `[[`, i = name)
    c <- lapply(v, class)
    c <- c[!vapply(c, is.null, logical(1))]
    v <- unlist(v, use.names = F, recursive = F)
    `class<-`(v, c[[1]])
  })
  names(tags) <- tnms
  
  # remove hidden tags
  if (!.all) {
    R <- grep('^\\.', names(tags))
    if (length(R)) tags <- tags[-R]
  }
  
  # grouping
  groups <- if (has_attr(col, 'grouped')) attr(col, 'grouped') else list()

  # return the summary object
  structure(list(path = path(col), comment = comment, sizes = sizes,
                 tags = tags, groups = groups),
            class = 'summary.collection')
}


#' @rdname summary.collection
#' @export
#' @importFrom plyr laply
print.summary.collection <- function (x, ...) {
  comment <- if (nchar(x$comment)) paste0('(comment: ', x$comment, ')')
  
  # sizes
  if (!length(x$sizes)) {
    cat('empty collection', comment)
  }
  else {
    cat(length(x$sizes), 'object(s),', format_size(sum(x$sizes)), comment)
  }
  
  # tags
  if (length(x <- x$tags)) {
    nms <-
      paste0(names(x), '(', laply(x, length), ')') %>%
      {format(., width = max(nchar(.)), justify = 'left')}
    names(x) <- nms
  
    width <- getOption('width') - nchar(nms[1]) - 4
    for (n in names(x)) {
      cat('\n', n, ':', toString(unique(x[[n]]), width = width))
    }
  }
  
  # grouping
  if (length(x$groups)) {
    vars <- names(attr(x$groups, 'labels'))
    cat('\ngrouped by tags:', paste(vars, collapse = ', '))
  }
}


#' Prints collection.
#' 
#' @export
print.collection <- function (x, ..., n = 10, width = getOption('width', 72), .full = FALSE) {
  cat('Collection: ', collection_name(x))
  
  if (!length(x)) return(cat(' [empty]\n'))
  
  cat(' [', length(x), ']\n\n', sep = '')
  
  if (length(x) > 1)
    list_simple(x, n, width, .full)
  else
    list_wide(x, width)
}


#' @rdname print.collection
#' @export
full <- function (col) {
  print(col, .full = TRUE)
}



#' List collection contents; simple format.
#' 
#' @param col Collection object.
#' @param n Print this many first objects.
#' @param width Line width.
#' @param .all Print short id, size and date.
#' 
#' @importFrom lubridate origin
list_simple <- function (col, n = 10, width = getOption('width', 72), .full = FALSE) {
  # TODO print .id and .size and .date only if requested
  # TODO first use a specified method for printing, if that does not exist, print tags
  
  n <- min(n, length(col))
  ncol <- col[seq(n)]
  
  # 
  if (!all(file.exists(obj_files(ncol)))) {
    stop('object files do not exist, maybe removed? refresh collection',
         call. = FALSE)
  }
  
  # read tags
  tags <- lapply(tag_files(ncol), readRDS)
  
  # prefix
  nms <- c('short id', short_id(as.character(ncol)))
  szs <- c('size', vapply(file.info(obj_files(ncol))$size, format_size, character(1)))
  cls <- c('class', vapply(tags, function(t) {
    if (is.null(t$class)) return('unknown')
    print_class(`class<-`(list(), t$class), t)
  }, character(1)))
  dts <- c('insert time', vapply(tags, function(t) {
    if (is.null(t$.date)) return('unknown')
    as.character(t$.date)
  }, character(1)))
  
  # format tags
  tags <- vapply(tags, function (x) {
    x[c('class', '.date')] <- NULL
    vls <- vapply(x, tag_to_string, character(1))
    paste(names(x), vls, sep = ':', collapse = '  ')
  }, character(1))

  if (any(nchar(tags) > 0)) tags <- c('tags', tags)
  
  # final formatting
  prefix <- if (.full) list(nms, cls, dts, szs) else list(cls)
  lines <- lapply(prefix, format, justify = 'right')
  lines$tags <- format(tags, justify = 'left')
  lines <- do.call(paste, c(lines, list(sep = '  ')))
  lines <- vapply(lines, toString, character(1), width = width, USE.NAMES = FALSE)
  
  # ordering & printing
  no <- format(c('', as.character(seq_along(lines[-1]))), justify = 'left')
  cat(paste(no, c(lines[1], lines[-1][order(nms[-1])]), collapse = '\n'), '\n', sep = '')
  
  if (length(col) > n) cat('...\n')
}


list_wide <- function (col, width) {
  data <- list(
    `long id` = as.character(col),
    size      = format_size(file.info(obj_files(col))$size)
  )
  data <- c(data, readRDS(tag_files(col)))
  
  nms <- format(names(data), justify = 'left')
  vls <- vapply(data, tag_to_string, character(1))
  
  # break lines if tag values are too long
  mapply(function(name, value) {
    lines <- strwrap(value, width = width, exdent = nchar(name), initial = name)
    cat(paste0(lines, collapse = '\n'), '\n', sep = '')
  }, name = paste0(nms, ' : '), value = vls)
}


tag_to_string <- function (value) {
  if (length(value) <= 1) return(as.character(value))
  paste0('[', paste(value, collapse = ', '),  ']')
}


# --- collection: adding & removing objects ----------------------------

#' Add object to collection
#' 
#' @param col Collection object.
#' @param obj Object to be added.
#' @param ... Named expressions producing \code{atomic}, single-element tag values.
#' @param .tags \code{list} of \code{atomic}, single-element tags.
#' @param .overwrite If \code{TRUE} and object already exists, it will be overwritten.
#' @return Single-element collection containing the newly added object.
#' 
#' @export
#' 
#' @importFrom lazyeval lazy_dots
add_object <- function (col, obj, ...,  .tags, .overwrite = F) {
  dots <- lazy_dots(...)
  res <- add_object_(col, obj, .dots = dots, .tags = .tags, .overwrite = .overwrite)
  
  # notify user only in the interactive version of "add"
  if (identical(parent.frame(), globalenv()))
    message("collection has changed, refresh it")
  
  res
}


#' @param .dots \code{\link[lazyeval]{lazy}} object equivalent to
#'              \code{...} parameter of \code{\link{add_object}}
#' 
#' @rdname add_object
#' 
#' @export
#' @importFrom plyr defaults
add_object_ <- function (col, obj, .dots, .tags, .overwrite = F) {
  # basic checks
  stopifnot(is_collection(col))
  
  if (!missing(.tags))
    stopifnot(is.list(.tags))
    
  # hash of the object's contents
  id   <- md5(no_tags(obj))
  file <- make_path(id)
  file <- file.path(path(col), file)
  
  # destination file
  if (file.exists(paste0(file, '.rds'))) {
    if (!.overwrite) stop('object already exists', call. = F)
    warning('object already exists, forced to overwrite', call. = F)
  }
  else
    dir.create(dirname(file), recursive = T, showWarnings = F)
  
  # check and combine .dots & .tags
  tags <- all_tags(obj, .dots, .tags)
  
  # save data to filesystem
  saveRDS(obj, paste0(file, '.rds'))
  saveRDS(tags, paste0(file, '_tags.rds'))
  
  from_collection(id, col)
}


#' Remove specified objects.
#' 
#' Removes all object and tag files specified by \code{col}.
#' 
#' @param col A collection object.
#' @return An empty collection.
#' 
#' @export
#' @examples
#' \dontrun{
#'   col <- collection('sample-collection')
#'   
#'   # remove all objects
#'   remove_objects(col)
#'   
#'   # remove selected objects
#'   col2 <- filter(col, size > 100, days == 10)
#'   remove_objects(col2)
#' }
remove_objects <- function (col) {
  stopifnot(is_collection(col))
  # this might be a sub-collection
  verify_files(col, TRUE)
  
  # remove object & tag files
  unlink(c(obj_files(col), tag_files(col)), T, T)
  
  # remove empty directories
  dirs <- list.dirs(path(col), full.names = T, recursive = T)
  dirs <- sort(dirs, decreasing = T) # shorter names come later
  lapply(dirs, function (dir) {
    if (length(list.files(dir))) return() # not empty
    unlink(dir, recursive = F, force = F)
  })
  
  # remove an empty collection
  structure(character(0), path = path(col), class = 'collection')
}


# --- collection: filtering & grouping ---------------------------------

#' @export
#' @importFrom dplyr filter filter_ %>%
#' @importFrom magrittr extract2
#' @importFrom lazyeval lazy_eval
filter_.collection <- function (col, .dots, cores = getOption('cores', 1)) {
  stopifnot(is_collection(col))
  
  files <- file.path(path(col), make_path(col))
  res <- run_in_parallel(files, t_apply, function (tags) {
    # compute expressions
    vals <- lazy_eval(.dots, tags)
    # by default tags should return one logical value
    il <- (vapply(vals, length, integer(1)) > 1)
    if (any(il)) {
        warning(tags$.id, ': tag(s) did not compute to single value: ',
                paste(names(vals)[il], collapse = ', '), call. = FALSE)
    }
    # recompute, all must be true
    all(vapply(vals, function(val) any(as.logical(val)), logical(1)))
  }, cores)
  
  res <- as_ply_result(res)
  ids <- unlist(res$res)
  ids <- if (is.null(ids)) list() else names(which(ids))
  
  # discard the result if there are any errors
  if (length(res$err)) {
    return(structure(character(), errors = res$err, class = 'ply_result'))
  }
  
  # return a new collection if filtering went correctly
  structure(ids, path = path(col), class = 'collection')
}


#' @export
#' @importFrom dplyr group_by_ data_frame as_data_frame select bind_cols %>%
#' @importFrom plyr mdply
#' @importFrom assertthat has_attr
group_by_.collection <- function (col, .dots, add = FALSE) {
  tag_names <- vapply(.dots, function(x) as.character(x$expr), character(1))
  if (any(nchar(tag_names) == 0))
    stop('all .dots must be coercible to a non-empty character value', call. = FALSE)
  
  # read tags, check if they are all present, return only requested
  read_tags <- function (id, path) {
    tagset <- readRDS(path)
    if (!all(tag_names %in% names(tagset))) {
      nf <- paste("'", setdiff(tag_names, names(tagset)), "'", collapse = "', '")
      stop('tags ', nf, 'not found for object ', id)
    }
    as_data_frame(tagset[tag_names])
  }
  
  # read tags
  tags <-
    data_frame(id = as.character(col), path = tag_files(col)) %>%
    mdply(read_tags) %>%
    select(-id, -path)
  
  # merge with previous grouping
  if (add && has_attr(col, 'grouped')) {
    tags <- bind_cols(attr(col, 'grouped'), tags)
  }
  
  tags <- group_by_(tags, .dots = tag_names, add = add)
  attr(col, 'grouped') <- tags
  
  col
}

#' @export
#' @importFrom assertthat has_attr
ungroup.collection <- function (col) {
  if (has_attr(col, 'grouped'))
    attr(col, 'grouped') <- NULL
  col
}

#' @importFrom assertthat has_attr
is_grouped <- function (col) {
  stopifnot(is_collection(col))
  has_attr(col, 'grouped')
}


# --- reading objects --------------------------------------------------

#' Read object(s) into memory.
#' 
#' \code{read_all} reads all objects into a \code{list}.
#' 
#' @param col A \emph{collection} object.
#' 
#' @export
read_all <- function (col) {
  if (!length(col)) return(list())
  clply(col, function(o, t) o)
}


#' @description \code{read_one} reads one object into memory; there is
#' no wrapping \code{list}.
#' @param n Index of the object to be read.
#' @rdname read_all
#' @export
read_one <- function (col, n = 1) {
  if (!length(col))
    stop('cannot read one element, collection is empty', call. = FALSE)
  
  path <- file.path(path(col), make_path(col[n]))
  c_apply(path, function(o, t) o)
}
