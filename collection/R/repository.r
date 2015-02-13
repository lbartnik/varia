#' Read or create a repository.
#' 
#' @param path Repository path.
#' @param .create If \code{path} does exist, create a new repository.
#' @return Object of class \emph{repository}.
#' 
#' @export
repository <- function (path, .create = FALSE) {
  if (!file.exists(path)) {
    if (!.create) stop('path does not exist but .create is FALSE', call. = FALSE)
    dir.create(path)
  }
  else if (!is_dir(path)) {
    stop('path does not point to a directory', call. = FALSE)
  }
  else if (.create) {
    warning('directory already exists but .create is TRUE', call. = FALSE)
  }
  
  # TODO maybe some basic checks?
  
  structure(path, class = 'repository')
}


is_repository <- function (x) inherits(x, 'repository')


#' @rdname collection
#' @export
collection.repository <- function (repo, name, comment, .create = FALSE) {
  dir_name <- file.path(repo, hash32(name))
  
  # if directory exists just return the object; comment and .create are discarded
  if (is_dir(dir_name)) {
    if (.create) warning('collection already exists but .create is TRUE', call. = FALSE)
    return(collection(dir_name))
  }

  # if does not exist, .create must be TRUE
  if (!.create) stop('no such collection in repository and .create is FALSE', call. = FALSE)
  
  col <- create_collection(dir_name, comment)
  saveRDS(name, paste0(dir_name, '.rds'))
  
  col
}


list_collections <- function (repo) {
  stopifnot(is_repository(repo))
  
  dirs <- setdiff(list.dirs(repo, full.names = T, recursive = F), repo)
  if (!length(dirs))
    return(character())
  
  ex <- file.exists(paste0(dirs, '.rds'))
  a  <- vapply(dirs[ex], function(x)readRDS(paste0(x, '.rds')), character(1))
  b  <- vapply(dirs[!ex], basename, character(1))
  sort(c(a, b))
}


#' @export
#' @importFrom dplyr between
`[.repository` <- function (repo, i) {
  # assumes ordering in list_collections
  cols <- list_collections(repo)
  
  if (is.numeric(i)) {
    stopifnot(between(i, 1, length(cols)))
    return(collection(repo, as.character(cols[i])))
  }
  
  if (is.character(i)) {
    stopifnot(any(cols == i))
    return(collection(repo, i))
  }
  
  stop(class(i), ' is not supported for indexing')
}


#' @export
print.repository <- function (repo) {
  # title line - repository path
  name <- path_to_name(normalizePath(repo), 2)
  
  cat("repository in '", name, "/'", sep = '')
  
  # print collections in alphabetical order to enable indexing
  cols <- list_collections(repo)
  if (length(cols)) {
    cat('\n')
    nm <- format(cols, width = max(nchar(cols)))
    
    invisible(mapply(function(name, path) {
      cat(name, ': ')
      print(collection(path))
      cat('\n')
    }, name = nm, path = names(nm)))
  }
  else
    cat('empty repository')
}

