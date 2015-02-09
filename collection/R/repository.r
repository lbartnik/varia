#' @export
repository <- function (path) {
  stopifnot(is_dir(path))
  structure(path, class = 'repository')
}

#' @export
create_repository <- function (path) {
  stopifnot(!file.exists(path))
  dir.create(path)
  repository(path)
}

is_repository <- function (x) inherits(x, 'repository')


#' @export
add_collection <- function (repo, name, comment = '') {
  stopifnot(is_repository(repo))
  
  dir_name <- file.path(repo, hash32(name))
  if (is_dir(dir_name))
    stop('collection already exists', call. = FALSE)
  
  col <- create_collection(dir_name, comment)
  saveRDS(name, paste0(dir_name, '.rds'))
  
  col
}

#' @export
collection.repository <- function (repo, name) {
  dir_name <- file.path(repo, hash32(name))
  if (!is_dir(dir_name))
    stop('no such collection in repository', call. = FALSE)
  collection(dir_name)
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
  splt <- function (x) { if (dirname(x) != x) c(basename(x), Recall(dirname(x))) } # reversed
  path <- splt(normalizePath(repo))
  name <- do.call(file.path, as.list(rev(if (length(path)>2) c(path[1:2], '...') else path)))
  
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

