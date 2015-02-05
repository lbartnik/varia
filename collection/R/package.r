# from https://stackoverflow.com/questions/14276728/finding-the-names-of-all-functions-in-an-r-expression/14295659#14295659
find_calls <- function(x) {
  # Base case
  if (!is.recursive(x)) return()
  
  recurse <- function(x) {
    sort(unique(as.character(unlist(lapply(x, find_calls)))))
  }
  
  if (!is.call(x)) return(recurse(x))
  
  # x[[1]] is a name
  if (!is.call(x[[1]])) {
    f_name <- as.character(x[[1]])
    return(c(f_name, recurse(x[-1])))
  }

  # x[[1]] is a call
  y <- x[[1]]
  if (identical(y[[1]], as.name('::')) || identical(y[[1]], as.name(':::'))) {
    f_name <- deparse(y)
    return(c(f_name, recurse(x[-1])))
  }
  
  # default from the original version; something other than :: and :::
  c(as.character(x[[1]]), recurse[-1])
}

#NOT_STORED <- methods:::.BasicFunsList
# from testthat:mock.r
pkg_rx <- ".*[^:]"
colons_rx <- "::(?:[:]?)"
name_rx <- ".*"
pkg_and_name_rx <- sprintf("^(?:(%s)%s)?(%s)$", pkg_rx, colons_rx, name_rx)


#' @importFrom dplyr filter %>%
#' @importFrom plyr ldply
choose_funcs <- function (names) {
  fncs <- ldply(names, function (fname) {
    pkg_name <- gsub(pkg_and_name_rx, "\\1", fname)
    name <- gsub(pkg_and_name_rx, "\\2", fname)
    
    # if package is given explicitely
    if (pkg_name != '') return(c(pkg = pkg_name, name = name))
    
    # try to access function and determine its environment
    f <- tryCatch(get(fname), error = function(x)'not-accessible')
    
    # error
    if (!is.function(f)) return(c(pkg = NA, name = fname))
    
    # primitive
    if (is.primitive(f)) return(c(pkg = 'primitive', name = fname))
    
    # not primitive; determine the environment
    e <- environment(f)
    if (identical(e, globalenv())) return(c(pkg = 'global', name = fname))
    
    # some package
    c(pkg = environmentName(e), name = fname)
  })
  
  # not found
  NF <- which(is.na(fncs$pkg))
  NF <- fncs$name[NF]
  if (length(NF) > 0) {
    # internally defined functions
    if (is.function(x)) x <- body(x)
    NF <- setdiff(NF, all.names(x, functions = T, unique = T))
  }
  
  if (length(NF) > 0) {
    warning('functions not found: ', paste(, sep = ', '),
            call. = F)
  }
  
  # finally, the list of functions
  fncs %>%
    filter(!is.na(pkg), pkg != 'primitive')
}


#' @importFrom dplyr bind_rows
#' @importFrom plyr dlply
build_funcs_pkg <- function (x) {
  # either a function name (character) or whole object
  stopifnot(is.character(x) || is.function(x))
  if (is.function(x)) x <- find_calls(x)
  
  # now find all function names
  fncs <- choose_funcs(x)

  # recursively find all functions called in globals
  not_processed <- fncs$name[fncs$pkg == 'global']
  processed     <- c()
  
  while (length(not_processed) > 0) {
    # pop and push function name
    name <- not_processed[1]
    not_processed <- not_processed[-1]
    processed <- append(processed, name)
    
    fun  <- tryCatch(
      get(name, envir = globalenv(), mode = 'function', inherits = F),
      error = function(x)NA
    )
    if (!is.function(fun)) next
    
    tmp  <- choose_funcs(find_calls(fun))
    fncs <- bind_rows(fncs, tmp)
    
    new_glb <- setdiff(tmp$name[tmp$pkg == 'global'], processed)
    not_processed <- append(not_processed, new_glb)
  }

  # make a list for each package
  info <- dlply(fncs, .(pkg), function(x)unique(x$name))
  if (any(names(info) == '')) {
    I <- which(names(info) == '')
    warning('package could not be determined for: ', paste(info[[I]], sep = ', '),
            call. = F)
    info[[I]] <- NULL # TODO maybe we shouldn't remove them?
  }
  
  attributes(info) <- list(names = names(info)) # drop other attributes
  info
}


#' @importFrom devtools session_info
build_exec_pkg <- function (x) {
  # determine if this is function or just a name
  orig <- substitute(x)
  if (is.name(orig)) {
    x <- as.character(orig)
  }
  
  # get all dependencies
  fpkg <- build_funcs_pkg(x)

  # assign package version
  pkgs <- session_info(include_base = T)$packages
  for (pname in setdiff(names(fpkg), 'global')) {
    I <- which(pkgs$package == pname)
    attr(fpkg[[pname]], 'version') <- pkgs$version[I]
  }
  
  # store global functions code
  if (length(fpkg[['global']])) {
    nms <- fpkg[['global']]
    fpkg[['global']] <- lapply(fpkg[['global']], function (name) {
      f <- get(name, envir = globalenv(), inherits = F)
      environment(f) <- emptyenv() # TODO make sure this is necessary for serialization
      f
    })
    names(fpkg[['global']]) <- nms
  }
  
  fpkg[['exec']] <- x # better then not be
  
  fpkg
}


# maybe this way
build_exec_pkg_2 <- function () {
  # if a name of packaged function -> record its name and package version
  # if a name of global function -> record its name and its dependencies
  # if code/expression -> record its dependencies and save its deparsed version
}


eval_pkg <- function (pkg) {
  e <- new.env(parent = globalenv())
  e$obj <- list() # object
  e$tag <- list() # tag(s)
  eval()
}

