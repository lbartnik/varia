#' Evaluate a package.
#' 
#' @param pkg Package to be avaluated.
#' 
#' @export
#' @importFrom dplyr filter select %>%
#' @importFrom plyr dlply
#' 
#' @examples
#' \dontrun{
#' pkg <- package(mean)
#' eval_pkg(pkg)
#' }
pkg_eval <- function (pkg, data) {
  stopifnot(is_package(pkg))
  
  # 1. load the libraries
  load_deps(pkg$deps)
  
  # 2. assign global functions
  e <- new.env(parent = globalenv())
  load_pkg_to_env(pkg, e)

  # 3. run user code
  run_entry(e, data)
  
  # TODO finish
  # 4. process results
}


load_deps <- function (deps) {
  load <- function (name) require(name, quietly = T, warn.conflicts = F, character.only = T)
  libs <- unique(deps$lib)
  succ <- vapply(libs, load, logical(1))
  if (!all(succ))
    stop('could not load: ', paste(libs[!succ], sep = ', '), call. = F)
}

#' Load package to a given environment.
load_pkg_to_env <- function (pkg, e) {
  stopifnot(is_package(pkg))
  
  args <- as.list(pkg$global)
  args$FUN <- function (name, fun, env) {
    if (length(env)) {
      fe <- as.environment(env)
      parent.env(fe) <- e
      environment(fun) <- fe
    } else
      environment(fun) <- e
    assign(name, fun, envir = e)
  }
  do.call(mapply, args)
  
  # 3. make sure all other functions are available
  #   for (pkg_name in setdiff(names(pkg$deps), 'global')) {
  #     for (func_name in pkg$deps[[pkg_name]]) {
  #       res <- tryCatch(`::`(pkg_name, func_name), error = toString)
  #       if (!is.function(res)) {
  #         errors$push_back(paste0('function ', pkg_name, '::',
  #                                 func_name, ' not available'))
  #       }
  #     }
  #   }
}

run_entry <- function (env, data) {
  do.call('__entry__', data, envir = env, quote = T)
}

