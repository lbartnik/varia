get_pkgs <- function (names) {
  session_info(include_base = T)$packages %>%
    filter(package %in% names) %>%
    select(package, version)
}

mock_eval_pkg <- function (exec, deps, pkgs) {
  if ('global' %in% names(deps))
    deps$global <- lapply(deps$global, function (x) {
      environment(x$fun) <- emptyenv()
      x
    })
  structure(list(exec = exec, deps = deps, packages = get_pkgs(pkgs)),
            class = 'evaluation_package')
}
