load_or_skip <- function (...) {
  names <- deparse(substitute(...))
  for (name in names) {
    if (!require(name, quietly = T, warn.conflicts = F, character.only = T))
      skip(paste('could not load', name))
  }
}
