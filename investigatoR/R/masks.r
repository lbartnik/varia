#' Adds magic to \code{\link[stats]{lm}}.
#' 
#' 
lm <- function(...)
{
  # TODO: match arguments with stats::lm and determine 'data'
  # then add 'data' to repository and run stats::lm as requested
  # and return the final result with all necessary attributes
  stats::lm(...)
}
