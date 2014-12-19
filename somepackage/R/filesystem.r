#' @importFrom dplyr %>%
#' @importFrom plyr alply
#' @importFrom tools file_path_sans_ext
expand_name <- function (x, n = 2) {
  if (!is.character(x)) x <- get_id(x)
  y <- file_path_sans_ext(x)
  if (nchar(y) %/% 2 < n)
    stop('name too short, cannot expand by ', n, ' level(s)',
         call. = F)
  
  data.frame(start = seq(n)*2-1, stop = seq(n)*2) %>%
    alply(1, function (s) {
      substr(y, s$start, s$stop)
    }) %>%
    {do.call(file.path, .)} %>%
    file.path(x)
}
