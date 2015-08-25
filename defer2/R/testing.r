#' @export
#' @importFrom base64enc base64decode
test <- function (x) {
  r <- base64decode(x)
  readRDS(rawConnection(r, 'r'))
}
