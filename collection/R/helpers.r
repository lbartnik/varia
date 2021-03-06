#' Creates a sample collection.
#' 
#' @param path Where to create the collection.
#' @param size Add that many random objects.
#' @return A \emph{collection} object.
#' 
#' @export
create_sample_collection <- function (
  path = file.path(tempdir(), 'sample-collection'),
  size = 100
)
{
  if (file.exists(path)) {
    warning('directory ', path, ' already exists, just reading', call. = F)
    return(collection(path, 'a sample collection'))
  }
  
  col <- collection(path, 'a sample collection', .create = T)
  lapply(seq(size), function(i) {
    x <- iris[sample.int(nrow(iris), 50), ]
    add_object(col, x, .tags = as.list(table(x$Species)))
  })
  refresh(col)
}
