#' Execute an evaluation package in the current session.
#'
#' @export
#' @importFrom base64enc base64decode
remote_run_opencpu <- function (package, arguments) {
  raw_vector <- base64decode(package)
  readRDS(rawConnection(raw_vector, 'r'))

  Sys.sleep(10)

  return('It works!')
}


#' Send an evaluation package to an OpenCPU session for evaluation.
#'
#' @param what Evaluation package.
#' @param where OpenCPU server address:port
#'
#' @export
#' @importFrom httr POST
#' @importFrom parallel mcparallel
defer_opencpu <- function (where, package, arguments)
{
  stopifnot(is_package(package))
  stopifnot(is.list(arguments))

  body <- list(
    package   = paste0('"', serialize_and_b64encode(package), '"'),
    arguments = paste0('"', serialize_and_b64encode(arguments), '"')
  )

  where <- paste0('http://', where, "/ocpu/library/defer2/R/remote_run_opencpu/json")

  # run & create handle
  result_handle <- mcparallel(POST(where, body = body), name = 'some_name', silent = TRUE)

  # check for errors
  if (is_error(e <- result_handle)) {
    stop(as.character(e))
  }

  # return an interactive handle
  OpenCPU_Handle$new(result_handle)
}


#' @importFrom parallel mccollect
#' @importFrom R6 R6Class
OpenCPU_Handle <- R6::R6Class('OpenCPU_Handle',
  public = list(
    initialize = function (result_handle) {
      private$result_handle <- result_handle
    },
    print = function () {
      if (is.null(private$result))
        private$result <- mccollect(private$result_handle, wait = FALSE)
      if (is.null(private$result)) {
        cat('results not available yet\n')
      }
      else {
        cat('results from a remote OpenCPU run:\n')
        print(private$result)
      }
    }
  ),
  private = list(
    result_handle = NULL,
    result        = NULL
  )
)


#' @importFrom base64enc base64encode
serialize_and_b64encode <- function (object)
{
  # serialize object
  RDSbuffer <- rawConnection(raw(0), 'w')
  saveRDS(object, RDSbuffer)
  serialized <- rawConnectionValue(RDSbuffer)
  close(RDSbuffer)

  # returned serialized & encoded object
  base64encode(serialized)
}
