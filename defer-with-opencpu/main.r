# library(defer)

package <- function (...) {
  
}

package_ <- function (.dots) {
  
}

execute <- function (what, where) {
  
}

# ----------------------------------------------------------------------

# various ways to put code together, produces an deferred evaluation
# package that can be sent to remote R sessions for evaluation
p <- package( { code ; expression } , summary, func = stats::rnorm, funb = function (x) x*x )

# creates a list of nodes that can be used to compute/evaluate the
# package
nodes <- opencpu(10, 'localhost', 12345) + RServe(4, 'localhost', 54321)

# how to pass the data that is the argument for the top-level function
# in the evaluation package?
#
# this call will evaluate the package in a remote session, in the
# background; it will return a handle to the evaluating instance/session
# which when printed checks if the results are available and downloads
# them or just prints a message that the computation is still underway
execute(p, data, nodes)

