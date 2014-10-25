make_lazy_fun <- function (f)
{
  wrapper <- function() { }
  formals(wrapper) <- formals(f)
  
  bdy <- sprintf("{ .call <- match.call()\n.call[[1]] <- as.name('%s')\neval(.call) }",
                 deparse(substitute(f)))
  
  body(wrapper) <- parse(text = bdy)
  
  parent.env(environment(f)) <- environment(wrapper)
  environment(wrapper) <- environment(f)
  
  # TODO wrapper should in fact return a lazy object that can be
  #      evaluated in a separate R process; it has to gather all the
  #      call parameters but only those actually specified in the
  #      call to the wrapper; so it has to use match.call somehow
  
  wrapper
}


# TODO wrappers are constructed in do.sourcerer; when there is only
#      one input set then 'do' evaluates the code instantly and
#      returns the results; in case there is more than one input
#      set (more than one input file) a 'lazy' object is returned
#      which can be evaluated later on in the same R process or in
#      another R process, and possibly in parallel in a cluster
f <- function (formula, data) {
  .call <- match.call(expand.dots = F)
  .call[[1]] <- as.name('lm')
  lazy_(.call, parent.frame())
}





# Previous approaches.
#
# make_lazy_simple <- function (f)
# {
#   wrapper <- function() { }
#   formals(wrapper) <- formals(f)
#   body(wrapper) <- substitute({ 
#     do.call(x, as.list(environment()))
#   }, list(x = substitute(f)))
#   environment(wrapper) <- environment(f)
#   
#   wrapper
# }
# 
# 
# make_lazy_parse <- function (f)
# {
#   wrapper <- function() { }
#   formals(wrapper) <- formals(f)
#   
#   nms <- setdiff(names(formals(f)), '...')
#   if (ellipsis <- ('... ' %in% nms)) {
#     nms <- setdiff(nms, '...')
#   }
#   
#   bdy <- sprintf('%s(%s%s)',
#                  deparse(substitute(f)),
#                  paste(nms, '=', nms, collapse=', '),
#                  ifelse(ellipsis, ', ...', ''))
#   # TODO ellipsis is not appearing
#   
#   body(wrapper) <- parse(text = bdy)
#   
#   parent.env(environment(f)) <- environment(wrapper)
#   environment(wrapper) <- environment(f)
#   
#   wrapper
# }
# 
