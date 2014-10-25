make_lazy <- function (f)
{
  wrapper <- function() { }
  formals(wrapper) <- formals(f)
  
  bdy <- sprintf("{ .call <- match.call()\n.call[[1]] <- as.name('%s')\neval(.call) }",
                 deparse(substitute(f)))
  
  body(wrapper) <- parse(text = bdy)
  
  parent.env(environment(f)) <- environment(wrapper)
  environment(wrapper) <- environment(f)
  
  wrapper
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
