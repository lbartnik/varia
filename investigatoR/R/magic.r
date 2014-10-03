#' Add some magic.
#' 
#' @importFrom assertthat has_attr
#' @export
#' 
magic <- function (X)
{
    Y <- substitute(X)
    if (!is.call(Y)) stop('argument must be a call')
    if (length(Y) != 2) stop('the call can take only one parameter')

    fname <- deparse(Y[[1]])
    if (grepl('::', fname)) {
      splt <- strsplit(fname, '::')
      fname <- splt[[1]][2]
      fenv  <- asNamespace(splt[[1]][1])
    }
    else {
      fenv <- parent.frame()
    }
      
    if (!exists(fname, envir = fenv))
      stop('function ', fname, ' does not exist')
    
    aname <- deparse(Y[[2]])
    if (!exists(aname, envir = parent.frame()))
      stop('argument ', aname, ' to ', fname, ' does not exist')

    dset <- eval(Y[[2]], envir = parent.frame())
    res  <- force(X)
    
    if (!has_attr(dset, 'inv-reference'))
      stop("no 'inv-reference' in the input data set")
    
    # TODO: references pointing to Przemek's archiver
    attr(res, 'inv-reference') <- attr(dset, 'inv-reference')
    
    calls <- grep('inv-call-[[:digit:]]+', names(attributes(dset)), value = T)
    for (cname in calls)
      attr(res, cname) <- attr(dset, cname)
    
    fun <- get(fname, envir = parent.frame())
    # TODO: calling functions from other might be problematic
    #       if these environments get removed in between function
    #       calls - or when a given data set is stored and the
    #       environments are not; maybe there is a way to see if
    #       there are any global references in a given function?
    #if (environment(fun) != globalenv())
    #  stop('function ', fname, ' is not in global env')
    
    attr(res, paste0('inv-call-', length(calls)+1)) <- fun
    
    res
}

