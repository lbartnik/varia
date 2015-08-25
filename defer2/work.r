# 1. package the function and save it into a file/object
# 2. upload package to remote session either as R Data or JSON
# 3. having the object in the remote session tmp/key run package
#
# 4. is it possible to upload package object directly to a function?



# --- 1. find a way to send serialized object to OpenCPU --------------

library(httr)

r <- GET("http://localhost:4822/ocpu/library/stats/R/rnorm")
POST("http://localhost:4822/ocpu/library/stats/R/rnorm", body = list(n = 10))
r <- GET("http://localhost:4822/ocpu/tmp/x008abbb9f4/R/.val/json")
str(content(r))
r

p <- package({ summary(.) })
package_eval(p, iris)


zz <- rawConnection(raw(0), 'w')
saveRDS('zz', zz)
vv <- rawConnectionValue(zz)
bb <- base64encode(vv)
rr <- base64decode(bb)
readRDS(rawConnection(rr, 'r'))

library(httr)
library(base64enc)
POST("http://localhost:4822/ocpu/library/defer2/R/test/json", body = list(x = paste0('"', bb, '"')))

GET("http://localhost:4822/ocpu/tmp/x082a295e26/stdout")

package_eval(fromJSON(txt = x), iris)




# --- 2. find a way to handle live OpenCPU connections via parallel ---

library(parallel)

f <- function (i) {
  Sys.sleep(i)
  return(i)
}

handles <- lapply(1:10, function (i) mcparallel(f(i), i))
sapply(handles, mccollect, wait = FALSE)

lapply(handles, mccollect, wait = FALSE)



h <- mcparallel(f(3))
mccollect(h, wait = FALSE)




