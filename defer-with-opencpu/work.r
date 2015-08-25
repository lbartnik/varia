# 1. package the function and save it into a file/object
# 2. upload package to remote session either as R Data or JSON
# 3. having the object in the remote session tmp/key run package
#
# 4. is it possible to upload package object directly to a function?

library(httr)

r <- GET("http://localhost:4822/ocpu/library/stats/R/rnorm")
POST("http://localhost:4822/ocpu/library/stats/R/rnorm", body = list(n = 10))
r <- GET("http://localhost:4822/ocpu/tmp/x008abbb9f4/R/.val/json")
str(content(r))
r

library(defer)

p <- package({ summary(.) })
package_eval(p, iris)


library(jsonlite)
x <- toJSON(`class<-`(p, NULL))

t = textConnection('x', 'w', )
saveRDS(p, t, ascii = FALSE)

POST("http://localhost:4822/ocpu/library/defer/R/eval_json",
     body = list(p = base64encode(rawConnectionValue(zz), NA)))

GET("http://localhost:4822/ocpu/tmp/x082a295e26/stdout")

zz <- rawConnection(raw(0), 'w')
saveRDS(p, zz)
xx <- rawConnectionValue(zz)
readRDS(rawConnection(xx))


package_eval(fromJSON(txt = x), iris)


