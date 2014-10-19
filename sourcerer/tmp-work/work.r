library(dplyr)

s <- sourcerer('tmp-work/iris.rds')

y <- 1
do(s, data.frame(x=mean(.[,1]+y)))
