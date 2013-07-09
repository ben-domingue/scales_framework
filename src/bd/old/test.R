#deprecated


## setwd("/home/domingue/scale_hierarchy")
## n.3mat<-2500
## n.workers<-10
## #
## library(ConjointChecks)
## #
## c(1,2,3,5,12,16)->index
## out<-list()
## for (i in index) {
##   paste("D",i,"_lg.dat",sep="")->fn
##   read.table(fn,header=TRUE)->x
##   PrepareChecks(x)->tmp
##   ConjointChecks(tmp$N,tmp$n,n.3mat=n.3mat,par.options=list(n.workers=n.workers))->out[[i]]
## }
## save(out,file="~/scale.Rdata")

## sapply(out,is.null)->index
## out[!index]->out
## c("one","two","three","five","twelve","sixteen")->names(out)
## lapply(out,function(x) summary(x)$Means$unweighted)


