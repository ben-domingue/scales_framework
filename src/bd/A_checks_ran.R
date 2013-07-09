setwd("/home/domingue/scale_hierarchy/data")
n.workers<-15
n.3mat<-2500
#
library(ConjointChecks)
#
list.files(pattern="D.+dat")->lf

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  ConjointChecks(tmp$N,tmp$n,n.3mat=n.3mat,par.options=list(n.workers=n.workers))->out[[fn]]
}
save(out,file="~/scale_random.Rdata")

#single
setwd("/home/domingue/scale_hierarchy/data")
n.workers<-5
n.3mat<-2500
#
library(ConjointChecks)
#
list.files(pattern="D.+dat")->lf

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  ConjointChecks(tmp$N,tmp$n,n.3mat=n.3mat,par.options=list(n.workers=n.workers),single=TRUE)->out[[fn]]
}
save(out,file="~/scale_random_single.Rdata")
