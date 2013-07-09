#krypton
setwd("/home/domingue/scale_hierarchy/data")
n.workers<-15
#
library(ConjointChecks)
#
list.files(pattern="D.+dat")->lf

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  ConjointChecks(tmp$N,tmp$n,n.3mat="adjacent",par.options=list(n.workers=n.workers))->out[[fn]]
}
save(out,file="~/scale_adjacent.Rdata")

#single cancellation
setwd("/home/domingue/scale_hierarchy/data")
n.workers<-5
#
library(ConjointChecks)
#
list.files(pattern="D.+dat")->lf
out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  ConjointChecks(tmp$N,tmp$n,n.3mat="adjacent",par.options=list(n.workers=n.workers),single=TRUE)->out[[fn]]
}
save(out,file="~/scale_adjacent_single.Rdata")

