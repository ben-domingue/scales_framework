
setwd("/Users/dti/Dropbox/Research/TACM/src/dti/simdata") 

n.workers<-4
n.3mat<-2500
#
library(ConjointChecks)
#
lf <- list.files(pattern="T.+dat")

total <- 300
pb <- txtProgressBar(min = 0, max = total, style = 3)
h <- 0


out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="R")->out[[fn]]

  h <- h + 1
  print(fn)
  setTxtProgressBar(pb, h)
}

out->out_rows
save(out_rows,file="./scale_rows.Rdata")

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="C")->out[[fn]]
}
out->out_cols
save(out_cols,file="./scale_cols.Rdata")