library(ConjointChecks)
#
n.workers<-24
#
setwd("~/scale_hierarchy/data")
list.files(pattern="D.+dat")->lf

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="R")->out[[fn]]
}
out->out_rows
save(out_rows,file="~/scale_rows.Rdata")

out<-list()
for (fn in lf) {
  read.table(fn,header=TRUE)->x
  PrepareChecks(x)->tmp
  SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="C")->out[[fn]]
}
out->out_cols
save(out_cols,file="~/scale_cols.Rdata")



###########################################################################
library(ConjointChecks)
load("/home/bd/Dropbox/scale_hierarchy/data/Generating Models.Rdata")
ordered(d$genModel,c("UNC","MON","IIO","DM","LCR","RSH"))->d$genModel

summary.item<-function(object, ...) {
  mean(colMeans(object@tab,na.rm=TRUE),na.rm=TRUE)
}
setwd("/home/bd/Dropbox/scale_hierarchy/data")
load("scale_rows.Rdata")
sapply(out_rows,summary.item)->tmp
data.frame(data=names(tmp),adjacent_rows=tmp)->z
load("scale_cols.Rdata")
sapply(out_cols,summary.item)->tmp
data.frame(data=names(tmp),adjacent_cols=tmp)->tmp
merge(z,tmp,by="data")->z
merge(z,d,by=1)->z
#
load("scale_adjacent.Rdata")
sapply(out,summary.item)->tmp
data.frame(data=names(tmp),adjacent_item=tmp)->tmp
merge(z,tmp)->z
load("scale_adjacent_single.Rdata")
sapply(out,summary.item)->tmp
data.frame(data=names(tmp),adjacent_item_single=tmp)->tmp
merge(z,tmp)->z

par(mfrow=c(2,2))
boxplot(adjacent_cols~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Columns")
boxplot(adjacent_rows~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Rows")
boxplot(adjacent_item~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Items, SC + DC")
boxplot(adjacent_item_single~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Items, SC")

pdf("/home/bd/Dropbox/FRIENDS/berkeley/draft1/figs/boxplots_single.pdf",width=6,height=9)
par(mfrow=c(2,1))
boxplot(adjacent_cols~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Columns")
boxplot(adjacent_rows~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Rows")
dev.off()
