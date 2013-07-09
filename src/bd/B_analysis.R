library(ConjointChecks)
load("/home/bd/Dropbox/scale_hierarchy/data/Generating Models.Rdata")
ordered(d$genModel,c("UNC","MON","IIO","DM","LCR","RSH"))->d$genModel

summary.person<-function(object, ...) {
  mean(rowMeans(object@tab,na.rm=TRUE),na.rm=TRUE)
}
summary.item<-function(object, ...) {
  mean(colMeans(object@tab,na.rm=TRUE),na.rm=TRUE)
}
setwd("/home/bd/Dropbox/scale_hierarchy/data")
load("scale_random.Rdata")
sapply(out,summary.person)->tmp
data.frame(data=names(tmp),random_person=tmp)->z
sapply(out,summary.item)->tmp
data.frame(data=names(tmp),random_item=tmp)->tmp
merge(z,tmp,by="data")->z
load("scale_adjacent.Rdata")
sapply(out,summary.person)->tmp
data.frame(data=names(tmp),adjacent_person=tmp)->tmp
merge(z,tmp,by="data")->z
sapply(out,summary.item)->tmp
data.frame(data=names(tmp),adjacent_item=tmp)->tmp
merge(z,tmp,by="data")->z
merge(d,z,by.x="Dataset",by.y="data")->z

pdf("/home/bd/Dropbox/FRIENDS/berkeley/draft1/figs/boxplots.pdf",width=6,height=9)
par(mfrow=c(2,1))
boxplot(random_item~genModel,z,ylim=c(0,1))
mtext(side=3,"Random Item")
boxplot(adjacent_item~genModel,z,ylim=c(0,1))
mtext(side=3,"Adjacent Item")
## boxplot(random_person~genModel,z,ylim=c(0,1))
## mtext(side=3,"random person")
## boxplot(adjacent_person~genModel,z,ylim=c(0,1))
## mtext(side=3,"adjacent person")
dev.off()

