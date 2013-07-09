library(ConjointChecks)
setwd("/home/bd/Dropbox/scale_hierarchy/data")
z<-list()
load("scale_random.Rdata")
sapply(out,function(x) c(summary(x)$Means$unweighted,summary(x)$Means$weighted))->z$random
load("scale_adjacent.Rdata")
sapply(out,function(x) c(summary(x)$Means$unweighted,summary(x)$Means$weighted))->z$adjacent
load("scale_random_single.Rdata")
sapply(out,function(x) c(summary(x)$Means$unweighted,summary(x)$Means$weighted))->z$random_single
load("scale_adjacent_single.Rdata")
sapply(out,function(x) c(summary(x)$Means$unweighted,summary(x)$Means$weighted))->z$adjacent_single
lapply(z,t)->z

do.call("cbind",z)->x
data.frame(x)->x
names(x)<-c("ran_mean_double","ran_weight_double","adj_mean_double","adj_weight_double","ran_mean_single","ran_weight_single","adj_mean_single","adj_weight_single")

strsplit(rownames(x),"D")->tmp
sapply(tmp,function(x) x[2])->tmp
strsplit(tmp,"_",fixed=TRUE)->tmp
sapply(tmp,function(x) x[1])->tmp
as.numeric(tmp)->x$iter
x[order(x$iter),]->x

pdf("/tmp/scale.pdf",width=15,height=15)
par(mfrow=c(3,1))
#
cols<-c("black","red","blue","green")
nms<-c("adj_mean_double","adj_weight_double","ran_mean_double","ran_weight_double")
matplot(subset(x,select=c(nms)),type="l",col=cols,ylim=c(0,1),xlab="",ylab="")
legend("topright",nms,col=cols,lty=1)
#
cols<-c("black","red","blue","green")
nms<-c("adj_mean_double","adj_weight_double","adj_mean_single","adj_weight_single")
matplot(subset(x,select=c(nms)),type="l",col=cols,ylim=c(0,1),xlab="",ylab="")
legend("topright",nms,col=cols,lty=1)
#
cols<-c("black","red","blue","green")
nms<-c("ran_mean_double","ran_weight_double","ran_mean_single","ran_weight_single")
matplot(subset(x,select=c(nms)),type="l",col=cols,ylim=c(0,1),xlab="",ylab="")
legend("topright",nms,col=cols,lty=1)
#
dev.off()
