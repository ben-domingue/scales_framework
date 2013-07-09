n.items<-45
n.respondents<-5000
#simulate data
rnorm(n.items)->diff
rnorm(n.respondents)->theta
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp

#first look at double cancellation
library(ConjointChecks)
PrepareChecks(resp)->tmp
tmp$n/tmp$N->pv.obs
fun<-function(pv) {
  hold<-list()
  for (i in 1:(nrow(N)-2)) for (j in 1:(ncol(N)-2)) {
    pv[i+1,j]>pv[i,j+1]->t1
    pv[i+2,j+1]>pv[i+1,j+2]->t2
    pv[i+2,j]>pv[i,j+2]->t3
    c(t1,t2,t3)->hold[[paste(i,j)]]
  }
  do.call("rbind",hold)->tab
  ftable(tab[,1],tab[,2],tab[,3])
}
fun(pv.true)->t1
fun(pv.obs)->t2


##############################################################
#short
n.items<-20
n.respondents<-5000
rnorm(n.respondents)->theta
#simulate data
rnorm(n.items)->diff
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp.short
#medium
n.items<-50
#simulate data
rnorm(n.items)->diff
rnorm(n.respondents)->theta
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp.med
#long
n.items<-500
rnorm(n.items)->diff
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp.long

pdf("/home/bd/Dropbox/FRIENDS/berkeley/draft1/figs/noise.pdf",width=5,height=5)
par(mfrow=c(3,1),mar=c(3,3,2,2),oma=c(3,3,1,1))
data.frame(theta=theta)->df
yl<-c(0,1.2*sd(theta))
#
rowSums(resp.short)->df$theta.short
aggregate(df$theta,list(df$theta.short),sd)->obs.sd
plot(obs.sd,type="l",ylim=yl,lwd=2)
mtext(side=3,line=.5,"Short")
abline(h=true.sd,col="red",lwd=2)
#
rowSums(resp.med)->df$theta.med
aggregate(df$theta,list(df$theta.med),sd)->obs.sd
plot(obs.sd,type="l",ylim=yl,lwd=2)
mtext(side=3,line=.5,"Medium")
abline(h=true.sd,col="red",lwd=2)
#
rowSums(resp.long)->df$theta.long
aggregate(df$theta,list(df$theta.long),sd)->obs.sd
plot(obs.sd,type="l",ylim=yl,lwd=2)
mtext(side=3,line=.5,"Long")
abline(h=true.sd,col="red",lwd=2)
#
mtext(side=1,line=2,"Observed sum score")
mtext(side=2,line=.5,"SD of true scores associated with each sum score",outer=TRUE)
dev.off()
