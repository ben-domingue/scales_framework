n.respondents<-5000
rnorm(n.respondents)->theta
library(ConjointChecks)
out<-list()
n.workers<-15

#normal
n.items<-45
rnorm(n.items)->diff
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp
PrepareChecks(resp)->tmp
SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="R")->out[["normal-rows"]]
SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="C")->out[["normal-cols"]]

#many ites
n.items<-450
rnorm(n.items)->diff
matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
m1-m2 -> kern
exp(kern)/(1+exp(kern))->pv.true
runif(n.items*n.respondents)->test
ifelse(pv.true>test,1,0)->resp
PrepareChecks(resp)->tmp
SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="R")->out[["trans-rows"]]
SingleCancel(tmp$N,tmp$n,par.options=list(n.workers=n.workers),single="C")->out[["trans-cols"]]

save(out,file="~/tmp/table2x2.Rdata")

lapply(out,function(x) summary(x)$Means$unweighted)
