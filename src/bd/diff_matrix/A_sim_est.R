n.items<-45
n.respondents<-5000
n.levels<-10
#
rnorm(n.respondents)->theta.rm
rnorm(n.levels)->tmp
sample(1:n.levels,n.respondents,replace=TRUE)->index
tmp[index]->theta.lcr
#
#simulate data
rnorm(n.items)->diff
get.answers<-function(theta,diff) {
  length(diff)->n.items
  length(theta)->n.respondents
  matrix(theta,n.respondents,n.items,byrow=FALSE)->m1
  matrix(diff,n.respondents,n.items,byrow=TRUE)->m2
  m1-m2 -> kern
  exp(kern)/(1+exp(kern))->pv.true
  runif(n.items*n.respondents)->test
  ifelse(pv.true>test,1,0)->resp
  resp
}
get.answers(theta.lcr,diff)->resp.lcr
get.answers(theta.rm,diff)->resp.rm

## #first look at double cancellation
## library(ConjointChecks)
## PrepareChecks(resp)->tmp
## tmp$n/tmp$N->pv.obs
## fun<-function(pv) {
##   hold<-list()
##   for (i in 1:(nrow(N)-2)) for (j in 1:(ncol(N)-2)) {
##     pv[i+1,j]>pv[i,j+1]->t1
##     pv[i+2,j+1]>pv[i+1,j+2]->t2
##     pv[i+2,j]>pv[i,j+2]->t3
##     c(t1,t2,t3)->hold[[paste(i,j)]]
##   }
##   do.call("rbind",hold)->tab
##   ftable(tab[,1],tab[,2],tab[,3])
## }
## fun(pv.true)->t1
## fun(pv.obs)->t2

library(eRm)
RM(resp.lcr)->tmp
person.parameter(tmp)$theta.table[,1]->hat.lcr
RM(resp.rm)->tmp
person.parameter(tmp)$theta.table[,1]->hat.rm




