library(ConjointChecks)
matrix(c(15,47,61,84,82,86,60,47,8),9,9,byrow=FALSE)->N
per <-structure(c(0, 0.06, 0.07, 0.18, 0.13, 0.13, 0.17, 0.17,
                  1, 0, 0.04, 0.15, 0.24, 0.33, 0.28, 0.47, 0.85, 1, 0, 0.04, 0.08,
                  0.12, 0.3, 0.64, 0.85, 1, 1, 0, 0.19, 0.39, 0.4, 0.51, 0.58,
                  0.82, 0.98, 1, 0, 0.06, 0.18, 0.52, 0.73, 0.95, 1, 1, 1, 0,
                  0.23, 0.33, 0.51, 0.68, 0.91, 0.93, 1, 1, 0.27, 0.51, 0.61,
                  0.64, 0.68, 0.77, 0.9, 1, 1, 0, 0.21, 0.52, 0.68, 0.84, 0.97,
                  0.97, 1, 1, 0.73, 0.64, 0.67, 0.7, 0.78, 0.78, 0.9, 1, 1),
                .Dim = c(9L, 9L) )
round(per*N)->n

## ConjointChecks(N,n,n.3mat="adjacent",single=TRUE,seed=383,par.options=list(n.workers=24))->adj.sc
## ConjointChecks(N,n,n.3mat="adjacent",seed=383,par.options=list(n.workers=24))->adj.dc
## ConjointChecks(N,n,n.3mat=2000,single=TRUE,seed=383,par.options=list(n.workers=24))->ran.sc
## ConjointChecks(N,n,n.3mat=2000,seed=383,par.options=list(n.workers=24))->ran.dc
## save.image(file="~/tmp/single_example.Rdata") #krypton

load(file="~/Dropbox/scale_hierarchy/data/single_example.Rdata") #krypton
pdf("/home/bd/Dropbox/FRIENDS/berkeley/draft1/figs/single_example.pdf",width=5,height=5)
par(mfrow=c(1,2))
plot(summary(adj.sc)$items,summary(adj.dc)$items,xlab="Single",ylab="Double",type="n",main="Adjacent")
text(summary(adj.sc)$items,summary(adj.dc)$items,1:nrow(N))
abline(0,1)
plot(summary(ran.sc)$items,summary(ran.dc)$items,xlab="Single",ylab="Double",type="n",main="Random")
text(summary(ran.sc)$items,summary(ran.dc)$items,1:nrow(N))
abline(0,1)
dev.off()





###drilling deeper

#simple example
ro<-3:5
co<-c(2,4,9)
ConjointChecks_exp(N[ro,co],n[ro,co],n.3mat=1,single=TRUE,seed=383)->out.sc
ConjointChecks_exp(N[ro,co],n[ro,co],n.3mat=1,seed=383)->out.dc
n[ro,co]/N[ro,co]
out.sc[[3]][[1]][[3]][[4]]->sc
out.dc[[3]][[1]][[3]][[4]]->dc

#no dc
ro<-4:6
co<-4:6
n[ro,co]/N[ro,co]

#violatoin
n/N->pv
hold<-list()
for (i in 1:(nrow(N)-2)) for (j in 1:(ncol(N)-2)) {
  pv[i+1,j]<pv[i,j+1]->t1
  pv[i+2,j+1]<pv[i+1,j+2]->t2
  pv[i+2,j]>pv[i,j+2]->t3
  c(t1,t2,t3)->test
  if (all(test) | all(!test)) c(i,j)->hold[[paste(i,j)]]
}


for (ii in 1:length(hold)) {
  hold[[ii]][1]->i
  i:(i+2)->ro
  hold[[ii]][2]->j
  j:(j+2)->co
  ConjointChecks(N[ro,co],n[ro,co],n.3mat=1,single=TRUE,seed=383)->out.sc
  ConjointChecks(N[ro,co],n[ro,co],n.3mat=1,seed=383)->out.dc
  if (summary(out.sc)$Means[[1]]<summary(out.dc)$Means[[2]]) print(ii)
}




