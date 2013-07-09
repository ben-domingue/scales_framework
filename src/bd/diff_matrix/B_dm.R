dm<-function(theta,hat) {
  make.mat<-function(x) {
    length(x)->N
    m1<-matrix(x,N,N,byrow=TRUE)
    m2<-matrix(x,N,N,byrow=FALSE)
    abs(m1-m2)->M
    diag(M)<-NA
    M
  }
  make.mat(theta)->M.theta
  make.mat(hat)->M.hat
  M.hat-M.theta->M
  apply(M,2,median,na.rm=TRUE)->med
  loess(med~theta)->mod
  xv<-seq(min(theta),max(theta),length.out=1000)
  predict(mod,xv)->yv
  data.frame(xv=xv,yv=yv)
}
dm(theta.rm,hat.rm)->dm.rm
dm(theta.lcr,hat.lcr)->dm.lcr

plot(dm.rm$xv,dm.rm$yv,type="l",ylim=c(-.21,.21),lwd=2)
lines(dm.lcr$xv,dm.lcr$yv,col="red",lwd=2)
legend("bottomright",c("rasch model","lcr"),lty=1,col=c("black","red"),lwd=2)

  
