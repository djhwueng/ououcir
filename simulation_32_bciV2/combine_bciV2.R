rm(list=ls())

postbci<-function(sample_params=sample_params,model=model,xlab=xlab,ylim=ylim){
  sample_median<-apply(sample_params,1,median)
  hist(post.params.array,main = model,breaks=6,freq=FALSE,cex.main=2.5,xlab= NULL,ylab="Density",ylim=ylim,cex.lab=3.5, cex.sub=2,col="lightgreen")
  lines(density(post.params.array))
  abline(v=quantile(post.params.array,probs=c(.025,.975)),col="red",lty=2)
  min.ylab<-min(density(post.params.array)$y)
  max.ylab<-max(density(post.params.array)$y)
  points((post.params.array),seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.5)
  k <- (sample_median<quantile(post.params.array,probs=0.025))+(sample_median>quantile(post.params.array,probs=0.975))
  points(sample_median,seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.5,col=c("blue","red")[1+k])
  segments(sample_median,seq(min.ylab,max.ylab, length=rep),post.params.array,seq(min.ylab,max.ylab,length=rep),col="grey")
  }



# OUBMBM 
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),axes=FALSE,ann=FALSE)
text(x=5,y=7,"Model",cex=4)
text(x=5,y=3.5,"OUBMBM",cex=4)

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmbm/oubmbm32_alpha.y.rda")
model<-expression(paste( alpha[y],sep= "     "))
xlab<-expression(alpha[y])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,3.5))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmbm/oubmbm32_sigmasq.x.rda")
model<-expression(paste( sigma[x],sep= "     "))
#xlab<-expression(sigma[x]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.9))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmbm/oubmbm32_tau.rda")
model<-expression(paste( tau,sep= "     "))
xlab<-expression(tau)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,1.1))

# OUOUBM

par(mfrow=c(2,3))
par(mar=c(2,1,2,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),axes=FALSE,ann=FALSE)
text(x=5,y=7,"Model",cex=4)
text(x=5,y=3.5,"OUOUBM",cex=4)

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/ououbm32_alpha.y.rda")
model<-expression(paste( alpha[y],sep= "     "))
xlab<-expression(alpha[y])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,2.8))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/ououbm32_sigmasq.x.rda")
model<-expression(paste( sigma[x],sep= "     "))
#xlab<-expression(sigma[x]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.7))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/ououbm32_tau.rda")
model<-expression(paste( tau,sep= "     "))
xlab<-expression(tau)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,1.4))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/ououbm32_alpha.x.rda")
model<-expression(paste( alpha[x],sep= "     "))
xlab<-expression(alpha[x])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,5))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/ououbm32_theta.x.rda")
model<-expression(paste( theta[x],sep= "     "))
xlab<-expression(theta[x])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.45))


# OUBMCIR
par(mfrow=c(2,3))
par(mar=c(2,2,2,2))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),axes=FALSE,ann=FALSE)
text(x=5,y=7,"Model",cex=4)
text(x=5,y=3.5,"OUBMCIR",cex=4)

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_alpha.y.rda")
model<-expression(paste( alpha[y],sep= "     "))
xlab<-expression(alpha[y])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,5))
  
load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_sigmasq.x.rda")
model<-expression(paste( sigma[x],sep= "     "))
#xlab<-expression(sigma[x]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.9))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_alpha.tau.rda")
model<-expression(paste( alpha[tau],sep= "     "))
xlab<-expression(alpha[tau])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,3))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_theta.tau.rda")
model<-expression(paste( theta[tau],sep= "     "))
xlab<-expression(theta[tau])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.5))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_sigmasq.tau.rda")
model<-expression(paste( sigma[tau],sep= "     "))
#xlab<-expression(sigma[tau]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,1.6))


# OUOUCIR
par(mfrow=c(2,4))
par(mar=c(2,2,2,2))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),axes=FALSE,ann=FALSE)
text(x=5,y=7,"Model",cex=4)
text(x=5,y=3.5,"OUOUCIR",cex=2.8)

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_alpha.y.rda")
model<-expression(paste( alpha[y],sep= "     "))
xlab<-expression(alpha[y])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,3.5))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_alpha.x.rda")
model<-expression(paste( alpha[x],sep= "     "))
xlab<-expression(alpha[x])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,6.5))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_theta.x.rda")
model<-expression(paste(theta[x],sep= "     "))
xlab<-expression(alpha[x])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.45))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_sigmasq.x.rda")
model<-expression(paste( sigma[x],sep= "     "))
#xlab<-expression(sigma[x]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.8))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_alpha.tau.rda")
model<-expression(paste( alpha[tau],sep= "     "))
xlab<-expression(alpha[tau])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,3))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououcir/ououcir32_theta.tau.rda")
model<-expression(paste( theta[tau],sep= "     "))
xlab<-expression(theta[tau])
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,0.45))

load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmcir/oubmcir32_sigmasq.tau.rda")
model<-expression(paste( sigma[tau],sep= "     "))
#xlab<-expression(sigma[tau]^2)
postbci(sample_params=sample_params,model=model,xlab=xlab,ylim=c(0,1.6))
