#setwd("~/Dropbox/ChihPingWang/Model1/abc_V2/simulation64V2/ououbm/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation64V2/ououbm/")


rm(list=ls())
library(TreeSim)
library(EasyABC)
library(coda)
library(Sim.DiffProc)
library(MCMCpack)
library(ape)
library(abc)
library(nlme)
library(adephylo)
library(maps)
library(phytools)
library(phytools)
library(phylobase)

ououbmmodel<-function(model.params,reg.params,root=root,tree=tree){
  alpha.y<<-model.params[1]
  alpha.x<-model.params[2]
  theta.x<-model.params[3]
  sigmasq.x<-model.params[4]
  tau<-model.params[5]
  
  b0<-reg.params[1]
  b1<-reg.params[2]
  b2<-reg.params[3]
  
  n<-Ntip(tree)
  x1nodestates<-array(0,c(2*n-1))
  x1nodestates[n+1]<-root
  x2nodestates<-array(0,c(2*n-1))
  x2nodestates[n+1]<-root
  optimnodestates<-array(0,c(2*n-1))
  optimnodestates[n+1]<-root
  sigmasqnodestates<-array(0,c(2*n-1))
  sigmasqnodestates[n+1]<-root
  ynodestates<-array(0,c(2*n-1))
  ynodestates[n+1]<-root
  
  N<-dim(tree$edge)[1]
  anc<-tree$edge[,1]
  des<-tree$edge[,2]
  treelength<-tree$edge.length
  
  for(index in N:1){
    
    x1ou.mean<-x1nodestates[anc[index]]*exp(-alpha.x*treelength[index]) + theta.x*(1-exp(-alpha.x*treelength[index]))
    x1ou.sd<-sqrt((sigmasq.x/(2*alpha.x))*(1-exp(-2*alpha.x*treelength[index])))
    x1nodestates[des[index]]<-rnorm(n=1,mean=x1ou.mean,sd=x1ou.sd)
    
    x2ou.mean<-x2nodestates[anc[index]]*exp(-alpha.x*treelength[index]) + theta.x*(1-exp(-alpha.x*treelength[index]))
    x2ou.sd<-sqrt((sigmasq.x/(2*alpha.x))*(1-exp(-2*alpha.x*treelength[index])))
    x2nodestates[des[index]]<-rnorm(n=1,mean=x2ou.mean,sd=x2ou.sd)
    
    optimnodestates[des[index]]<- b0 + b1*x1nodestates[des[index]] + b2*x2nodestates[des[index]]
    sigmasqnodestates[des[index]]<-rnorm(n=1,mean=sigmasqnodestates[anc[index]],sd=sqrt(tau*treelength[index]))
    sigmasq.theta<- b1^2*sigmasq.x + b2^2*sigmasq.x
    
    ###
    theta0.y<-optimnodestates[des[index]]        
    A<-(alpha.y*theta0.y/ (alpha.y-alpha.x)) *(exp((alpha.y-alpha.x)*treelength[index]) -1)
    tilde.theta.y<- optimnodestates[des[index]]
    B<- tilde.theta.y*(exp(alpha.y*treelength[index])-1) - (alpha.y*tilde.theta.y/(alpha.y-alpha.x))*(exp((alpha.y-alpha.x)*treelength[index]) -1)
    vs<-sigmasq.theta*alpha.y^2*exp(2*alpha.y*treelength[index])*(1-exp(-2*alpha.x*treelength[index]))/ (2*alpha.x)
    C<-pnorm(treelength[index],mean=0,sd=sqrt(vs))- 0.5#integral starts from 0
    INTtime<- A+B+C
    INT1<-exp(-alpha.y*treelength[index])*INTtime
    
    ###
    fexpr<-expression(exp(alpha.y*t)*w)
    res<-st.int(fexpr,type="ito",M=1,lower=0,upper=treelength[index])
    INT2<-tau*exp(-alpha.y*treelength[index])*median(res$X)
    
    ynodestates[des[index]]<-ynodestates[anc[index]] + INT1 + INT2
  }
  
  simtrait<-ynodestates[1:n]
  return(list(y=simtrait,x1=x1nodestates[1:n],x2=x2nodestates[1:n]))
  #return(c(mean(simtrait),sd(simtrait)))
}

ououbmprior<-function(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params){
  alpha.y.rate <-prior.model.params["alpha.y.rate"]
  alpha.x.rate <- prior.model.params["alpha.x.rate"]
  theta.x.mean <-prior.model.params["theta.x.mean"]
  theta.x.sd<-prior.model.params["theta.x.sd"]
  sigmasq.x.shape <-prior.model.params["sigmasq.x.shape"]
  sigmasq.x.scale <-prior.model.params["sigmasq.x.scale"]
  tau.shape <- prior.model.params["tau.shape"]
  tau.scale <- prior.model.params["tau.scale"]

  alpha.y<-rexp(n=1, rate=alpha.y.rate)
  alpha.x<-rexp(n=1,rate=alpha.x.rate)
  theta.x<-rnorm(n=1,mean=theta.x.mean,sd=theta.x.sd)
  sigmasq.x<-rinvgamma(n=1, shape=sigmasq.x.shape, scale=sigmasq.x.scale)
  tau<-rinvgamma(n=1, shape=tau.shape, scale=tau.scale)

  b0.min<-prior.reg.params[1]
  b0.max<-prior.reg.params[2]
  b1.min<-prior.reg.params[3]
  b1.max<-prior.reg.params[4]
  b2.min<-prior.reg.params[5]
  b2.max<-prior.reg.params[6]
  b0<-runif(n=1, min=b0.min, max=b0.max)
  b1<-runif(n=1, min=b1.min, max=b1.max)
  b2<-runif(n=1, min=b2.min, max=b2.max)

  model.params<-c(alpha.y, alpha.x, theta.x, sigmasq.x, tau)
  reg.params<- c(b0, b1, b2)

  return(list(model.params=model.params, reg.params=reg.params))
}

sum.stat<-function(trait=trait,tree=tree){
  names(trait)<-tree$tip.label
  pic.trait<-pic(x=trait,phy=tree)
  return(c(mean(pic.trait),sd(pic.trait),skewness(pic.trait),kurtosis(pic.trait),phylosig(tree,x=trait,method = "K",test=T)$K,phylosig(tree,x=trait,method = "lambda",test=T)$lambda))
}


# main
n<-64
tree<-compute.brlen(stree(n,type="balanced"))
plot(tree)
tree<-reorder(tree,"postorder")


root<-0
true.alpha.y<-0.2
true.alpha.x<-0.125
true.theta.x<-0
true.sigmasq.x<-2
true.tau<- 1
#
true.b0 <- 0
true.b1 <- 1
true.b2 <- 0.2

# hyper parameters
alpha.y.rate <- 5
alpha.x.rate <- 8
theta.x.mean <- 0
theta.x.sd  <- 1
sigma.sq.x.shape <-2
sigma.sq.x.scale <-1
tau.shape <- 2
tau.scale <- 1
b0.min = -2
b0.max = 2
b1.min = -2
b1.max = 2
b2.min = -2
b2.max = 2


prior.model.params=c(alpha.y.rate, alpha.x.rate, theta.x.mean, theta.x.sd, sigma.sq.x.shape, sigma.sq.x.scale, tau.shape, tau.scale)
names(prior.model.params)<-c("alpha.y.rate", "alpha.x.rate", "theta.x.mean","theta.x.sd","sigmasq.x.shape", "sigmasq.x.scale", "tau.shape", "tau.scale")
prior.reg.params=c(b0.min, b0.max, b1.min, b1.max, b2.min, b2.max)
prior.params <- ououbmprior(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params)

true.trait<-ououbmmodel(model.params=c(true.alpha.y, true.alpha.x, true.theta.x, true.sigmasq.x, true.tau),reg.params=c(true.b0,true.b1,true.b2),root=root,tree=tree)

raw.sum.stat.y<-sum.stat(trait=true.trait$y,tree=tree)
raw.sum.stat.x1<-sum.stat(trait=true.trait$x1,tree=tree)
raw.sum.stat.x2<-sum.stat(trait=true.trait$x2,tree=tree)

sims<-1000
tolrate<-0.1
sim.ououbm.trait<-array(0,c(n,5,sims))
model.params.array<-array(0,c(5,sims))
rownames(model.params.array)<-c("alpha.y","alpha.x","theta.x","sigmasq.x","tau")
reg.params.array<-array(0,c(3,sims))
row.names(reg.params.array)<-c("b0", "b1", "b2")
y.sum.stat.array<-array(0,c(6,sims))
rownames(y.sum.stat.array)<-c("y.mean","y.sd","y.skewness","y.kurtosis","y.K","y.lambda")
x1.sum.stat.array<-array(0,c(6,sims))
rownames(x1.sum.stat.array)<-c("x1.mean","x1.sd","x1.skewness","x1.kurtosis","x1.K","x1.lambda")
x2.sum.stat.array<-array(0,c(6,sims))
rownames(x2.sum.stat.array)<-c("x2.mean","x2.sd","x2.skewness","x2.kurtosis","x2.K","x2.lambda")
post.model.params.array<-array(0,c(5,sims))
rownames(post.model.params.array)<-c("alpha.y","alpha.x","theta.x","sigma.sq.x","tau")

for(simIndex in 1:sims){
  if(simIndex %%  100 ==0){print(simIndex)}
#  simIndex<-1
  prior.params <- ououbmprior(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params)
  model.params.array[,simIndex]<-prior.params$model.params#for record only
  reg.params.array[,simIndex]<-prior.params$reg.params#for record only
#
  sim.trait <-ououbmmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.ououbm.trait[,1,simIndex]<-sim.trait$y
  sim.ououbm.trait[,2,simIndex]<-sim.trait$x1
  sim.ououbm.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
  }#end of loop
#

sim.sum.stat <- cbind(t(y.sum.stat.array),t(x1.sum.stat.array),t(x2.sum.stat.array))
ououbm.par.sim <- cbind(t(model.params.array),t(reg.params.array))

### The rejection alogoritm
rej <- abc(target=c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2), param=ououbm.par.sim, sumstat=sim.sum.stat, tol=tolrate, method="rejection"  )
post.ououbm <- as.data.frame(rej$unadj.values)

save.image(paste("ououbmsimV2size",n,".RData",sep=""))




#Credible Interval for alpha.x
# postTheta<-data.frame(rej$unadj.values)
# postTheta$alpha.x
# rep<-50
# post_alpha.x<-sample(postTheta$alpha.x,rep,replace=FALSE)
# sample_alpha.x<-matrix(NA,rep,sims*tolrate)
# for(repIndex in 1:rep){
#  # repIndex<-1
#   print(paste("repIndex",repIndex,sep=""))
#   true.tau<- post_alpha.x[repIndex]
#   true.trait<-ououbmmodel(model.params=c(true.alpha.y, true.alpha.x, true.theta.x, true.sigma.sq.x, true.tau),reg.params=c(true.b0,true.b1,true.b2),root=root,tree=tree)
# 
#   raw.sum.stat.y<-sum.stat(trait=true.trait$y,tree=tree)
#   raw.sum.stat.x1<-sum.stat(trait=true.trait$x1,tree=tree)
#   raw.sum.stat.x2<-sum.stat(trait=true.trait$x2,tree=tree)
# 
#   for(simIndex in 1:sims){
#     if(simIndex %% 100 == 0){print(simIndex)}
# 
#     sim.alpha.x<-rexp(n=1,rate=alpha.x.rate)
#     model.params<-c(true.alpha.y, sim.alpha.x, true.theta.x, true.sigma.sq.x, true.tau)
#     reg.params<-c(true.b0,true.b1,true.b2)
#     model.params.array[,simIndex]<-model.params
#     reg.params.array[,simIndex]<-reg.params
#     sim.trait<-ououbmmodel(model.params=model.params,reg.params= reg.params,root=root,tree=tree)
#     sim.ououbm.trait[,1,simIndex]<-sim.trait$y
#     sim.ououbm.trait[,2,simIndex]<-sim.trait$x1
#     sim.ououbm.trait[,3,simIndex]<-sim.trait$x2
#     y.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
#     x1.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
#     x2.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
#   }
#   sim.sum.stat <- cbind(t(y.sum.stat.array),t(x1.sum.stat.array),t(x2.sum.stat.array))
#   head(sim.sum.stat)
#   ououbm.par.sim <- cbind(t(model.params.array),t(reg.params.array))
#   ououbm.post.sim <- cbind(t(post.model.params.array),t(post.reg.params.array))
# 
#   rej <- abc(target=c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2), param=ououbm.par.sim, sumstat=sim.sum.stat, tol=tolrate, method="rejection"  )
# 
# 
#   postTheta_sim<-data.frame(rej$unadj.values)
#   sample_alpha.x[repIndex,]<-postTheta_sim$alpha.x
#   }
# 
# summary(sample_alpha.x)
# dim(sample_alpha.x)
# 
# sample_alpha.x_median<-apply(sample_alpha.x,1,median)
# png("alpha.x64.png")
# 
# hist(postTheta$alpha.x,main = paste("OUOUBM ",expression(alpha.x),sep=""),breaks=5,freq=FALSE,cex.main=3.5,xlab=expression(alpha),ylab="density",col="lightgreen",ylim=c(0,4))
# lines(density(postTheta$alpha.x))
# 
# min.ylab<-min(density(postTheta$alpha.x)$y)
# max.ylab<-max(density(postTheta$alpha.x)$y)
# 
# abline(v=quantile(postTheta$alpha.x,prob=c(0.025,0.975)),col="red",lty=2)
# points(post_alpha.x,seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.7)
# k<-(sample_alpha.x_median>quantile(postTheta$alpha.x,prob=0.975))+(sample_alpha.x_median<quantile(postTheta$alpha.x,prob=0.025))
# points(sample_alpha.x_median,seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.7,col=c("blue","red")[1+k])
# segments(sample_alpha.x_median,seq(min.ylab,max.ylab,length=rep),post_alpha.x,seq(min.ylab,max.ylab,length=rep),col="grey")
# dev.off()
# save.image("ououbm_bci_alpha.x64.RData")

