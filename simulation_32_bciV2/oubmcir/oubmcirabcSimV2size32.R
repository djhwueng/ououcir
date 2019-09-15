#setwd("~/Dropbox/ChihPingWang/Model1/abc_V2/simulation64V2/oubmcir/")

setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation64V2/oubmcir/")


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

oubmcirmodel<-function(model.params,reg.params,root=root,tree=tree){
  alpha.y<-model.params[1]
  sigmasq.x<-model.params[2]
  alpha.tau<-model.params[3]
  theta.tau<-model.params[4]
  sigmasq.tau<-model.params[5]
  b0<-reg.params[1]
  b1<-reg.params[2]
  b2<-reg.params[3]

  n<-Ntip(tree)
  x1nodestates<-array(NA,c(2*n-1))
  x1nodestates[n+1]<- root$x1.bm.root
  x2nodestates<-array(NA,c(2*n-1))
  x2nodestates[n+1]<- root$x2.bm.root
  optimnodestates<-array(NA,c(2*n-1))
  optimnodestates[n+1]<-  b0 + b1*root$x1.bm.root + b2*root$x2.bm.root
  sigmasqnodestates<-array(NA,c(2*n-1))
  sigmasqnodestates[n+1]<- root$y.ou.sigmsq
  ynodestates<-array(NA,c(2*n-1))
  ynodestates[n+1]<-root$y.ou.root

  N<-dim(tree$edge)[1]
  anc<-tree$edge[,1]
  des<-tree$edge[,2]
  treelength<-tree$edge.length
  for(index in N:1){

    x1nodestates[des[index]]<-rnorm(n=1,mean=x1nodestates[anc[index]],sd= sqrt(sigmasq.x*treelength[index]))
    x2nodestates[des[index]]<-rnorm(n=1,mean=x2nodestates[anc[index]],sd= sqrt(sigmasq.x*treelength[index]))

    optimnodestates[des[index]]<- b0 + b1*x1nodestates[des[index]] + b2*x2nodestates[des[index]]
    sigmasq.theta<- b1^2*sigmasq.x + b2^2*sigmasq.x

    c<- sigmasq.tau*(1-exp(-alpha.tau*treelength[index]))/(4*alpha.tau)
    k<- (4*theta.tau*alpha.tau)/sigmasq.tau
    tau0<-sigmasqnodestates[anc[index]]
    lambda<-4*alpha.tau*exp(-alpha.tau*treelength[index])
    lambda<-lambda/(sigmasq.tau*(1-exp(-alpha.tau*treelength[index])))*tau0
    tmp = rchisq(n=1, df=k, ncp = lambda)
    sig_u <- c*tmp
    sigmasqnodestates[des[index]]<-sig_u

    ###
    INT1var<-sigmasq.theta*(exp(2*alpha.y*treelength[index])-1)/(2*alpha.y)
    INT1<-exp(-alpha.y*treelength[index])* rnorm(n=1,mean=0,sd=sqrt(INT1var))

    ###
    a.var<-(1-exp(-2*alpha.y*treelength[index]))/(2*alpha.y)
    a <- rnorm(n=1, mean=0, sd=sqrt(a.var))
    b.var<- (sigmasqnodestates[anc[index]]-theta.tau)^2/(2*(alpha.y-alpha.tau))
    b.var<-b.var*(exp(-2*alpha.tau*treelength[index])-exp(-2*alpha.y*treelength[index]))
    b <- rnorm(n=1, mean=0, sd=sqrt(b.var))

    n_t <- 1
    n_s <- 1
    outer.int.sum=0
    for(outer.index in 1:n_t){
      inner.int.sum = 0
      for(inner.index in 1:n_s){
        c<- sigmasq.tau*(1-exp(-alpha.tau*(inner.index/n_s)))/(4*alpha.tau)
        k<- (4*theta.tau*alpha.tau)/sigmasq.tau
        lambda<- 4*alpha.tau*exp(-alpha.tau*(inner.index/n_s))
        tau0<-sigmasqnodestates[anc[index]]
        lambda<-lambda/(sigmasq.tau*(1-exp(-alpha.tau*(inner.index/n_s))))*tau0
        tmp = rchisq(n=1, df=k, ncp = lambda)
        sig_u <- c*tmp
        inner.int.sum  <-  inner.int.sum + exp(alpha.tau*(inner.index/n_s))*rnorm(n=1,mean=0, sd=sqrt(1/n_s))*sqrt(sig_u)
      }
      outer.int.sum <- outer.int.sum + exp(-(alpha.y+alpha.tau)*(outer.index/n_t))*inner.int.sum*rnorm(n=1,mean=0, sd=sqrt(1/n_t))
    }
    c <- sqrt(sigmasq.tau)*outer.int.sum
    INT2 <- (a + b + c)
    ynodestates[des[index]]<-ynodestates[anc[index]] + INT1 + INT2
  }
  simtrait<-ynodestates[1:n]
  return(list(y=simtrait,x1=x1nodestates[1:n],x2=x2nodestates[1:n]))
}

oubmcirprior<-function(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params){
  alpha.y.rate <-prior.model.params["alpha.y.rate"]# assume exponential
  sigmasq.x.shape <-prior.model.params["sigmasq.x.shape"]# assume invgamma
  sigmasq.x.scale <-prior.model.params["sigmasq.x.scale"]
  alpha.tau.rate <- prior.model.params["alpha.tau.rate"]# assume exponential
  theta.tau.mean <- prior.model.params["theta.tau.mean"] #assume LogNormal
  theta.tau.sd <- prior.model.params["theta.tau.sd"]
  sigmasq.tau.shape <- prior.model.params["sigmasq.tau.shape"]# assume inv gamma
  sigmasq.tau.scale <- prior.model.params["sigmasq.tau.scale"]

  alpha.y<-rexp(n=1, rate=alpha.y.rate)
  sigmasq.x<-rinvgamma(n=1, shape=sigmasq.x.shape, scale=sigmasq.x.scale)
  alpha.tau <- rexp(n=1, rate=alpha.tau.rate)
  theta.tau <-  rlnorm(n=1, meanlog=theta.tau.mean, sdlog=theta.tau.sd)
  sigmasq.tau<-rinvgamma(n=1, shape=sigmasq.tau.shape, scale=sigmasq.tau.scale)

  b0.min<-prior.reg.params[1]
  b0.max<-prior.reg.params[2]
  b1.min<-prior.reg.params[3]
  b1.max<-prior.reg.params[4]
  b2.min<-prior.reg.params[5]
  b2.max<-prior.reg.params[6]
  b0<-runif(n=1, min=b0.min, max=b0.max)
  b1<-runif(n=1, min=b1.min, max=b1.max)
  b2<-runif(n=1, min=b2.min, max=b2.max)

  model.params<-c(alpha.y, sigmasq.x, alpha.tau, theta.tau, sigmasq.tau)
  reg.params<- c(b0, b1, b2)

  return(list(model.params=model.params, reg.params=reg.params))
}

sum.stat<-function(trait=trait,tree=tree){
  names(trait)<-tree$tip.label
  pic.trait<-pic(x=trait,phy=tree)
  return(c(mean(trait),sd(trait),median(trait),skewness(trait),kurtosis(trait),mean(pic.trait),sd(pic.trait),median(pic.trait),skewness(pic.trait),kurtosis(pic.trait),phylosig(tree,x=trait,method = "K",test=T)$K,phylosig(tree,x=trait,method = "lambda",test=T)$lambda))
}

### main
n<-32 # we actually need to do 10, 30, 50, 100 for good
tree<-compute.brlen(stree(n,type = "balanced"))
tree<-reorder(tree,"postorder")
plot(tree)
nodelabels()
tiplabels()


root<-list(y.ou.sigmsq=1 ,y.ou.root=0, x1.ou.root=0, x2.ou.root=0,x1.bm.root=0, x2.bm.root=0)# Here we have root
true.alpha.y<-0.2
true.sigma.sq.x<-2
true.alpha.tau<-0.25
true.theta.tau<-0.5
true.sigmasq.tau<- 1

true.b0 <- 0
true.b1 <- 1
true.b2 <- 0.2

# hyper paramters
alpha.y.rate <-5# assume exponential
sigmasq.x.shape <-2# assume invgamma
sigmasq.x.scale <-2
alpha.tau.rate <- 4# assume exponential
theta.tau.mean <- 0.5 #assume LogNormal
theta.tau.sd <- 1
sigmasq.tau.shape <- 2 # assume inv gamma
sigmasq.tau.scale <- 1
b0.min=-2
b0.max=2
b1.min=-2
b1.max=2
b2.min=-2
b2.max=2

prior.model.params=c(alpha.y.rate, sigmasq.x.shape, sigmasq.x.scale, alpha.tau.rate, theta.tau.mean, theta.tau.sd, sigmasq.tau.shape, sigmasq.tau.scale)
names(prior.model.params)<-c("alpha.y.rate", "sigmasq.x.shape", "sigmasq.x.scale", "alpha.tau.rate", "theta.tau.mean", "theta.tau.sd", "sigmasq.tau.shape", "sigmasq.tau.scale")
prior.reg.params=c(b0.min, b0.max, b1.min, b1.max, b2.min, b2.max)
prior.params <- oubmcirprior(prior.model.params = prior.model.params, prior.reg.params=prior.reg.params)

true.trait <- oubmcirmodel(model.params = c(true.alpha.y, true.sigma.sq.x, true.alpha.tau, true.theta.tau, true.sigmasq.tau),reg.params = c(true.b0,true.b1,true.b2), root=root, tree=tree)
sim.trait <- oubmcirmodel(model.params = prior.params$model.params, reg.params = prior.params$reg.params, root=root, tree=tree)

raw.sum.stat.y <- sum.stat(trait = true.trait$y, tree=tree)
raw.sum.stat.x1 <- sum.stat(trait = true.trait$x1, tree=tree)
raw.sum.stat.x2 <- sum.stat(trait = true.trait$x2, tree=tree)

sims<-10000
tol<-0.1
sim.oubmcir.trait<-array(0,c(n,3,sims))
model.params.array<-array(0,c(5,sims))
rownames(model.params.array)<-c("alpha.y","sigmasq.x","alpha.tau","theta.tau","sigmasq.tau")
reg.params.array<-array(0,c(3,sims))
row.names(reg.params.array)<-c("b0", "b1", "b2")
y.sum.stat.array<-array(0,c(6,sims))
rownames(y.sum.stat.array)<-c("y.mean","y.sd","y.skewness","y.kurtosis","y.K","y.lamda")
x1.sum.stat.array<-array(0,c(6,sims))
rownames(x1.sum.stat.array)<-c("x1.mean","x1.sd","x1.skewness","x1.kurtosis","x1.K","x1.lambda")
x2.sum.stat.array<-array(0,c(6,sims))
rownames(x2.sum.stat.array)<-c("x2.mean","x2.sd","x2.skewness","x2.kurtosis","x2.K","x2.lambda")

for(simIndex in 1:sims){
  if(simIndex %%10==0){print(simIndex)}
  prior.params<-oubmcirprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
  model.params.array[,simIndex]<-prior.params$model.params#for record only
  reg.params.array[,simIndex]<-prior.params$reg.params#for record only

  sim.trait <-oubmcirmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.oubmcir.trait[,1,simIndex]<-sim.trait$y
  sim.oubmcir.trait[,2,simIndex]<-sim.trait$x1
  sim.oubmcir.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
}# end of loop

sim.sum.stat <- cbind(t(y.sum.stat.array),t(x1.sum.stat.array),t(x2.sum.stat.array))
oubmcir.par.sim <- cbind(t(model.params.array),t(reg.params.array))

rej <- abc(target=c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2), param=oubmcir.par.sim, sumstat=sim.sum.stat, tol=tol, method="rejection"  )
post.oubmcir <- as.data.frame(rej$unadj.values)

save.image(paste("oubmcirsimV2size",n,".RData",sep=""))

### Bayesian Credible Interval for alpha.y
# postTheta<-data.frame(rej$unadj.values)
# rep<-50
# post_sigma.sq.x<-sample(postTheta$sigma.sq.x,rep,replace = FALSE)
# sample_sigma.sq.x<-matrix(NA,rep,sims*tolrate)
# dim(sample_sigma.sq.x)
#
# for(repIndex in 1:rep){
#   #true data
#   #parameter
#   #repIndex<-1
#
#   true.sigma.sq.x<-post_sigma.sq.x[repIndex]
#   true.trait<-oubmcirmodel(model.params = c(true.alpha.y, true.sigma.sq.x, true.alpha.tau, true.theta.tau, true.sigma.tau),reg.params = c(true.b0,true.b1,true.b2), root=root, tree=tree)
#
#   raw.sum.stat.y <- sum.stat(trait = true.trait$y, tree=tree)
#   raw.sum.stat.x1 <- sum.stat(trait = true.trait$x1, tree=tree)
#   raw.sum.stat.x2 <- sum.stat(trait = true.trait$x2, tree=tree)
#
#   for(simIndex in 1:sims){
#     if(simIndex %%10==0){print(paste("repIndex= ",repIndex," simIndex= ",simIndex,sep=""))}
#
#     sim.sigma.sq.x<-rinvgamma(n=1,shape=sigma.sq.x.shape,scale=sigma.sq.x.scale)
#
#     model.params<-c(true.alpha.y, sim.sigma.sq.x, true.alpha.tau, true.theta.tau, true.sigma.tau)
#     reg.params<-c(true.b0,true.b1,true.b2)
#     model.params.array[,simIndex]<-model.params#for record only
#     reg.params.array[,simIndex]<-reg.params
#     sim.trait<-oubmcirmodel(model.params = model.params,reg.params = reg.params, root=root, tree=tree)
#     sim.oubmcir.trait[,1,simIndex]<-sim.trait$y
#     sim.oubmcir.trait[,2,simIndex]<-sim.trait$x1
#     sim.oubmcir.trait[,3,simIndex]<-sim.trait$x2
#     y.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
#     x1.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
#     x2.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
#   }
#   sim.sum.stat <- cbind(t(y.sum.stat.array),t(x1.sum.stat.array),t(x2.sum.stat.array))
#   oubmcir.par.sim <- cbind(t(model.params.array),t(reg.params.array))
#   rej <- abc(target=c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2), param=oubmcir.par.sim, sumstat=sim.sum.stat, tol=0.05, method="rejection"  )
#   postTheta_sim<-data.frame(rej$unadj.values)
#   sample_sigma.sq.x[repIndex,]<-postTheta_sim$sigma.sq.x
# }
# #dim(sample_sigma.sq.x)
# sample_sigma.sq.x_median<-apply(sample_sigma.sq.x,1,median)
#
# # Plot
# png("sigma.sq.x.64.png")
# hist(postTheta$sigma.sq.x,xlim=c(0,max(postTheta$sigma.sq.x)),ylim=c(0,max.ylab),main = paste("OUBMCIR ", expression(sigma.sq.x),sep=""),breaks=5,freq=FALSE,cex.main=2.5,xlab= expression(sigma[x]^2),ylab="Density",cex.lab=1.5, cex.sub=2,col="lightgreen")
# lines(density(postTheta$sigma.sq.x))
# abline(v=quantile(postTheta$sigma.sq.x,probs=c(0.025,0.975)),col="red",lty=2)
#
# min.ylab<-min(density(postTheta$sigma.sq.x)$y)
# max.ylab<-max(density(postTheta$sigma.sq.x)$y)
#
# points(post_sigma.sq.x,seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.7)
#
# k<-(sample_sigma.sq.x_median<quantile(postTheta$sigma.sq.x,probs=0.025))+(sample_sigma.sq.x_median>quantile(postTheta$sigma.sq.x,probs=0.975))
#
# points(sample_sigma.sq.x_median,seq(min.ylab,max.ylab,length=rep),pch=19,cex=0.7,col=c("blue","red")[1+k])
# segments(sample_sigma.sq.x_median,seq(min.ylab,max.ylab,length=rep),post_sigma.sq.x,seq(min.ylab,max.ylab,length=rep),col="grey")
# dev.off()
#
# save.image("oubmcir_bci_sigmasqx64.RData")
