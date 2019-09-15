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
library(phylobase)
library(phytools)

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmbmabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmcirabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououbmabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououcirabcv2.r")

source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmbmabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmcirabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououbmabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououcirabcv2.r")

OUprior<-function(tree=tree,trait=trait,model=model){
  ou.result<-geiger::fitContinuous(tree,trait,model=model)
  alpha <- ou.result$opt$alpha#rexp(n=1, rate=1/alpha.prior.mean)
  if(model=="OU"){
      if(alpha < 1e-5){alpha <-0.001}
    }
  theta <- ou.result$opt$z0#rnorm(n=1, mean=theta.prior.mean, sd=theta.prior.sd)
  sigmasq<-ou.result$opt$sigsq#rexp(n=1, rate=1/sigmasq.prior.mean)
  return(list(alpha=alpha,theta=theta,sigmasq=sigmasq,root=theta))
}


sum.stat<-function(trait=trait,tree=tree){
  names(trait)<-tree$tip.label
  pic.trait<-pic(x=trait,phy=tree)
  return(c(mean(trait),sd(trait),median(trait),skewness(trait),kurtosis(trait),mean(pic.trait),sd(pic.trait),median(pic.trait),skewness(pic.trait),kurtosis(pic.trait),phylosig(tree,x=trait,method = "K",test=T)$K,phylosig(tree,x=trait,method = "lambda",test=T)$lambda))
}

regboundfcn<-function(olssum=olssum){
  bdd<-  c(olssum$coefficients[,1]-3*olssum$coefficients[,2],olssum$coefficients[,1]+3*olssum$coefficients[,2])
  bdd<-bdd[c(1,4,2,5,3,6)]  
  return(bdd)
}


raw.sum.stat.y <- sum.stat(trait = resptrait, tree=tree)
raw.sum.stat.x1 <- sum.stat(trait = predtrait1, tree=tree)
raw.sum.stat.x2 <- sum.stat(trait = predtrait2, tree=tree)
raw.sum.stat<-c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2)

y.ou.result <- OUprior(tree=tree,trait=resptrait,model="OU")
x1.ou.result<- OUprior(tree=tree,trait=predtrait1,model="OU")
x2.ou.result <- OUprior(tree=tree,trait=predtrait2,model="OU")
x1.bm.result<- OUprior(tree=tree,trait=predtrait1,model="BM")
x2.bm.result <- OUprior(tree=tree,trait=predtrait2,model="BM")

root<-list(y.ou.sigmsq=y.ou.result$sigmasq ,y.ou.root=y.ou.result$root, x1.ou.root=x1.ou.result$root, x2.ou.root=x2.ou.result$root,x1.bm.root=x1.bm.result$root, x2.bm.root=x2.bm.result$root )# Here we have root


# In Empirical Analysis: use uniform distribution may be OK
##### HERE WE USE UNIFORM PRIOR FOR EMPIRICAL ANALYSIS
# Hyperparameter

####
#### HERE WE NEED x1.root, x2.root, y.root.root, sigmasq, optimum.root,  current we only set y.root need modification
####

alpha.y.min <-  0 
alpha.y.max <-  3*y.ou.result$alpha

alpha.x.min <-  0 
alpha.x.max <-  3*mean(x1.ou.result$alpha,x2.ou.result$alpha)#0.126#3*mean(x1.ou.result$alpha,x2.ou.result$alpha)

theta.x.min <-  -3*abs(mean(x1.ou.result$theta,x2.ou.result$theta))#-0.01#-3*abs(mean(x1.ou.result$theta,x2.ou.result$theta))
theta.x.max <-   3*abs(mean(x1.ou.result$theta,x2.ou.result$theta))#0.01#3*abs(mean(x1.ou.result$theta,x2.ou.result$theta))

tau.min <-       0
tau.max <-       3*mean(y.ou.result$sigma,x1.ou.result$sigma,x2.ou.result$sigma)

sigmasq.x.min <- 0
sigmasq.x.max <- 3*mean(x1.ou.result$sigmasq,x2.ou.result$sigmasq)

alpha.tau.min <- 0
alpha.tau.max <- 3*mean(y.ou.result$alpha, x1.ou.result$alpha,x2.ou.result$alpha)

theta.tau.min <- 0 
theta.tau.max <- 3*mean(abs(y.ou.result$theta), abs(x1.ou.result$theta),abs(x2.ou.result$theta))

sigmasq.tau.min <- 0 
sigmasq.tau.max <- 3*mean(y.ou.result$sigmasq,x1.ou.result$sigmasq,x2.ou.result$sigmasq)
  
# For regression
olssum <- summary(lm(resptrait~predtrait1+predtrait2))
regbound<-regboundfcn(olssum=olssum)

b0.min=regbound[1]
b0.max=regbound[2]
b1.min=regbound[3]
b1.max=regbound[4]
b2.min=regbound[5]
b2.max=regbound[6]

tol<-0.1
sims <- 50000

prior.model.params<-c(alpha.y.min,alpha.y.max,alpha.x.min,alpha.x.max,theta.x.min,theta.x.max,tau.min,tau.max,sigmasq.x.min,sigmasq.x.max,alpha.tau.min,alpha.tau.max,theta.tau.min,theta.tau.max,sigmasq.tau.min,sigmasq.tau.max)
names(prior.model.params)<-c("alpha.y.min","alpha.y.max","alpha.x.min","alpha.x.max","theta.x.min","theta.x.max","tau.min","tau.max","sigmasq.x.min","sigmasq.x.max","alpha.tau.min","alpha.tau.max","theta.tau.min","theta.tau.max","sigmasq.tau.min","sigmasq.tau.max")
prior.reg.params=c(b0.min, b0.max, b1.min, b1.max, b2.min, b2.max)
###############
### oubmbm ###
##############

prior.params.oubmbm <- oubmbmprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
sim.oubmbm.trait<-array(NA,c(dim(dataset)[1],3,sims))
model.params.oubmbm<-array(NA,c(3,sims))
rownames(model.params.oubmbm)<-c("alpha.y","sigmasq.x","tau")
reg.params.oubmbm<-array(NA,c(3,sims))
row.names(reg.params.oubmbm)<-c("b0", "b1", "b2")
y.sum.stat.oubmbm<-array(NA,c(12,sims))
rownames(y.sum.stat.oubmbm)<-c("y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda")
x1.sum.stat.oubmbm<-array(NA,c(12,sims))
rownames(x1.sum.stat.oubmbm)<-c("x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda")
x2.sum.stat.oubmbm<-array(NA,c(12,sims))
rownames(x2.sum.stat.oubmbm)<-c("x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")

for(simIndex in 1:sims){
  if(simIndex %%100==0){print(paste("oubmbm",simIndex,sep=""))}
  #print(paste("oubmbm simIndex= ",simIndex,sep=""))
  prior.params<-oubmbmprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
  model.params.oubmbm[,simIndex]<-prior.params$model.params#for record only
  reg.params.oubmbm[,simIndex]<-prior.params$reg.params#for record only
  
  sim.trait <-oubmbmmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.oubmbm.trait[,1,simIndex]<-sim.trait$y
  sim.oubmbm.trait[,2,simIndex]<-sim.trait$x1
  sim.oubmbm.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.oubmbm[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.oubmbm[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.oubmbm[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
}# end of loop

sum.stat.oubmbm <- cbind(t(y.sum.stat.oubmbm),t(x1.sum.stat.oubmbm),t(x2.sum.stat.oubmbm))
oubmbm.par.sim <- cbind(t(model.params.oubmbm),t(reg.params.oubmbm))
###############
### ououbm ###
##############
prior.params.ououbm <- ououbmprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
sim.ououbm.trait<-array(NA,c(dim(dataset)[1],5,sims))
model.params.ououbm<-array(NA,c(5,sims))
rownames(model.params.ououbm)<-c("alpha.y","alpha.x","theta.x","sigmasq.x","tau")
reg.params.ououbm<-array(NA,c(3,sims))
row.names(reg.params.ououbm)<-c("b0", "b1", "b2")
y.sum.stat.ououbm<-array(NA,c(12,sims))
rownames(y.sum.stat.ououbm)<-c("y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda")
x1.sum.stat.ououbm<-array(NA,c(12,sims))
rownames(x1.sum.stat.ououbm)<-c("x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda")
x2.sum.stat.ououbm<-array(NA,c(12,sims))
rownames(x2.sum.stat.ououbm)<-c("x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")
# rownames(post.model.params.ououbm)<-c("alpha.y","alpha.x","theta.x","sigma.x","tau")
# row.names(post.reg.params.ououbm)<-c("b0", "b1", "b2")
for(simIndex in 1:sims){
  if(simIndex %%100==0){print(paste("ououbm",simIndex,sep=""))}
  #simIndex<-1
  #print(paste("ououbm simIndex= ",simIndex,sep=""))
  prior.params <- ououbmprior(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params)
  model.params.ououbm[,simIndex]<-prior.params$model.params#for record only
  reg.params.ououbm[,simIndex]<-prior.params$reg.params#for record only
  
  sim.trait <-ououbmmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.ououbm.trait[,1,simIndex]<-sim.trait$y
  sim.ououbm.trait[,2,simIndex]<-sim.trait$x1
  sim.ououbm.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.ououbm[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.ououbm[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.ououbm[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
}#end of loop
sum.stat.ououbm <- cbind(t(y.sum.stat.ououbm),t(x1.sum.stat.ououbm),t(x2.sum.stat.ououbm))
ououbm.par.sim <- cbind(t(model.params.ououbm),t(reg.params.ououbm))

###############
### oubmcir ###
##############
prior.params.oubmcir <- oubmcirprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
sim.oubmcir.trait<-array(NA,c(dim(dataset)[1],3,sims))
model.params.oubmcir<-array(NA,c(5,sims))
rownames(model.params.oubmcir)<-c("alpha.y","sigmasq.x","alpha.tau","theta.tau","sigmasq.tau")
reg.params.oubmcir<-array(NA,c(3,sims))
row.names(reg.params.oubmcir)<-c("b0", "b1", "b2")
y.sum.stat.oubmcir<-array(NA,c(12,sims))
rownames(y.sum.stat.oubmcir)<-c("y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda")
x1.sum.stat.oubmcir<-array(NA,c(12,sims))
rownames(x1.sum.stat.oubmcir)<-c("x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda")
x2.sum.stat.oubmcir<-array(NA,c(12,sims))
rownames(x2.sum.stat.oubmcir)<-c("x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")

for(simIndex in 1:sims){
  if(simIndex %%100==0){print(paste("oubmcir",simIndex,sep=""))}

  prior.params<-oubmcirprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
  model.params.oubmcir[,simIndex]<-prior.params$model.params#for record only
  reg.params.oubmcir[,simIndex]<-prior.params$reg.params#for record only
  
  sim.trait <-oubmcirmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.oubmcir.trait[,1,simIndex]<-sim.trait$y
  sim.oubmcir.trait[,2,simIndex]<-sim.trait$x1
  sim.oubmcir.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.oubmcir[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.oubmcir[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.oubmcir[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
}# end of loop

sum.stat.oubmcir <- cbind(t(y.sum.stat.oubmcir),t(x1.sum.stat.oubmcir),t(x2.sum.stat.oubmcir))
oubmcir.par.sim <- cbind(t(model.params.oubmcir),t(reg.params.oubmcir))

###############
### ououcir ###
##############
prior.params.ououcir <- ououcirprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
sim.ououcir.trait<-array(NA,c(dim(dataset)[1],3,sims))
model.params.ououcir<-array(NA,c(7,sims))
rownames(model.params.ououcir)<-c("alpha.y", "alpha.x", "theta.x", "sigmasq.x","alpha.tau","theta.tau","sigmasq.tau")
reg.params.ououcir<-array(NA,c(3,sims))
row.names(reg.params.ououcir)<-c("b0", "b1", "b2")
y.sum.stat.ououcir<-array(NA,c(12,sims))
rownames(y.sum.stat.ououcir)<-c("y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda")
x1.sum.stat.ououcir<-array(NA,c(12,sims))
rownames(x1.sum.stat.ououcir)<-c("x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda")
x2.sum.stat.ououcir<-array(NA,c(12,sims))
rownames(x2.sum.stat.ououcir)<-c("x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")

for(simIndex in 1:sims){
  if(simIndex %%100==0){print(paste("oubmcir",simIndex,sep=""))}
  prior.params<-ououcirprior(prior.model.params = prior.model.params, prior.reg.params = prior.reg.params)
  model.params.ououcir[,simIndex]<-prior.params$model.params#for record only
  reg.params.ououcir[,simIndex]<-prior.params$reg.params#for record only
  
  sim.trait <-ououcirmodel(model.params=prior.params$model.params,reg.params=prior.params$reg.params,root=root,tree=tree)
  sim.ououcir.trait[,1,simIndex]<-sim.trait$y
  sim.ououcir.trait[,2,simIndex]<-sim.trait$x1
  sim.ououcir.trait[,3,simIndex]<-sim.trait$x2
  y.sum.stat.ououcir[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
  x1.sum.stat.ououcir[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
  x2.sum.stat.ououcir[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
}# end of loop

sum.stat.ououcir <- cbind(t(y.sum.stat.ououcir),t(x1.sum.stat.ououcir),t(x2.sum.stat.ououcir))
ououcir.par.sim <- cbind(t(model.params.ououcir),t(reg.params.ououcir))


###############
### Analysis ##
###############
models<-rep(c("oubmbm","ououbm","oubmcir","ououcir"), each=sims)
full.sum.stat <- rbind(sum.stat.oubmbm, sum.stat.ououbm, sum.stat.oubmcir, sum.stat.ououcir)

#################
### rejection ###
#################

### posterior
rej.result.oubmbm <- abc(target = raw.sum.stat,param = data.frame(oubmbm.par.sim), sumstat = sum.stat.oubmbm,tol = tol,method="rejection")
rej.result.ououbm <- abc(target = raw.sum.stat,param = data.frame(ououbm.par.sim), sumstat = sum.stat.ououbm,tol = tol,method="rejection")
rej.result.oubmcir <- abc(target = raw.sum.stat,param = data.frame(oubmcir.par.sim), sumstat = sum.stat.oubmcir,tol = tol,method="rejection")
rej.result.ououcir <- abc(target = raw.sum.stat,param = data.frame(ououcir.par.sim), sumstat = sum.stat.ououcir,tol = tol,method="rejection")

post.oubmbm <- as.data.frame(rej.result.oubmbm$unadj.values)
post.ououbm <- as.data.frame(rej.result.ououbm$unadj.values)
post.oubmcir <- as.data.frame(rej.result.oubmcir$unadj.values)
post.ououcir <- as.data.frame(rej.result.ououcir$unadj.values)

#######################
### model selection ###
#######################
#modsel <- postpr(target=raw.sum.stat,models,full.sum.stat, tol=tol,method="rejection")
#print(summary(modsel))
modsel.mnlog<-postpr(target=raw.sum.stat,models,full.sum.stat, tol=tol,method="mnlogistic")
print(summary(modsel.mnlog))
#modsel.neuralnet<-postpr(target=raw.sum.stat,models,full.sum.stat, tol=tol,method="neuralnet")
#summary(modsel.neuralnet)

