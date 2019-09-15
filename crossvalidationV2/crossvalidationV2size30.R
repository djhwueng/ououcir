#setwd("~/Dropbox/ChihPingWang/Thesis/crossvalidationV2/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Thesis/crossvalidationV2/")

rm(list=ls())

# Package
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

# Model
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmbmabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmcirabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououbmabcv2.r")
#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououcirabcv2.r")

source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmbmabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/oubmcirabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououbmabcv2.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/ououcirabcv2.r")


sum.stat<-function(trait=trait,tree=tree){
  names(trait)<-tree$tip.label
  pic.trait<-pic(x=trait,phy=tree)
  return(c(mean(trait),sd(trait),median(trait),skewness(trait),kurtosis(trait),mean(pic.trait),sd(pic.trait),median(pic.trait),skewness(pic.trait),kurtosis(pic.trait),phylosig(tree,x=trait,method = "K",test=T)$K,phylosig(tree,x=trait,method = "lambda",test=T)$lambda))
}

# Main
reps<-1000
treesize.array<-c(30)
numbsim<-1;lambda<-2.0;mu<-0.5;frac<-0.6;age<-2
#?TreeSim

# Model parameters
root<-list(y.ou.sigmsq=1 ,y.ou.root=0, x1.ou.root=0, x2.ou.root=0,x1.bm.root=0, x2.bm.root=0)# Here we have root

alpha.y <- 0.2
alpha.x <- 0.1
theta.x <- 0.5
sigmasq.x <- 1
tau <- 1
alpha.tau <- 0.25
theta.tau <- 0.15
sigmasq.tau <- 0.5
b0 <- 0
b1 <- 1
b2 <- -1

oubmbm.params <- c(alpha.y,sigmasq.x,tau)
ououbm.params<- c(alpha.y,alpha.x,theta.x,sigmasq.x,tau)
oubmcir.params<-c(alpha.y,sigmasq.x,alpha.tau,theta.tau,sigmasq.tau)
ououcir.params <- c(alpha.y,alpha.x,theta.x,sigmasq.x,alpha.tau,theta.tau,sigmasq.tau)
reg.params<-c(b0,b1,b2)

for(treesizeIndex in 1:length(treesize.array) ){
  #treesizeIndex<-1
  treesize<-treesize.array[treesizeIndex]
  tree<-sim.bd.taxa.age(n=treesize,numbsim=1,lambda=lambda,mu=mu,frac=frac,age=age,mrca=TRUE)[[1]]
  tree<-reorder(tree,"postorder")
  
  oubmbmstat<-matrix(0,reps,36)#12 statistics each for y,x1,x2
  ououbmstat <- matrix(0,reps,36)
  oubmcirstat <- matrix(0,reps,36)
  ououcirstat<-matrix(0,reps,36)
  
  for (repIndex in 1:reps) {
    if(repIndex %%50 == 0){
      print(paste("treesize ", treesize.array[treesizeIndex]," rep ", repIndex,sep=""))}
    oubmbm.trait <-oubmbmmodel(model.params=oubmbm.params,reg.params,root=root,tree=tree)
    oubmbmstat[repIndex,]<-c(sum.stat(trait=oubmbm.trait$y,tree=tree),sum.stat(trait=oubmbm.trait$x1,tree=tree),sum.stat(trait=oubmbm.trait$x2,tree=tree))
    
    ououbm.trait <-ououbmmodel(model.params=ououbm.params,reg.params,root=root,tree=tree)
    ououbmstat[repIndex,]<-c(sum.stat(trait=ououbm.trait$y,tree=tree),sum.stat(trait=ououbm.trait$x1,tree=tree),sum.stat(trait=ououbm.trait$x2,tree=tree))
    
    oubmcir.trait <-oubmcirmodel(model.params=oubmcir.params,reg.params,root=root,tree=tree)
    oubmcirstat[repIndex,]<-c(sum.stat(trait=oubmcir.trait$y,tree=tree),sum.stat(trait=oubmcir.trait$x1,tree=tree),sum.stat(trait=oubmcir.trait$x2,tree=tree))
    
    ououcir.trait <-ououcirmodel(model.params=ououcir.params,reg.params,root=root,tree=tree)
    ououcirstat[repIndex,]<-c(sum.stat(trait=ououcir.trait$y,tree=tree),sum.stat(trait=ououcir.trait$x1,tree=tree),sum.stat(trait=ououcir.trait$x2,tree=tree))
  }
  modelset <- c(rep("oubmbm",reps),rep("ououbm",reps),rep("oubmcir",reps),rep("ououcir",reps))
  stat <- rbind(oubmbmstat,ououbmstat,oubmcirstat,ououcirstat)
  colnames(stat)<-c(
    "y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda",
    "x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda",
    "x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")
  save.image(paste(treesize.array[treesizeIndex], "cvfullmodel.RData",sep=""))
  # Cross-Validation
  cv.mod1 <- cv4postpr(modelset,stat,nval=100,tols=0.01,method="mnlogistic")
  cv.mod <- cv4postpr(modelset,stat,nval=100,tols=0.01,method="rejection")
  cv.mod$estim<-cv.mod1$estim
  s<-summary(cv.mod)
  save.image(paste(treesize.array[treesizeIndex], "cvfullmodel.RData",sep=""))
}