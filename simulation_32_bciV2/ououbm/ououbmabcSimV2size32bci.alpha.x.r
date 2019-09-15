#setwd("~/Dropbox/ChihPingWang/Model1/abc_V2/simulation64V2/oubmbm/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/ououbm/")

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


source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/bcifcn.r")

load("ououbmsimV2size32.RData")

sum.stat<-function(trait=trait,tree=tree){
  names(trait)<-tree$tip.label
  pic.trait<-pic(x=trait,phy=tree)
  return(c(mean(trait),sd(trait),median(trait),skewness(trait),kurtosis(trait),mean(pic.trait),sd(pic.trait),median(pic.trait),skewness(pic.trait),kurtosis(pic.trait),phylosig(tree,x=trait,method = "K",test=T)$K,phylosig(tree,x=trait,method = "lambda",test=T)$lambda))
}


postTheta <- data.frame(rej$unadj.values)

rep=50
sims=1000
root=root
tree=tree
tol=0.1
model="ououbm"
true.reg.params=prior.params$reg.params
names(true.reg.params)<-c("b0","b1","b2")


paramname<-"alpha.x"
post.params.array=sample(postTheta[,paramname],rep,replace=FALSE)
true.model.params=prior.params$model.params
rownames(model.params.array)<-c("alpha.y","alpha.x","theta.x","sigmasq.x","tau")
sample_params<-bcifcn(model=model,paramname=paramname,rep=rep,post.params.array=post.params.array,true.model.params=true.model.params,true.reg.params=true.reg.params,root=root,tree=tree,sims=sims,tol=tol)
save.image(paste(model,n,"_",paramname,".rda",sep=""))
