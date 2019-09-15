rm(list=ls())
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitdata/sanchez.Lasker_coral/")
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitdata/sanchez.Lasker_coral/")
library(ape)
library(phytools)
#http://www.phytools.org/Cordoba2017/ex/15/Plotting-methods.html

tree<-read.tree("tree.phy")
tree$root.edge<-0
dataset <- read.csv(file="trait.csv")
X <- read.csv(file="trait.csv",row.names=1)
tree$tip.label<-rownames(X)
dotTree(tree,X,standardize=FALSE,length=6)
tiplabels(cex=0.6)
nodelabels(cex=0.6)
#?dotTree

