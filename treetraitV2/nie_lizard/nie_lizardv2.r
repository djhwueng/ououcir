rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/nie_lizard/")

library(phangorn)
library(geiger)
library(moments)

#avesize
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-dataset$avgsize
names(resptrait)<-tree$tip.label
predtrait1<-dataset$cluthsize
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$cluthmass
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_raw_avesize.RData")



#cluthsize
rm(list=ls())
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-dataset$cluthsize
names(resptrait)<-tree$tip.label
predtrait1<-dataset$avgsize
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$cluthmass
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_raw_cluthsize.RData")



#cluthmass
rm(list=ls())
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-dataset$cluthmass
names(resptrait)<-tree$tip.label
predtrait1<-dataset$avgsize
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$cluthsize
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_raw_cluthmass.RData")


###
### log
###

#avesize
rm(list=ls())
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-log(dataset$avgsize)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$cluthsize)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$cluthmass)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_log_avesize.RData")


#cluthsize
rm(list=ls())
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-log(dataset$cluthsize)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$avgsize)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$cluthmass)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_log_cluthsize.RData")



#cluthmass
rm(list=ls())
Gmatrix<-read.table("tree_EOV-2.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.table(file="trait_EOV-2.txt")
colnames(dataset) <- c("sizematurity","avgsize","agematurity","eggmass","cluthsize","cluthmass","eggperyear")
resptrait<-log(dataset$cluthmass)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$avgsize)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$cluthsize)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("nie_lizard_log_cluthmass.RData")
