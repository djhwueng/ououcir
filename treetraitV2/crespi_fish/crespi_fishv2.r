rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/crespi_fish/")

library(phangorn)
library(geiger)
library(moments)

#bodylength
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-dataset$bodylength
names(resptrait)<-tree$tip.label
predtrait1<-dataset$eggweight
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$fecundity
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_raw_bodylength.RData")




rm(list=ls())
#eggweight
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-dataset$eggweight
names(resptrait)<-tree$tip.label
predtrait1<-dataset$bodylength
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$fecundity
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_raw_eggweight.RData")


rm(list=ls())
#fecundity
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-dataset$fecundity
names(resptrait)<-tree$tip.label
predtrait1<-dataset$bodylength
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$eggweight
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_raw_fecundity.RData")


###
### log
###

rm(list=ls())
#bodylength
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-log(dataset$bodylength)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$eggweight)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$fecundity)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_log_bodylength.RData")




rm(list=ls())
#eggweight
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-log(dataset$eggweight)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$bodylength)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$fecundity)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_log_eggweight.RData")


rm(list=ls())
#fecundity
Gmatrix<-read.table("tree_EOV-14.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="trait_EOV-14.txt"))
colnames(dataset) <- c("bodylength","GSI","fecundity","eggweight")
resptrait<-log(dataset$fecundity)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$bodylength)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$eggweight)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("crespi_fish_log_fecundity.RData")
