rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/bonine_lizard/")

library(phangorn)
library(geiger)
library(moments)
#bodymass
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$bodymass
names(resptrait)<-tree$tip.label
predtrait1<-dataset$snoutlength
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$thighmuscle
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_raw_bodymass.RData")


rm(list=ls())
#snoutlength
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$snoutlength
names(resptrait)<-tree$tip.label
predtrait1<-dataset$bodymass
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$thighmuscle
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_raw_snoutlength.RData")

rm(list=ls())
#thighmuscle
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$thighmuscle
names(resptrait)<-tree$tip.label
predtrait1<-dataset$bodymass
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$snoutlength
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_raw_thighmuscle.RData")


###
### log
###



rm(list=ls())
#bodymass
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$bodymass)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$snoutlength)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$thighmuscle)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_log_bodymass.RData")


rm(list=ls())
#snoutlength
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$snoutlength)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$bodymass)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$thighmuscle)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_log_snoutlength.RData")

rm(list=ls())
#thighmuscle
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$thighmuscle)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$bodymass)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$snoutlength)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("bonine_lizard_log_thighmuscle.RData")
