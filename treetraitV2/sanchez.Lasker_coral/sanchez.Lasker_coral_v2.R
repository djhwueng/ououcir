rm(list=ls())
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/sanchez.Lasker_coral/")
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/sanchez.Lasker_coral/")

library(phangorn)
library(geiger)
library(moments)

tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

###thickness
resptrait<-dataset$thickness
names(resptrait)<-tree$tip.label
predtrait1<-dataset$distance
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$polyp
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("sanchez.Lasker_coral_raw_thickness.RData")

####distance
rm(list=ls())

tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

resptrait<-dataset$distance
names(resptrait)<-tree$tip.label
predtrait1<-dataset$thickness
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$polyp
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("sanchez.Lasker_coral_raw_distance.RData")


### polyp
rm(list=ls())
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

resptrait<-dataset$polyp
names(resptrait)<-tree$tip.label
predtrait1<-dataset$thickness
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$distance
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")


save.image("sanchez.Lasker_coral_raw_polypTest2.RData")


###
### log data
###
rm(list=ls())
tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

###thickness
resptrait<-log(dataset$thickness)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$distance)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$polyp)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("sanchez.Lasker_coral_log_thickness.RData")

####distance
rm(list=ls())

tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

resptrait<-log(dataset$distance)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$thickness)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$polyp)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("sanchez.Lasker_coral_log_distance.RData")


### polyp
rm(list=ls())

tree<-read.tree("tree.phy")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")

resptrait<-log(dataset$polyp)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$thickness)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$distance)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("sanchez.Lasker_coral_log_polyp.RData")
