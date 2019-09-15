rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/webster.purvis_foram/")

library(phangorn)
library(geiger)
library(moments)


#area
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$area
names(resptrait)<-tree$tip.label
predtrait1<-dataset$width
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$length
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_raw_area.RData")


#width
rm(list=ls())
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$width
names(resptrait)<-tree$tip.label
predtrait1<-dataset$area
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$length
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_raw_width.RData")


#width
rm(list=ls())
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$length
names(resptrait)<-tree$tip.label
predtrait1<-dataset$area
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$width
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_raw_length.RData")



###
### log data
###

#area
rm(list=ls())
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$area)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$width)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$length)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_log_area.RData")


#width
rm(list=ls())
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$width)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$area)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$length)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_log_width.RData")


#width
rm(list=ls())
tree<-read.tree("intree")
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$length)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$area)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$width)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("webster.purvis_foram_log_length.RData")
