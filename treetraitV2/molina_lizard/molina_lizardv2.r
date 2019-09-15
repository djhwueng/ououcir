rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/molina_lizard/")

library(phangorn)
library(geiger)
library(moments)

# max.length
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$max.length
names(resptrait)<-tree$tip.label
predtrait1<-dataset$hatchlingmass
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$hacthlinglength
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_raw_max.length.RData")


rm(list=ls())
# hatchlingmass
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$hatchlingmass
names(resptrait)<-tree$tip.label
predtrait1<-dataset$max.length
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$hacthlinglength
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_raw_hatchlingmass.RData")


rm(list=ls())
# hacthlinglength
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$hacthlinglength
names(resptrait)<-tree$tip.label
predtrait1<-dataset$max.length
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$hatchlingmass
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_raw_hacthlinglength.RData")


###
### log
###



rm(list=ls())
# max.length
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$max.length)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$hatchlingmass)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$hacthlinglength)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_log_max.length.RData")


rm(list=ls())
# hatchlingmass
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$hatchlingmass)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$max.length)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$hacthlinglength)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_log_hatchlingmass.RData")


rm(list=ls())
# hacthlinglength
Gmatrix<-read.table("EOV-24-1.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$hacthlinglength)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$max.length)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$hatchlingmass)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("molina_lizard_log_hacthlinglength.RData")
