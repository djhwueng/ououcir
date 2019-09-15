rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/weiblen_fig/")

library(phangorn)
library(geiger)
library(moments)

#wallwidth
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
tree$tip.label<-dataset[,1]
resptrait<-dataset$wallwidth
names(resptrait)<-tree$tip.label
predtrait1<-dataset$stylelength
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$gallwidth
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_raw_wallwidth.RData")

#style length
rm(list=ls())
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$stylelength
names(resptrait)<-tree$tip.label
predtrait1<-dataset$wallwidth
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$gallwidth
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_raw_stylelength.RData")


#style length
rm(list=ls())
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-dataset$gallwidth
names(resptrait)<-tree$tip.label
predtrait1<-dataset$wallwidth
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$stylelength
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_raw_gallwidth.RData")


###
### log
###

rm(list=ls())

#wallwidth
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$wallwidth)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$stylelength)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$gallwidth)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_log_wallwidth.RData")

#style length
rm(list=ls())
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$stylelength)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$wallwidth)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$gallwidth)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_log_stylelength.RData")


#style length
rm(list=ls())
Gmatrix<-read.table("matrix.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- read.csv(file="trait.csv")
resptrait<-log(dataset$gallwidth)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$wallwidth)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$stylelength)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("weiblen_fig_log_gallwidth.RData")
