rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")

library(phangorn)
library(geiger)
library(moments)
#mass
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-dataset$mass
names(resptrait)<-tree$tip.label
predtrait1<-dataset$head_height
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$head_length
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_raw_mass.RData")


#head_height
rm(list=ls())
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-dataset$head_height
names(resptrait)<-tree$tip.label
predtrait1<-dataset$mass
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$head_length
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_raw_head_height.RData")



rm(list=ls())
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0
#head_length
dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-dataset$head_length
names(resptrait)<-tree$tip.label
predtrait1<-dataset$mass
names(predtrait1)<-tree$tip.label
predtrait2<-dataset$head_height
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_raw_head_length.RData")


###
### log
###


rm(list=ls())
setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/aguirre_bat/")

library(phangorn)
library(geiger)
library(moments)
#mass
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-log(dataset$mass)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$head_height)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$head_length)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_log_mass.RData")


#head_height
rm(list=ls())
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0

dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-log(dataset$head_height)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$mass)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$head_length)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_log_head_height.RData")



rm(list=ls())
Gmatrix<-read.table("treeJSTOR-66.txt")
Gmatrix<-Gmatrix+t(Gmatrix)
diag(Gmatrix)<-1
Dmatrix<-2*(1-Gmatrix)
tree<-upgma(Dmatrix)
tree<-reorder(tree,"postorder")
tree$root.edge<-0
#head_length
dataset <- as.data.frame(read.table(file="traitJSTOR-66.txt"))
colnames(dataset) <- c("mass","head_height","head_length","max_bite_force")

resptrait<-log(dataset$head_length)
names(resptrait)<-tree$tip.label
predtrait1<-log(dataset$mass)
names(predtrait1)<-tree$tip.label
predtrait2<-log(dataset$head_height)
names(predtrait2)<-tree$tip.label

#source("~/Dropbox/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
source("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/modelcodeV2/empirical_analysis.r")
save.image("aguirre_bat_log_head_length.RData")
