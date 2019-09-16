rm(list=ls())
library(xtable)
#sanchez.Lasker_coral.RData
load("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/sanchez.Lasker_coral/sanchez.Lasker_coral_raw_polyp50000.RData")
post.oubmbm.mean <- round(apply(post.oubmbm,2,mean),digits = 4)
post.ououbm.mean <- round(apply(post.ououbm,2,mean),digits = 4)
post.oubmcir.mean <- round(apply(post.oubmcir,2,mean),digits = 4)
post.ououcir.mean <- round(apply(post.ououcir,2,mean),digits = 4)


table.params <- data.frame(matrix(NA,4,8))
rownames(table.params) <- c("OUBMBM","OUOUBM","OUBMCIR","OUOUCIR")
colnames(table.params)<- c("alpha.y","sigmasq.x","tau","alpha.x","theta.x","alpha.tau","theta.tau","sigmasq.tau")

table.params["OUBMBM",c("alpha.y","sigmasq.x","tau")]<-post.oubmbm.mean[c("alpha.y","sigmasq.x","tau")]


table.params["OUOUBM",c("alpha.y","alpha.x","theta.x", "sigmasq.x", "tau")]<-post.ououbm.mean[c("alpha.y","alpha.x","theta.x", "sigmasq.x", "tau")] 

table.params["OUBMCIR",c("alpha.y", "sigmasq.x","alpha.tau","theta.tau", "sigmasq.tau")]<-post.oubmcir.mean[c("alpha.y", "sigmasq.x","alpha.tau","theta.tau", "sigmasq.tau")]

table.params["OUOUCIR",c("alpha.y","alpha.x","theta.x","sigmasq.x","alpha.tau","theta.tau", "sigmasq.tau")]<-post.ououcir.mean[c("alpha.y","alpha.x","theta.x","sigmasq.x","alpha.tau","theta.tau", "sigmasq.tau")]
table.params[,"sigmasq.x"]<-sqrt(table.params[,"sigmasq.x"])
table.params[,"sigmasq.tau"]<-sqrt(table.params[,"sigmasq.tau"])

colnames(table.params)<- c("alpha.y","sigma.x","tau","alpha.x","theta.x","alpha.tau","theta.tau","sigma.tau")
table.params<-table.params[, c("alpha.y","alpha.x", "alpha.tau", "theta.x","theta.tau", "sigma.x","tau","sigma.tau")]

xtable(table.params,digits=4)


table.b<- matrix(NA,4,3)
rownames(table.b) <- c("OUBMBM","OUOUBM","OUBMCIR","OUOUCIR")
colnames(table.b)<-c("b0","b1","b2")
table.b[1,]<-post.oubmbm.mean[c(4:6)]
table.b[2,]<-post.ououbm.mean[c(6:8)]
table.b[3,]<-post.oubmcir.mean[c(6:8)]
table.b[4,]<-post.ououcir.mean[c(8:10)]
table.b


ols<-lm(resptrait~predtrait1+predtrait2)
ols$coefficients
OLS<-rep(NA,11)
OLS[9:11]<-ols$coefficients
table.out<-rbind(cbind(table.params,table.b),OLS)
xtable(table.out,digits=4)



post.oubmbm.qt <- round(apply(post.oubmbm,2,quantile,probs=c(0.025,0.975)),digits = 4)
post.ououbm.qt <- round(apply(post.ououbm,2,quantile,probs=c(0.025,0.975)),digits = 4)
post.oubmcir.qt <- round(apply(post.oubmcir,2,quantile,probs=c(0.025,0.975)),digits = 4)
post.ououcir.qt <- round(apply(post.ououcir,2,quantile,probs=c(0.025,0.975)),digits = 4)


