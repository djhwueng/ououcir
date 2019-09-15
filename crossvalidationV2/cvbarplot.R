#setwd("~/Dropbox/ChihPingWang/Thesis/crossvalidationV2/")
rm(list=ls())
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Thesis/crossvalidationV2/")
system("ls")
par(mfrow=c(2,2))
par(mar=c(2,2,1.5,0.5))
size<-array(c(10,30,50,100),c(4))
for(sizeIndex in 1: length(size)){
#  sizeIndex<-1
  print(sizeIndex)
  print(paste(size[sizeIndex],"cvfullmodel.RData",sep = ""))
  load(paste(size[sizeIndex],"cvfullmodel.RData",sep = ""))
  sdata<-as.matrix(s$conf.matrix$tol0.01)
  barplot(t(sdata),col=c("pink","skyblue","orange","green"),main=paste("taxa size ", size[sizeIndex]),ylim=c(0,100) ,xlab="", ylab="",beside=TRUE)
  #print(s$conf.matrix$tol0.01)
  }
