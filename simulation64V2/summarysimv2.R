rm(list=ls())
library(xtable)

simfolder<-"~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation64V2/"
foldername<-c("oubmbm","ououbm","oubmcir","ououcir")
n.array<-c(16,32,64,128)
oubmbm.outputtable<-array(NA,c(4,2+3+3))
ououbm.outputtable<-array(NA,c(4,2+5+3))
oubmcir.outputtable<-array(NA,c(4,2+5+3))
ououcir.outputtable<-array(NA,c(4,2+7+3))

for(folderIndex in 1: length(foldername)){
    #folderIndex<-1
    folder<-paste(simfolder,foldername[folderIndex],"/L=1000",sep = "")
    #print(folder)
    setwd(folder)
    outputtable<-get(paste(foldername[folderIndex],".outputtable",sep="" ))
    outputtable[,1]<-foldername[folderIndex]
    outputtable[,2]<-n.array
    for(sizeIndex in 1:length(n.array)){
    #  sizeIndex<-1
      rfile<-paste(foldername[folderIndex],"SimV2size",n.array[sizeIndex],".RData",sep="")
    #  print(rfile)
      load(rfile)
      postsample<-get(paste("post.",foldername[folderIndex],sep=""))
    #  print(round(apply(postsample,2, mean),3))
      meanpost<-round(apply(postsample,2, mean),2)
      sdpost<-round(apply(postsample,2, sd),2)
      for(fillIndex in 3:dim(outputtable)[2]){
        outputtable[sizeIndex,fillIndex]<-paste(meanpost[fillIndex-2],"(",sdpost[fillIndex-2],")",sep="")
      }
    }
    colnames(outputtable)<-c("model","taxa", colnames(postsample))
    print(xtable(outputtable))
  }




### V2 BIG TABLE FOR ALL PARAMETERS IN MODELS

rm(list=ls())
library(xtable)
simfolder<-"~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation64V2/"
foldername<-c("oubmbm","ououbm","oubmcir","ououcir")
n.array<-c(16,32,64,128)

bigoutputtable<- array(NA,c(16,13))
colnames(bigoutputtable)<-c("model","taxa","alpha.y","alpha.x","theta.x","sigmasq.x","tau","alpha.tau","theta.tau","sigmasq.tau","b0","b1","b2")
bigoutputtable[,1]<-rep(c("oubmbm","ououbm","oubmcir","ououcir") ,each=4)
bigoutputtable[,2]<-rep(c(16,32,64,128) ,times=4)
count<-0
for(folderIndex in 1: length(foldername)){
 # folderIndex<-1
  folder<-paste(simfolder,foldername[folderIndex],"/L=1000",sep = "")
  #print(folder)
  setwd(folder)
  for(sizeIndex in 1:length(n.array)){
    
    count<-count+1
#    sizeIndex<-1
    rfile<-paste(foldername[folderIndex],"SimV2size",n.array[sizeIndex],".RData",sep="")
    #  print(rfile)
    load(rfile)
    postsample<-get(paste("post.",foldername[folderIndex],sep=""))
    #  print(round(apply(postsample,2, mean),3))
    meanpost<-round(apply(postsample,2, mean),2)
    sdpost<-round(apply(postsample,2, sd),2)
    meansdpost<-paste(meanpost,"(",sdpost,")",sep="")
    names(meansdpost)<-names(meanpost)
    fillposition<-colnames(bigoutputtable)%in%names(meanpost)
    bigoutputtable[count,fillposition]<-meansdpost
  }
}
print(bigoutputtable)
xtable(bigoutputtable[,1:10])
xtable(bigoutputtable[,c(1,2,11:13)])



