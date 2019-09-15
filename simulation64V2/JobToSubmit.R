rm(list=ls())
simfolder<-"~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation64V2/"
foldername<-c("oubmbm","ououbm")
n.array<-c(64,128)
for(folderIndex in 1: length(foldername)){
    print(paste(simfolder,foldername[folderIndex],sep = ""))
    setwd(paste(simfolder,foldername[folderIndex],sep = ""))
for(size in 1:length(n.array)){
    #print(paste(foldername[folderIndex],"abcSimV2size",n.array[size],sep=""))
    rfile<-paste(foldername[folderIndex],"abcSimV2size",n.array[size],sep="")
    print(paste("nohup R CMD BATCH ", rfile, ".r /dev/null &",sep=""))
    submitjob<-paste("nohup R CMD BATCH ", rfile, ".r >/dev/null &",sep="")
    system(submitjob)
    }
  }
