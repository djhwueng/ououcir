rm(list=ls())
#setwd("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/")
system("pwd")
foldername<-c("aguirre_bat","bonine_lizard","crespi_fish","molina_lizard","nie_lizard","sanchez.Lasker_coral","webster.purvis_foram","weiblen_fig")
Rfilenames<-c("aguirre_bat_v2.r","bonine_lizardv2.r","crespi_fishv2.r","molina_lizardv2.r","nie_lizardv2.r","sanchez.Lasker_coral_v2.R","webster.purvis_foramv2.r","weiblen_figv2.r")

for(Index in 1:length(foldername)){
  #foldertoset<-paste("~/Dropbox/ChihPingWang/EmpiricalMaincode/treetraitV2/",foldername[Index],sep="")  
  foldertoset<-paste("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/EmpiricalMaincode/treetraitV2/",foldername[Index],sep="")  
  setwd(foldertoset)
  print(foldertoset)
  jobtosubmit<- paste("nohup R CMD BATCH ",Rfilenames[Index]," > /dev/null &", sep="")
  print(jobtosubmit)
  system(jobtosubmit)
}

