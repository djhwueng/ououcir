#setwd("~/Dropbox/ChihPingWang/Model1/abc_V2/simulation64V2/oubmbm/")
setwd("~/Dropbox/FCU/Teaching/Mentoring/2019Spring/ChihPingWang/Model1/abc_V2/simulation_32_bciV2/oubmbm/")
model.params.array<-c("alpha.y", "sigmasq.x","tau")

jobs<-paste("oubmbmabcSimV2size32bci.",model.params.array,".r",sep="")

for(jobIndex in 1:length(jobs)){
  jobgo<-paste("nohup R CMD BATCH ", jobs[jobIndex]," >/dev/null &",sep="")
  print(jobgo)
  system(jobgo)
}