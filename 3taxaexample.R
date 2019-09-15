rm(list=ls())
library(ape)
library(sde)
library(phytools)
library(phyclust)

sim.bm.one.path<-function(model.params,T=T,N=N,x0=x0){
  sigma<-model.params[3]
  dw<-rnorm(N,0,sqrt(T/N))
  path<-c(x0)
  for(Index in 2:(N+1)){
    path[Index]<-path[Index-1]+sigma*dw[Index-1]
  }
  return(path)
}

sim.bm.tree.path<-function(model.params,phy=phy,N=N,root=root){
  sim.node.data<-integer(length(phy$tip.label)+phy$Nnode)
  edge.number<-dim(phy$edge)[1]
  edge.length<-phy$edge.length
  ntips<-length(phy$tip.label)
  ROOT<-ntips+1
  anc<-phy$edge[,1]
  des<-phy$edge[,2]
  path.objects<-list()
  start.state<-root
  for(edgeIndex in edge.number:1){
    brnlen<-edge.length[edgeIndex]
    start.state<-sim.node.data[anc[edgeIndex]]
    assign(paste("path",edgeIndex,sep=""),sim.bm.one.path(model.params,T=brnlen,N=N,x0=start.state))
    temp.path<-get(paste("path", edgeIndex,sep=""))
    sim.node.data[des[edgeIndex]]<-temp.path[length(temp.path)]
    path.objects<-c(path.objects,list(get(paste("path",edgeIndex,sep=""))))
  }
  return(list(path.objects=path.objects, sim.node.data=sim.node.data))
}

plot.history.dt<-function(phy=phy,path.data=path.data,main=main){
  ntips<-length(phy$tip.label)
  edge.number<-dim(phy$edge)[1]
  x.start<-array(0,c(edge.number,1))
  x.end<-array(0,c(edge.number,1))
  step<-array(0,c(edge.number,1))
  anc.des.start.end<-cbind(phy$edge,step,x.start, x.end)
  colnames(anc.des.start.end)<-c("anc","des","step","x.start","x.end")
  anc.des.start.end<-apply(anc.des.start.end,2,rev)
  anc.des.start.end<-data.frame(anc.des.start.end)
  anc<- anc.des.start.end$anc
  des<- anc.des.start.end$des
  for(edgeIndex in 1:edge.number){
    path<-unlist(path.data$path.objects[edgeIndex])
    anc.des.start.end$step[edgeIndex]<-length(path)
  }
  for(edgeIndex in 1:edge.number){
    if(anc[edgeIndex]== Ntip(phy)+1){
      anc.des.start.end$x.start[edgeIndex]<- 1+ ceiling(nodeheight(phy,node=anc.des.start.end$anc[edgeIndex]))
    }else{
      anc.des.start.end$x.start[edgeIndex]<- ceiling(nodeheight(phy,node=anc.des.start.end$anc[edgeIndex]))
    }
    anc.des.start.end$x.end[edgeIndex]<- 1 + ceiling(nodeheight(phy,node=anc.des.start.end$des[edgeIndex]))
  }
  plot( NA,type="l",xlim=c(1,ceiling(get.rooted.tree.height(phy))+1 ),ylim=c(min(unlist(path.data)), max(unlist(path.data))),xaxt='n',frame.plot=FALSE,ylab="Trait value", xlab="",main=main, cex.main=1.2)
  anc.des.start.end
  for(edgeIndex in 1:edge.number){
    path<-unlist(path.data$path.objects[edgeIndex])
    x.start <-  anc.des.start.end$x.start[edgeIndex]
    x.end   <-  anc.des.start.end$x.end[edgeIndex]
    gap <- round(anc.des.start.end$step[edgeIndex]/( x.end-x.start))
    point.to.use<-(anc.des.start.end$x.start[edgeIndex]: anc.des.start.end$x.end[edgeIndex])
    sample.path<-path[seq(1,  anc.des.start.end$step[edgeIndex],by=gap)]
    
    if(length(point.to.use)!=length(sample.path)){
      if(length(point.to.use) > length(sample.path)){
        point.to.use<-point.to.use[1:length(sample.path)]
      }else{
        sample.path<-sample.path[1:length(point.to.use)]
      }
    }
    points(point.to.use , sample.path, type="l",lwd=3,col=colorS[edgeIndex])
  }
}#end of plot history

colorS<-c("black","red","blue","green")

T<-1
N<-2000
x0<-0.1
sims<-1000
true.alpha<-0.01
true.mu<-1
true.sigma<-0.1
model.params<-c(true.mu,true.alpha,true.sigma)
#plot(sim.cir.path(model.params, T=T, N=N,x0=0.1))
root<-0
# we can do sims to different regimes
size<-3
phy<-rcoal(size)
#phy<-rtree(size)
phy<-reorder(phy,"postorder")
min.length<-100
while(min(phy$edge.length)<min.length){
  phy$edge.length<-1.005*phy$edge.length
}

bm.path.data<-sim.bm.tree.path(model.params,phy=phy,N=N,root=root)


#par(mfrow=c(1,2),
op<-par(mfrow=c(1,2),
    oma=c(2,2,0,0) + 0.1,
    mar=c(1,1,1,1) + 0.1
    )

#plot(phy,edge.width=5,cex=1.5,show.node.lable=TRUE)
#tiplabels(pch=21, col="black", adj=1, bg="black", cex=2)
#nodelabels(pch=21,col="black",adj=1,bg="black",cex=2)
#edgelabels(phy$edge.length,bg="black",col="white",font=2)
#https://rdrr.io/cran/ape/man/makeNodeLabel.html
phy1<-"((A,B),C);"
phy1<-read.tree(text=phy1)
phy1$edge.length<-c(1.2,1.6,1.6,2.8)
phy1$edge.length
phy1$tip.label<-c("  A","  B","  C")

colorS<-c("black","green","red","blue")
color2<-c("green","blue","red","black")
plot(phy1,edge.width=5,cex=1.5,edge.color=color2)

nodelabels("", 5, frame="c",bg="black",adj=0)
nodelabels("E", 5, frame="c",bg="black",adj=-1.5,cex=1)
nodelabels("", 4, frame="c",bg="black",adj=0)
nodelabels("D", 4, frame="c",bg="black",adj=-1.5,cex=1)
nodelabels("", 1, frame="c",bg=   "black",adj=0)
nodelabels("", 2, frame="c",bg="black",adj=0)
nodelabels("", 3, frame="c",bg="black",adj=0)
edgelabels(phy1$edge.length,bg="black",adj=c(0.5,0.5),col="white",font=2)
axisPhylo(1,las=1,backward=FALSE)

plot.history.dt(phy=phy,path.data=bm.path.data,main= "Trajectories of Phenotypic Value")
par(op)
