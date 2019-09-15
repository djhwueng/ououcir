rm(list=ls())

sim.bm<-function(x,n_path=n_path){
  xi<-x[1]
  bm<-cumsum(c(0,xi*rnorm(n_path-1)))
  return(bm)
  }

sim.ou<-function(x,n_path=n_path){
  bet  <-x[1]
  theta<-x[2]
  xi  <- x[3]
  ou<-numeric(n_path)
  ou[1]<-0
  for(i in 2:n_path){
    ou[i]<-ou[i-1]+bet*(theta-ou[i-1])+xi*rnorm(1)
  }
return(ou)
}

sim.cir<-function(x,n_path=n_path){
  gamma<-x[1]
  tau  <-x[2]
  xi   <-x[3]
  cir<-numeric(n_path)
  cir[1]<-0.1
  for(i in 2:n_path){
    cir[i]<-cir[i-1]+gamma*(tau-cir[i-1]) + xi*sqrt(cir[i-1])*rnorm(1)
    cir[i]<-abs(cir[i])
    }
  return(cir)
  }

#plot( sim.cir(c(0.1,2,1),n_path=100),type="l")

#we would like to simulate oubm: 
#the response trait is an ou process
#the predictor trait is a bm process with drift parameter sig_x  
#the optimum of the response is a bm process with drift paramter b1*sig_x

sim.bm.predictor<-function(x,n_path=n_path){
  sig_x<-x[1]
  return(cumsum(c(0,sig_x*rnorm(n_path-1))))
  }

sim.ou.predictor<-function(x,n_path=n_path){
  bet  <-x[1]
  theta<-x[2]
  sig_x<-x[3]
  ou<-numeric(n_path)
  ou[1]<-0
  for(i in 2:n_path){
    ou[i]<-ou[i-1] +  bet*(theta -ou[i-1]) + sig_x*rnorm(1)
    }
  return(ou)
  }

sim.cir.predictor<-function(x,n_path=n_path){
  gamma<-x[1]
  tau  <-x[2]
  sig_x<-x[3]
  cir<-numeric(n_path)
  cir[1]<-0
  for(i in 2:n_path){
    cir[i]<-cir[i-1]+ gamma*(tau-cir[i-1]) + sig_x*sqrt(cir[i-1])*rnorm(1)
  }
  cir[i]<-abs(cir[i])
  return(cir)
}

sim.optimum<-function(x,predictor_path1=predictor_path1,predictor_path2=predictor_path2){
  b0 <-x[1]
  b1 <-x[2]
  b2 <-x[3]
  b12<-x[4]#b12=0 no interaction
  return(b0+b1*predictor_path1+b2*predictor_path2+b12*predictor_path1*predictor_path2)
}

sim.response<-function(x,optimum_path=optimum_path,n_path=n_path){
  alp  <-x[1]
  sig_y<-x[2]
  response<-numeric(n_path)
  response[1]<-0
  for(i in 2:n_path){
    response[i]<-response[i-1]+alp*(optimum_path[i-1]-response[i-1]) + sig_y*rnorm(1)
    #oubm[i]<-oubm[i-1]+alp*( b0+b1*predictor_path[i-1] +rnorm(1)  - oubm[i-1]) + sig_y*rnorm(1)
  }
  return(response)
}

sim.response.drift<-function(x,optimum_path=optimum_path,drift=drift,n_path=n_path){
  alp<-x[1]
  response<-numeric(n_path)
  response[1]<-0
  for(i in 2:n_path){
    response[i]<-response[i-1]+alp*(optimum_path[i-1]-response[i-1]) + drift[i-1]*rnorm(1)
    #xi<-5
    #oubm[i]<-oubm[i-1]+alp*( theta[i-1]- oubm[i-1]) + cumsum(c(0,xi*rnorm(i-1)))[i]*rnorm(1)
  }
  return(response)
  }


SimModelsPath<-function(x,n_path=n_path){
  alp  <- x[1]
  sig_y<- x[2]
  bet  <- x[3]
  sig_x<- x[4]
  theta<- x[5]
  gamma<- x[6]  # for oubmcir or ououcir (force)
  tau  <- x[7] # for oubmcir or ououcir (optimum)
  xi   <- x[8] # for oubmbm or ououbm or oubmcir or ououcir (rate)
  b0   <- x[9]
  b1   <- x[10]
  b2   <- x[11]
  b12  <- x[12]
  
  bm_path<-sim.bm(xi,n_path=n_path)
  ou_path<-sim.ou(c(bet,theta,xi),n_path=n_path)
  cir_path<-sim.cir(c(gamma,tau,xi),n_path=n_path)
  #OUBM and OUBMBM share the same BM predictor and BM optimum paths.
  sim.bm.predictor_path1 <- sim.bm.predictor(sig_x,n_path=n_path) 
  sim.bm.predictor_path2 <- sim.bm.predictor(sig_x,n_path=n_path) 
  oubm.optimum_path <- sim.optimum(c(b0,b1,b2,b12),predictor_path1=sim.bm.predictor_path1,predictor_path2=sim.bm.predictor_path2)
  #oubm.optimum_path<-sim.optimum(c(b0,b1),predictor_path=sim.bm.predictor(sig_x))
  
  #OUBM
  #oubm_path<-sim.response(c(alp,sig_y), predictor_path=sim.bm.predictor_path,optimum_path=oubm.optimum_path)
  oubm_path<-sim.response(c(alp,sig_y), optimum_path=oubm.optimum_path, n_path=n_path)
  
  #OUBMBM
  #oubmbm_path<-sim.response.bm(c(alp,sig_y),optimum_path=oubm.optimum_path,drift=sim.bm(xi))
  oubmbm_path<-sim.response.drift(alp,optimum_path=oubm.optimum_path,drift=bm_path,n_path=n_path)
  
  #OUBMCIR
  oubmcir_path<-sim.response.drift(alp,optimum_path=oubm.optimum_path, drift=cir_path,n_path=n_path)
  #oubm_path<-sim.response(c(alp,sig_y),theta=oubm.optimum_path)
  
  #OUOU and OUOUBM share the same OU predictor and OU optimum paths.
  sim.ou.predictor_path1<- sim.ou.predictor(c(bet,theta,sig_x),n_path = n_path)
  sim.ou.predictor_path2<- sim.ou.predictor(c(bet,theta,sig_x),n_path = n_path)
  ouou.optimum_path<-sim.optimum(c(b0,b1,b2,b12),predictor_path1=sim.ou.predictor_path1,predictor_path2=sim.ou.predictor_path2)
  
  #OUOU 
  ouou_path<-sim.response(c(alp,sig_y), optimum_path=ouou.optimum_path, n_path=n_path)

  #OUOUBM
  ououbm_path<-sim.response.drift(alp, optimum_path=ouou.optimum_path, drift=bm_path, n_path=n_path) 
  
  #OUOUCIR
  ououcir_path<-sim.response.drift(alp,optimum_path=ouou.optimum_path, drift=cir_path,n_path=n_path)
   
     
  results<-list(bm_path=bm_path,
                ou_path=ou_path,
                cir_path=cir_path,
                          
                sim.bm.predictor_path1=sim.bm.predictor_path1,
                sim.bm.predictor_path2=sim.bm.predictor_path2,
                oubm.optimum_path=oubm.optimum_path,
                
                oubm_path=oubm_path,
                oubmbm_path=oubmbm_path,
                oubmcir_path=oubmcir_path,
                
                sim.ou.predictor_path1=sim.ou.predictor_path1,
                sim.ou.predictor_path2=sim.ou.predictor_path2,
                ouou.optimum_path=ouou.optimum_path,
                
                ouou_path=ouou_path,
                ououbm_path=ououbm_path,
                ououcir_path=ououcir_path
              )
  return(results)
  }



#if(FALSE){

# we will do simulation 1000 replicates. 
models<-c("BM","OU","OUBM","OUOU","OUBMBM","OUOUBM","OUBMCIR","OUOUCIR")
n_path<-1000 # n is the number of generations.
sim<-10
VarOut<-array(0,c(length(models),sim))


#Use two pairs of (b0,b1) to disntinguish different paths
#For OUBMXX
# alp<- 0.01
# sig_y<- 0.2
# b0<- 0.05
# b1<- 0.30
# sig_x<-1.50
# bet<-0.01
# theta<-0.12
# xi<-0.005


alp  <- 0.01
sig_y<- 0.15
bet  <- 0.02
sig_x<- 0.25
theta<- 0.8
gamma<- 0.02
tau  <- 0.75
xi   <- 0.05
b0   <- 2
b1   <- 2.38
b2   <- 1.42
b12  <- 0 # 1  
b12int<-1



#for oubm,oubmbm
# response and optimum have similar trend when smaller b1 detected
# when b1 is a bit of large say 0.3 or more, the trend is not clear. 

#for ouou,oubmbm
#currently smaller alp and bet makes the path similiar to oubm and oubmbm model, make sense!
#need ouou and ououbm with larger parameter values.

#check more and conclude the picture

p0<-c(alp,sig_y,bet,sig_x,theta,gamma,tau,xi,b0,b1,b2,b12)
results<- SimModelsPath(p0,n_path=n_path)
p1<-c(alp,sig_y,bet,sig_x,theta,gamma,tau,xi,b0,b1,b2,b12int)
results1<- SimModelsPath(p1,n_path=n_path)


for(simIndex in 1:sim){
 print(simIndex)
 results<- SimModelsPath(p0,n_path=n_path)
 VarOut[1,simIndex]<- var(results$bm_path)
 VarOut[2,simIndex]<- var(results$ou_path)
 VarOut[3,simIndex]<- var(results$oubm_path)
 VarOut[4,simIndex]<- var(results$ouou_path)
 VarOut[5,simIndex]<- var(results$oubmbm_path)
 VarOut[6,simIndex]<- var(results$ououbm_path)
 VarOut[7,simIndex]<- var(results$oubmcir_path)
 VarOut[8,simIndex]<- var(results$ououcir_path)
  }

#save.image("VarSimModelPath.RData")

#load("VarSimModelPath.RData")


apply(VarOut,1,mean)
apply(VarOut,1,sd)


summary(results$bm_path)
summary(results$ou_path)
summary(results$oubm_path)
summary(results$ouou_path)
summary(results$ououbm_path)
summary(results$oubmbm_path)
summary(results$ououcir_path)
summary(results$oubmcir_path)


sd(results$oubm_path)
sd(results$ouou_path)
sd(results$oubmbm_path)
sd(results$ououbm_path)
sd(results$oubmcir_path)
sd(results$ououcir_path)

plot(results$oubm.optimum_path)
plot(results$sim.bm.predictor_path1)
plot(results$sim.bm.predictor_path2)
plot(results$oubm_path)


setwd("~/Dropbox/ChiuMinChou/Paper_submitWork/")
#setEPS()
#postscript("simpath.eps",height=8,width=12)
par(mfrow=c(2,2))
#load("oubmbm1.RData")
source("PathFcn.r")
#PathFcn(optimum_path=results$oubm.optimum_path,response_path=results$oubm_path,response_path_A=results$oubmbm_path,response_path_AA=results$oubmcir_path,Model=NULL)
#legend(0, max(results$oubm.optimum_path,results$oubm_path,results$oubmbm_path,results$oubmcir_path),c(expression(theta[t]),expression(y[1][t]), expression(y[2][t]),expression(y[3][t])), cex=0.8, col=c("black","darkgreen","blue","red"), lwd=3)

#load("ououbm1.RData")
#source("PathFcn.r")
#PathFcn(optimum_path=results$ouou.optimum_path,response_path=results$ouou_path,response_path_A=results$ououbm_path,response_path_AA=results$ououcir_path,Model=NULL)
#legend(0, max(results$ouou.optimum_path,results$ouou_path,results$ououbm_path,results$ououcir_path),c(expression(theta[t]),expression(y[1][t]), expression(y[2][t]), expression(y[3][t]) ), cex=0.8, col=c("black","darkgreen","blue","red"), lwd=3)
#dev.off()
#}#end of if FALSE

#without interaction
#1
PathFcn_1(optimum_path1=results$oubm.optimum_path,optimum_path2=results$ouou.optimum_path,response_path=results$oubmbm_path,response_path_A=results$ououbm_path,response_path_AA=results$oubmcir_path,response_path_AAA=results$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[1])), cex=0.7, col=c("black","orange","darkgreen"), lty=1)
#2
PathFcn_2(optimum_path1=results$oubm.optimum_path,optimum_path2=results$ouou.optimum_path,response_path=results$oubmbm_path,response_path_A=results$ououbm_path,response_path_AA=results$oubmcir_path,response_path_AAA=results$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[2])), cex=0.7, col=c("black","orange","blue"), lty=1)
#3
PathFcn_3(optimum_path1=results$oubm.optimum_path,optimum_path2=results$ouou.optimum_path,response_path=results$oubmbm_path,response_path_A=results$ououbm_path,response_path_AA=results$oubmcir_path,response_path_AAA=results$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[3])), cex=0.7, col=c("black","orange","red"), lty=1)
#4
PathFcn_4(optimum_path1=results$oubm.optimum_path,optimum_path2=results$ouou.optimum_path,response_path=results$oubmbm_path,response_path_A=results$ououbm_path,response_path_AA=results$oubmcir_path,response_path_AAA=results$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[4])), cex=0.7, col=c("black","orange","purple"), lty=1)

#with interaction
#1
PathFcn_1(optimum_path1=results1$oubm.optimum_path,optimum_path2=results1$ouou.optimum_path,response_path=results1$oubmbm_path,response_path_A=results1$ououbm_path,response_path_AA=results1$oubmcir_path,response_path_AAA=results1$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[1])), cex=0.7, col=c("black","orange","darkgreen"), lty=1)
#2
PathFcn_2(optimum_path1=results1$oubm.optimum_path,optimum_path2=results1$ouou.optimum_path,response_path=results1$oubmbm_path,response_path_A=results1$ououbm_path,response_path_AA=results1$oubmcir_path,response_path_AAA=results1$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[2])), cex=0.7, col=c("black","orange","blue"), lty=1)
#3
PathFcn_3(optimum_path1=results1$oubm.optimum_path,optimum_path2=results1$ouou.optimum_path,response_path=results1$oubmbm_path,response_path_A=results1$ououbm_path,response_path_AA=results1$oubmcir_path,response_path_AAA=results1$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[3])), cex=0.7, col=c("black","orange","red"), lty=1)
#4
PathFcn_4(optimum_path1=results1$oubm.optimum_path,optimum_path2=results1$ouou.optimum_path,response_path=results1$oubmbm_path,response_path_A=results1$ououbm_path,response_path_AA=results1$oubmcir_path,response_path_AAA=results1$ououcir_path,Model=NULL)
legend("topleft", max(results$oubm.optimum_path,results$sim.bm.predictor_path,results$oubm_path2), c(expression(theta[1]),expression(theta[2]),expression(y[4])), cex=0.7, col=c("black","orange","purple"), lty=1)







#save.image("ououbm1.RData")
