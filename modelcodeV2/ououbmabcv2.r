ououbmmodel<-function(model.params,reg.params,root=root,tree=tree){
  alpha.y<<-model.params[1]
  alpha.x<-model.params[2]
  theta.x<-model.params[3]
  sigmasq.x<-model.params[4]
  tau<-model.params[5]

  b0<-reg.params[1]
  b1<-reg.params[2]
  b2<-reg.params[3]

  n<-Ntip(tree)
  x1nodestates<-array(NA,c(2*n-1))
  x1nodestates[n+1]<- root$x1.ou.root
  x2nodestates<-array(NA,c(2*n-1))
  x2nodestates[n+1]<- root$x2.ou.root
  optimnodestates<-array(NA,c(2*n-1))
  optimnodestates[n+1]<- b0 + b1*root$x1.ou.root + b2*root$x2.ou.root
  ynodestates<-array(NA,c(2*n-1))
  ynodestates[n+1]<-root$y.ou.root

  N<-dim(tree$edge)[1]
  anc<-tree$edge[,1]
  des<-tree$edge[,2]
  treelength<-tree$edge.length

  for(index in N:1){
    x1ou.mean<-x1nodestates[anc[index]]*exp(-alpha.x*treelength[index]) + theta.x*(1-exp(-alpha.x*treelength[index]))
    x1ou.sd<-sqrt((sigmasq.x/(2*alpha.x))*(1-exp(-2*alpha.x*treelength[index])))
    if(x1ou.sd<1e-10){x1ou.sd<-sigmasq.x*treelength[index]}
    x1nodestates[des[index]]<-rnorm(n=1,mean=x1ou.mean,sd=x1ou.sd)

    x2ou.mean<-x2nodestates[anc[index]]*exp(-alpha.x*treelength[index]) + theta.x*(1-exp(-alpha.x*treelength[index]))
    x2ou.sd<-sqrt((sigmasq.x/(2*alpha.x))*(1-exp(-2*alpha.x*treelength[index])))
    if(x2ou.sd<1e-10){x2ou.sd<-sigmasq.x*treelength[index]}
    x2nodestates[des[index]]<-rnorm(n=1,mean=x2ou.mean,sd=x2ou.sd)

    optimnodestates[des[index]]<- b0 + b1*x1nodestates[des[index]] + b2*x2nodestates[des[index]]
    sigmasq.theta<- b1^2*sigmasq.x + b2^2*sigmasq.x
    
    ###
    theta0.y<-optimnodestates[des[index]]        
    A<-(alpha.y*theta0.y/ (alpha.y-alpha.x)) *(exp((alpha.y-alpha.x)*treelength[index]) -1)
    tilde.theta.y<- optimnodestates[des[index]]
    B<- tilde.theta.y*(exp(alpha.y*treelength[index])-1) - (alpha.y*tilde.theta.y/(alpha.y-alpha.x))*(exp((alpha.y-alpha.x)*treelength[index]) -1)
    vs<-sigmasq.theta*alpha.y^2*exp(2*alpha.y*treelength[index])*(1-exp(-2*alpha.x*treelength[index]))/ (2*alpha.x)
    C<-pnorm(treelength[index],mean=0,sd=sqrt(vs))- 0.5#integral starts from 0
    INTtime<- A+B+C
    INT1<-exp(-alpha.y*treelength[index])*INTtime
    
    ###
    fexpr<-expression(exp(alpha.y*t)*w)
    res<-st.int(fexpr,type="ito",M=1,lower=0,upper=treelength[index])
    INT2<-tau*exp(-alpha.y*treelength[index])*median(res$X)

    ynodestates[des[index]]<-ynodestates[anc[index]] + INT1 + INT2
    }

  simtrait<-ynodestates[1:n]
  return(list(y=simtrait,x1=x1nodestates[1:n],x2=x2nodestates[1:n]))
  #return(c(mean(simtrait),sd(simtrait)))
  }

#print(model(model.params=c(5,1,1,3,2),reg.params=c(1,1,1),root=root,tree=tree))

ououbmprior<-function(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params){
  alpha.y.min <-prior.model.params["alpha.y.min"]
  alpha.y.max <-prior.model.params["alpha.y.max"]
  alpha.x.min <- prior.model.params["alpha.x.min"]
  alpha.x.max <- prior.model.params["alpha.x.max"]
  theta.x.min <-prior.model.params["theta.x.min"]
  theta.x.max <- prior.model.params["theta.x.max"]
  sigmasq.x.min <-prior.model.params["sigmasq.x.min"]
  sigmasq.x.max <- prior.model.params["sigmasq.x.max"]
  tau.min <- prior.model.params["tau.min"]
  tau.max <- prior.model.params["tau.max"]

  alpha.y<-runif(n=1,min=alpha.y.min,max=alpha.y.max)
  alpha.x<-runif(n=1,min=alpha.x.min,max=alpha.x.max)
  theta.x<-runif(n=1,min=theta.x.min,max=theta.x.max)
  sigmasq.x<-runif(n=1,min=sigmasq.x.min,max=sigmasq.x.max)
  tau<-runif(n=1,min=tau.min,max=tau.max)

  b0.min<-prior.reg.params[1]
  b0.max<-prior.reg.params[2]
  b1.min<-prior.reg.params[3]
  b1.max<-prior.reg.params[4]
  b2.min<-prior.reg.params[5]
  b2.max<-prior.reg.params[6]
  b0<-runif(n=1, min=b0.min, max=b0.max)
  b1<-runif(n=1, min=b1.min, max=b1.max)
  b2<-runif(n=1, min=b2.min, max=b2.max)

  model.params<-c(alpha.y, alpha.x, theta.x, sigmasq.x, tau)
  reg.params<- c(b0, b1, b2)

  return(list(model.params=model.params, reg.params=reg.params))
}
