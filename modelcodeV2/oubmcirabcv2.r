oubmcirmodel<-function(model.params,reg.params,root=root,tree=tree){
  alpha.y<-model.params[1]
  sigmasq.x<-model.params[2]
  alpha.tau<-model.params[3]
  theta.tau<-model.params[4]
  sigmasq.tau<-model.params[5]
  b0<-reg.params[1]
  b1<-reg.params[2]
  b2<-reg.params[3]

  n<-Ntip(tree)
  x1nodestates<-array(NA,c(2*n-1))
  x1nodestates[n+1]<- root$x1.bm.root
  x2nodestates<-array(NA,c(2*n-1))
  x2nodestates[n+1]<- root$x2.bm.root
  optimnodestates<-array(NA,c(2*n-1))
  optimnodestates[n+1]<-  b0 + b1*root$x1.bm.root + b2*root$x2.bm.root
  sigmasqnodestates<-array(NA,c(2*n-1))
  sigmasqnodestates[n+1]<- root$y.ou.sigmsq 
  ynodestates<-array(NA,c(2*n-1))
  ynodestates[n+1]<-root$y.ou.root

  N<-dim(tree$edge)[1]
  anc<-tree$edge[,1]
  des<-tree$edge[,2]
  treelength<-tree$edge.length
  for(index in N:1){

    x1nodestates[des[index]]<-rnorm(n=1,mean=x1nodestates[anc[index]],sd= sqrt(sigmasq.x*treelength[index]))
    x2nodestates[des[index]]<-rnorm(n=1,mean=x2nodestates[anc[index]],sd= sqrt(sigmasq.x*treelength[index]))

    optimnodestates[des[index]]<- b0 + b1*x1nodestates[des[index]] + b2*x2nodestates[des[index]]
    sigmasq.theta<- b1^2*sigmasq.x + b2^2*sigmasq.x
    
    c<- sigmasq.tau*(1-exp(-alpha.tau*treelength[index]))/(4*alpha.tau)
    k<- (4*theta.tau*alpha.tau)/sigmasq.tau
    tau0<-sigmasqnodestates[anc[index]]
    lambda<-4*alpha.tau*exp(-alpha.tau*treelength[index])
    lambda<-lambda/(sigmasq.tau*(1-exp(-alpha.tau*treelength[index])))*tau0
    tmp = rchisq(n=1, df=k, ncp = lambda)
    sig_u <- c*tmp
    sigmasqnodestates[des[index]]<-sig_u

    ###
    INT1var<-sigmasq.theta*(exp(2*alpha.y*treelength[index])-1)/(2*alpha.y) 
    INT1<-exp(-alpha.y*treelength[index])* rnorm(n=1,mean=0,sd=sqrt(INT1var))
    
    ###
    a.var<-(1-exp(-2*alpha.y*treelength[index]))/(2*alpha.y) 
    a <- rnorm(n=1, mean=0, sd=sqrt(a.var))
    b.var<- (sigmasqnodestates[anc[index]]-theta.tau)^2/(2*(alpha.y-alpha.tau))
    b.var<-b.var*(exp(-2*alpha.tau*treelength[index])-exp(-2*alpha.y*treelength[index]))    
    b <- rnorm(n=1, mean=0, sd=sqrt(b.var))

    n_t <- 1
    n_s <- 1
    outer.int.sum=0
    for(outer.index in 1:n_t){
      inner.int.sum = 0
      for(inner.index in 1:n_s){
        c<- sigmasq.tau*(1-exp(-alpha.tau*(inner.index/n_s)))/(4*alpha.tau)
        k<- (4*theta.tau*alpha.tau)/sigmasq.tau
        lambda<- 4*alpha.tau*exp(-alpha.tau*(inner.index/n_s))
        tau0<-sigmasqnodestates[anc[index]]
        lambda<-lambda/(sigmasq.tau*(1-exp(-alpha.tau*(inner.index/n_s))))*tau0
        tmp = rchisq(n=1, df=k, ncp = lambda)
        sig_u <- c*tmp
        inner.int.sum  <-  inner.int.sum + exp(alpha.tau*(inner.index/n_s))*rnorm(n=1,mean=0, sd=sqrt(1/n_s))*sqrt(sig_u)
      }
      outer.int.sum <- outer.int.sum + exp(-(alpha.y+alpha.tau)*(outer.index/n_t))*inner.int.sum*rnorm(n=1,mean=0, sd=sqrt(1/n_t))
    }
    c <- sqrt(sigmasq.tau)*outer.int.sum
    INT2 <- (a + b + c)
    ynodestates[des[index]]<-ynodestates[anc[index]] + INT1 + INT2
  }
  simtrait<-ynodestates[1:n]
  return(list(y=simtrait,x1=x1nodestates[1:n],x2=x2nodestates[1:n]))
  }

oubmcirprior<-function(prior.model.params=prior.model.params,prior.reg.params=prior.reg.params){
  alpha.y.min <-prior.model.params["alpha.y.min"]
  alpha.y.max <-prior.model.params["alpha.y.max"]
  sigmasq.x.min <-prior.model.params["sigmasq.x.min"]
  sigmasq.x.max <- prior.model.params["sigmasq.x.max"]
  alpha.tau.min <- prior.model.params["alpha.tau.min"]
  alpha.tau.max <- prior.model.params["alpha.tau.max"]
  theta.tau.min <- prior.model.params["theta.tau.min"]
  theta.tau.max <- prior.model.params["theta.tau.max"]
  sigmasq.tau.min <- prior.model.params["sigmasq.tau.min"]
  sigmasq.tau.max <- prior.model.params["sigmasq.tau.max"]

  alpha.y<-runif(n=1,min=alpha.y.min,max=alpha.y.max)
  sigmasq.x<-runif(n=1,min=sigmasq.x.min,max=sigmasq.x.max)
  alpha.tau <- runif(n=1,min=alpha.tau.min,max=alpha.tau.max)
  theta.tau <- runif(n=1,min=theta.tau.min,max=theta.tau.max)
  sigmasq.tau<-runif(n=1,min=sigmasq.tau.min,max=sigmasq.tau.max)

  b0.min<-prior.reg.params[1]
  b0.max<-prior.reg.params[2]
  b1.min<-prior.reg.params[3]
  b1.max<-prior.reg.params[4]
  b2.min<-prior.reg.params[5]
  b2.max<-prior.reg.params[6]
  b0<-runif(n=1, min=b0.min, max=b0.max)
  b1<-runif(n=1, min=b1.min, max=b1.max)
  b2<-runif(n=1, min=b2.min, max=b2.max)

  model.params<-c(alpha.y, sigmasq.x, alpha.tau, theta.tau, sigmasq.tau)
  reg.params<- c(b0, b1, b2)

  return(list(model.params=model.params, reg.params=reg.params))
}
