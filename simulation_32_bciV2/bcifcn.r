simparam<-function(paramname){
  alpha.y.rate <-5
  tau.shape <- 2
  tau.scale <- 1
  alpha.x.rate <- 8# assume exponential
  theta.x.mean <- 0# assume normal
  theta.x.sd  <- 1
  sigmasq.x.shape <-2# assume invgamma
  sigmasq.x.scale <-2
  alpha.tau.rate <- 4# assume exponential
  theta.tau.mean <- 0.5 #assume logNormal
  theta.tau.sd <- 1
  sigmasq.tau.shape <- 2# assume inv gamma
  sigmasq.tau.scale <- 1

  if(paramname=="alpha.y"){param<-rexp(n=1, rate=alpha.y.rate)}
  if(paramname=="sigmasq.x"){param<-rinvgamma(n=1, shape=sigmasq.x.shape, scale=sigmasq.x.scale)}
  if(paramname=="tau"){param<-rinvgamma(n=1, shape=tau.shape, scale=tau.scale)}
  if(paramname=="alpha.x"){param<-rexp(n=1, rate=alpha.x.rate)}
  if(paramname=="theta.x"){param<-rnorm(n=1, mean=theta.x.mean, sd=theta.x.sd)}
  if(paramname=="sigmasq.tau"){param<-rinvgamma(n=1, shape=sigmasq.tau.shape, scale=sigmasq.tau.scale)}
  if(paramname=="alpha.tau"){param<-rexp(n=1, rate=alpha.tau.rate)}
  if(paramname=="theta.tau"){param<-rlnorm(n=1, meanlog=theta.tau.mean, sdlog=theta.tau.sd)}
  names(param)<-paramname
  return(param)
}



bcifcn<-function(model=model,paramname=paramname,rep=rep,post.params.array,true.model.params=true.model.params,true.reg.params=true.reg.params,root=root,tree=tree,sims=sims,tol=tol){
  sample_params<- matrix(NA,rep,sims*tol)
  for(repIndex in 1:rep){
    #repIndex<-1
    true.param<-post.params.array[repIndex]
    names(true.param)<-paramname
    true.model.params[names(true.param)]<-true.param
    if(model=="oubmbm"){true.trait<-oubmbmmodel(model.params=true.model.params,reg.params=true.reg.params,root=root,tree=tree)}
    if(model=="ououbm"){true.trait<-ououbmmodel(model.params=true.model.params,reg.params=true.reg.params,root=root,tree=tree)}
    if(model=="oubmcir"){true.trait<-oubmcirmodel(model.params=true.model.params,reg.params=true.reg.params,root=root,tree=tree)}
    if(model=="ououcir"){true.trait<-ououcirmodel(model.params=true.model.params,reg.params=true.reg.params,root=root,tree=tree)}

    raw.sum.stat.y<-sum.stat(trait=true.trait$y,tree=tree)
    raw.sum.stat.x1<-sum.stat(trait=true.trait$x1,tree=tree)
    raw.sum.stat.x2<-sum.stat(trait=true.trait$x2,tree=tree)

    sim.model.trait<-array(NA,c(n,3,sims))
    y.sum.stat.array<-array(NA,c(12,sims))
    rownames(y.sum.stat.array)<-c("y.trait.mean","y.trait.sd","y.trait.median","y.trait.skewness","y.trait.kurtosis","y.pic.trait.mean","y.pic.trait.sd","y.pic.trait.mediam","y.pic.trait.skewness","y.pic.trait.kurtosis","y.pic.trait.K","y.pic.trait.lambda")
    x1.sum.stat.array<-array(NA,c(12,sims))
    rownames(x1.sum.stat.array)<-c("x1.trait.mean","x1.trait.sd","x1.trait.median","x1.trait.skewness","x1.trait.kurtosis","x1.pic.trait.mean","x1.pic.trait.sd","x1.pic.trait.mediam","x1.pic.trait.skewness","x1.pic.trait.kurtosis","x1.pic.trait.K","x1.pic.trait.lambda")
    x2.sum.stat.array<-array(NA,c(12,sims))
    rownames(x2.sum.stat.array)<-c("x2.trait.mean","x2.trait.sd","x2.trait.median","x2.trait.skewness","x2.trait.kurtosis","x2.pic.trait.mean","x2.pic.trait.sd","x2.pic.trait.mediam","x2.pic.trait.skewness","x2.pic.trait.kurtosis","x2.pic.trait.K","x2.pic.trait.lambda")

    model.params.array<-array(NA,c(length(true.model.params),sims))
    reg.params.array<-array(NA,c(length(true.reg.params),sims))
    for (simIndex in 1:sims) {
    #  simIndex<-1
      if(simIndex %%100==0){print(paste("repIndex= ",repIndex," simIndex= ",simIndex,sep=""))}
      sim.param<- simparam(names(true.param))# rexp(n=1,rate=alpha.y.rate)
      model.params<-true.model.params
      model.params[names(sim.param)]<-sim.param
      reg.params<-true.reg.params
      model.params.array[,simIndex]<-model.params#for record only
      reg.params.array[,simIndex]<-reg.params#for record only

      if(model=="oubmbm"){sim.trait <-oubmbmmodel(model.params=model.params,reg.params=reg.params,root=root,tree=tree)}
      if(model=="ououbm"){sim.trait <-ououbmmodel(model.params=model.params,reg.params=reg.params,root=root,tree=tree)}
      if(model=="oubmcir"){sim.trait <-oubmcirmodel(model.params=model.params,reg.params=reg.params,root=root,tree=tree)}
      if(model=="ououcir"){sim.trait <-ououcirmodel(model.params=model.params,reg.params=reg.params,root=root,tree=tree)}

      sim.model.trait[,1,simIndex]<-sim.trait$y
      sim.model.trait[,2,simIndex]<-sim.trait$x1
      sim.model.trait[,3,simIndex]<-sim.trait$x2
      y.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$y,tree=tree)
      x1.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x1,tree=tree)
      x2.sum.stat.array[,simIndex]<- sum.stat(trait=sim.trait$x2,tree=tree)
    }
    sim.sum.stat <- cbind(t(y.sum.stat.array),t(x1.sum.stat.array),t(x2.sum.stat.array))
    model.par.sim <- cbind(t(model.params.array),t(reg.params.array))
    rej <- abc(target=c(raw.sum.stat.y,raw.sum.stat.x1,raw.sum.stat.x2), param=model.par.sim, sumstat=sim.sum.stat, tol=tol, method="rejection"  )
    postTheta_sim <- data.frame(rej$unadj.values)
    colnames(postTheta_sim)<-c(names(true.model.params),names(true.reg.params))
    sample_params[repIndex,] <- postTheta_sim[,names(true.param)]
  }
 return(sample_params)
  }
