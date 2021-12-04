## L functions 


f_P_tau_varycoef <- function(beta,A2,Phi,Sigma,tau,m2loglike_old,
                    Phi_m,tau_m,sd_Phi,sd_tau,status,type1,taper=0.1){
  if(status=='new'){
    
    control <- likelihood.control(extract_fun = T,
                                  cov.name= type.varycoef,
                                  tapering = taper,
                                  init = c(Phi,Sigma,
                                           tau,
                                           beta))
    VC.fit <- MLE_computation(y = Y,
                              X = X,
                              locs = locs,
                              W = XR,
                              control = control)
    
    m2loglike_prop<- do.call(n2LL.my,
                             c(list(x = control$init), VC.fit$args))
    
    m2logv <- m2loglike_prop$m2logv
    SigmaYinv <- m2loglike_prop$SigmaYinv
    logSigmaYdet <- m2loglike_prop$logSigmaYdet
    muhat <- m2loglike_prop$muhat
  }else{
    m2logv <- m2loglike_old
  }
  #logpriorphi <- sum(dunif(Phi,0,10,log=TRUE)) 
  logpriorphi <- d.pc.prior(Phi=Phi,sigma=Sigma,U_Phi=0.15,a_Phi=0.5,U_sigma=0.25,a_sigma=0.5,log=TRUE)
  logpriortau <- dinvgamma(tau,shape = 2,scale = 100,log = T)
  #logpriornu    <- dunif(Nu,0,3,log=TRUE)    #
  #TYPE HERE
  #if(type1=='Exponential'){
    logprior <- logpriorphi+logpriortau
  #}else{
  #  logprior <- logpriorphi+logpriornu+logpriortau
  #}
  logproposal <- log(dtruncnorm(Phi[1],a = 0,
                                b = 3,mean = Phi_m[1],sd = sd_Phi[1]))
  #  logproposal <- logproposal + log(dtruncnorm(Phi[2],
  #                                              a = 500,b = 3500,mean = Phi_m[2],sd = sd_Phi[2]))
  #logproposal <- logproposal + dgamma(tau,shape = tau_m/sd_tau,scale = sd_tau)
  logproposal <- logproposal + log(dtruncnorm(tau,
                                              a = 0,b = 3,mean = tau_m,sd = sd_tau))
  like <- -(m2logv/2) +logprior + logproposal
  
  if(status=='new'){
    f_result <- list(like=as.numeric(like),m2logv=m2logv,
                     SigmaYinv=SigmaYinv,logSigmaYdet=logSigmaYdet,
                     muhat=muhat)
  }else{
    f_result <- list(like=as.numeric(like),m2logv=m2logv)
  }
  return(f_result)
}

