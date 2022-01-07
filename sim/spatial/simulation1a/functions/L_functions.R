## L functions 


f_P_tau <- function(beta,A2,Phi,Sigma,tau,m2loglike_old,
                    Phi_m,tau_m,sd_Phi,sd_tau,status,type1,type2){
  if(status=='new'){
    MRA_num <- aa_P[[4]]
    m2loglike_prop <- likelihoodMRA(beta,A2,Phi,Nu,nCov_v,
                                    tau=tau,sigma2=Sigma,model,MRA_num,Y,X,XR,aa_P,types)
    m2logv <- m2loglike_prop$m2logv
    SigmaYinv <- m2loglike_prop$SigmaYinv
    logSigmaYdet <- m2loglike_prop$logSigmaYdet
    muhat <- m2loglike_prop$muhat
  }else{
    m2logv <- m2loglike_old
  }
  #logpriorphi   <- sum(dunif(Phi,0,10,log=TRUE)) 
  logpriorphi <- d.pc.prior(Phi=Phi,sigma=Sigma,U_Phi=0.15,a_Phi=0.5,U_sigma=0.25,a_sigma=0.5,log=TRUE)
  
  logpriortau <- dinvgamma(tau,shape = 2,scale = 100,log = T)
  #logpriornu    <- dunif(Nu,0,3,log=TRUE)    #
  #TYPE HERE
  if(type1=='Exponential'){
    logprior <- logpriorphi+logpriortau
  }else{
    logprior <- logpriorphi+logpriortau
  }
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

# f_A2 <- function(beta,A2,Phi,tau,m2loglike_old,
#                  A2_m,a_n_1,status,type1,type2){
#   if(status=='new'){
#     MRA_num <- aa_A[[4]]
#     m2loglike_prop <- likelihoodMRA(beta,A2,Phi,Nu,nCov_v,
#                                     tau=1,sigma2=tau,model,nn,Y,X,XR,aa_A,types)
#     m2logv <- m2loglike_prop$m2logv
#     SigmaYinv <- m2loglike_prop$SigmaYinv
#     logSigmaYdet <- m2loglike_prop$logSigmaYdet
#     muhat <- m2loglike_prop$muhat
#   }else{
#     m2logv <- m2loglike_old
#   }
#   p <- dim(A2)[1] 
#   if(p==1){
#     logpriorA2 <- dgamma(A2,shape = 10,scale = 1,log = T)
#     logproposal <- dgamma(A2,shape = a_n_1/2,
#                           scale = 2*A2_m/a_n_1,log = T)
#   }else{
#     #browser()
#     ut_A2 <- A2[upper.tri(A2)]
#     ut_A2m <- A2_m[upper.tri(A2_m)]
#     lmean_A <- length(ut_A2)
#     logpriorA2 <- dunif(ut_A2,min = -1,max = 0)
#     logproposal <- dtmvnorm(ut_A2,mean = ut_A2m,
#                             sigma = (1/a_n_1)*diag(lmean_A),
#                             lower = rep(-1,lmean_A),
#                             upper = rep(0,lmean_A),log = T)
#   }
#   logprior <- logpriorA2
#   like <- -(m2logv/2) +logprior + logproposal
#   
#   if(status=='new'){
#     f_result <- list(like=as.numeric(like),m2logv=m2logv,
#                      SigmaYinv=SigmaYinv,logSigmaYdet=logSigmaYdet,
#                      muhat=muhat)
#   }else{
#     f_result <- list(like=as.numeric(like),m2logv=m2logv)
#   }
#   return(f_result)
# }