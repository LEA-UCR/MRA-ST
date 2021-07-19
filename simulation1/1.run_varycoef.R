
here::i_am("README.md")

####################################################################
######### Simulation options: model, i, type1, type2, nAMA #########
####################################################################

model <- "varycoef" # "MRA2" # varycoef
i <- 1 # simulation ID # from 1 to 10
DEP<- "Exponential" # "Matern"  ## types
type <- DEP  
nres <- 3 # or 2
nreps <- 50 # number of iterations for MCMC
taper <- 0.1
####################################################################

####################################################################
########################  Set up ###################################
####################################################################

datasetfile=paste0("sim_data/dataset",
                   i,type,nres,
                   ".Rdata")
load(datasetfile)
source('functions/packages.R')

#########################################
# hay que modificar de aqui en adelante #
#########################################
source("functions/1.MRA_resolution_general.R")
source('functions/covariances.R')
source('functions/likelihoodK_general.R')
source('functions/L_functions.R')
source('functions/L_functions_varycoef.R')
source('functions/functions_varycoef.R')
# 
nCov_f <- 2 # fixed betas
nCov_v <- 1 # spatially varying betas
nlevels_P <- ifelse(model=="MRA2",2,1) # MRA number for phi
# nlevels_A <- ifelse(model=="MRA2",2,1) # MRA number for A (betas)
aa_P<-gen_resolution(datasetfile,nCov_v,nlevels_P)
# aa_A<-gen_resolution(datasetfile,nCov_v,nlevels_A)
Qlist <- aa_P[[3]]
nn <- aa_P[[4]]
hh <- Qlist[[nn+2]]


locs <- st_coordinates(hh$geometry)
# 
# ####################################################################
# ##################### Metropolis Hastings ##########################
# ####################################################################
# 
# # initial values

beta <- c(1,1)
# taub <- 2
# sigma2 <- 1/taub
start_beta <- beta
# start_A2 <- A2
start_Phi <- c(3)
start_tau <- 1
N <- dim(hh)[1] 
# 
# # fixed
types <- c(type)
Nu <- rep(2, nCov_v)
A1 <- diag(nCov_v) ## A1

# #Data
Y <- hh$Y
covariates<- scale(dataset$X1)
X <- data.matrix(cbind(rep(1,N),covariates))
XR <- data.matrix(X[,c(1)]) #scale(X[,c(1)])
#XR <- scale(X[,c(2,3)])
# 
# ###################
# ## Main M-H loop ##
# ###################
# 
Gibbs_beta <- function(SigmaYinv,beta_n){
  Sigma_beta <- 1*diag(beta_n)
  Sigma_beta_inv <- Sigma_beta
  mu_beta <- c(1,1)
  
  Sigma_beta_inv_p <- t(X)%*%SigmaYinv%*%X+Sigma_beta_inv
  Sigma_beta_p <- solve(Sigma_beta_inv_p)
  mu_beta_p <- Sigma_beta_p %*% (t(X)%*%SigmaYinv%*%Y+Sigma_beta_inv%*%mu_beta)
  
  Sigma_beta_p <- forceSymmetric(Sigma_beta_p)
  beta_sample <- mvtnorm::rmvnorm(1,mean = mu_beta_p,sigma = as.matrix(Sigma_beta_p))
  return(beta_sample)
}
proposalfunction_Phi <- function(chain,i,sd_Phi){
  sd <- sd_Phi
  if (is.null(dim(chain)[1])){
    mu <- chain
  }else{
    mu <- chain[i,]
  }
  Yn <- c(rtruncnorm(1,a = 0,b = 10,mean = mu[1],sd = sd[1]))#,
  #rtruncnorm(1,a = 0.1,b = 20,mean = mu[2],sd = sd[2]))
  #  Yn <- c(2000,2000)
  return(Yn)
}
# proposalfunction_A2 <- function(chain,i,a_n_1){
#   #https://stats.stackexchange.com/questions/77038/covariance-matrix-proposal-distribution
#   Sigma <- chain[[i]]
#   mean_A <- Sigma[upper.tri(Sigma)]
#   lmean_A <- length(mean_A)
#   prop_A <- rtmvnorm(1,mean = mean_A,sigma = (1/a_n_1)*diag(lmean_A),
#                      lower = rep(-1,lmean_A),upper = rep(0,lmean_A))
#   prop_A2 <- diag(dim(Sigma)[1])
#   prop_A2[upper.tri(prop_A2)] <- prop_A2[lower.tri(prop_A2)] <- prop_A
#   return(prop_A2)
# }
proposalfunction_tau <- function(chain,i,sd_tau){
  #tauprop <- rgamma(1,shape = chain[i]/sd_tau,scale = sd_tau)
  tauprop <- rtruncnorm(1,a = 0,b = 5,mean = chain[i],sd = sd_tau)
  #tauprop <- 1
  return(tauprop)
}



run_metropolis_MCMC <- function(start_beta,#start_A2,
                                start_Phi,start_tau,iterations,types
                                ,taper=0.1){
  chain_beta <- array(dim = c(iterations+1,length(start_beta)))
  chain_beta[1,] <- start_beta
  #chain_A2 <- list()
  #chain_A2[[1]] <- start_A2
  #p <- dim(start_A2)[1]
  chain_Phi <- array(dim = c(iterations+1,length(start_Phi)))
  chain_Phi[1,] <- start_Phi
  chain_tau <- rep(0,iterations+1)
  chain_tau[1] <- start_tau
  
  ###################varycoef specification

  control <- SVC_mle_control(extract_fun = T,
                             tapering = taper,
                             init = c(chain_Phi[1],1,
                                      chain_tau[1],
                                      chain_beta[1,]
                             ),
                             #lower= rep(1e-6,5),
                             lower= c(rep(1e-6,4),-5),
                             upper = rep(100,5))
  
  VC.fit <- SVC_mle(y = Y, X = X, W = XR, locs = locs,
                    control = control)
  ###################varycoef likelihood evaluation
  like_res_start<- do.call(n2LL.my,
                           c(list(x = control$init), VC.fit$args))
  
  SigmaYinv_old <- like_res_start$SigmaYinv
  logSigmaYdet_old <- like_res_start$logSigmaYdet
  
  ##############
  
  sd_Phi <- c(1)
  #a_n_1 <- 100
  sd_tau <- 0.05
  Ysample <- matrix(0,nrow = dim(SigmaYinv_old)[1],ncol = iterations)
  
  for (i in 1:iterations){
    show(i)
    tic()
    ## Gibbs Beta###
    
    chain_beta[i+1,] <- Gibbs_beta(SigmaYinv_old,dim(chain_beta)[2])
    
    m2loglike_old <- likelihood_no_MRA(chain_beta[i+1,],SigmaYinv_old,
                                       logSigmaYdet_old,Y,X)
    proposal_Phi <- proposalfunction_Phi(chain_Phi[c(1:i),],i,sd_Phi)
    #proposal_A2 <- proposalfunction_A2(chain_A2,i,a_n_1)
    proposal_tau <- proposalfunction_tau(chain_tau,i,sd_tau)
    
    ## MCMC Phi###
    fold_Phi <- f_P_tau_varycoef(  chain_beta[i+1,],A1,chain_Phi[i,],
                                   chain_tau[i],m2loglike_old,proposal_Phi,proposal_tau,
                                   sd_Phi,sd_tau,'old',types[1],types[2],taper=taper)$like
    fnew_Phi_p <- f_P_tau_varycoef(chain_beta[i+1,],A1, proposal_Phi,
                                   proposal_tau,m2loglike_old,chain_Phi[i,],chain_tau[i],
                                   sd_Phi,sd_tau,'new',types[1],types[2],taper=taper)
    
    fnew_Phi <- fnew_Phi_p$like
    
    probab_Phi <- min(0,fnew_Phi -fold_Phi)
    if(is.nan(fnew_Phi -fold_Phi)){
      probab_Phi <- -Inf
    }
    alpha_Phi <- exp(probab_Phi)
    if (log(runif(1)) <= probab_Phi){
      chain_Phi[i+1,] <- proposal_Phi
      chain_tau[i+1] <- proposal_tau
      SigmaYinv_old <- fnew_Phi_p$SigmaYinv
      logSigmaYdet_old <- fnew_Phi_p$logSigmaYdet
    }else{
      chain_Phi[i+1,] <- chain_Phi[i,]
      chain_tau[i+1] <- chain_tau[i]
    }
    d <- 1
    eta_n <- min(1,d*i^{-2/3})
    alpha_star <- 0.234
    sd_Phi[1] <- sqrt(sd_Phi[1]^2*(1+eta_n*(alpha_Phi-alpha_star)))
    #sd_Phi[2] <- sqrt(sd_Phi[2]^2*(1+eta_n*(alpha_Phi-alpha_star)))
    sd_Phi[1] <- min(sd_Phi[1],1)
    #sd_Phi[2] <- min(sd_Phi[2],5)
    sd_tau <- sqrt(sd_tau^2*(1+eta_n*(alpha_Phi-alpha_star)))
    sd_tau <- min(0.5,sd_tau)
    
    ## MCMC A###
    # fold_A2 <- f_A2(chain_beta[i+1,],chain_A2[[i]],chain_Phi[i+1,],
    #     chain_tau[i+1],m2loglike_old,proposal_A2,a_n_1,'old')$like
    # fnew_A2_p <- f_A2(chain_beta[i+1,],proposal_A2,chain_Phi[i+1,],
    #     chain_tau[i+1],m2loglike_old,chain_A2[[i]],a_n_1,'new')
    # fnew_A2 <- fnew_A2_p$like
    # 
    # probab_A2 <- min(0,fnew_A2 -fold_A2)
    # if(is.nan(fnew_A2 -fold_A2)){
    #   probab_A2 <- -Inf
    # }
    # alpha_A2 <- exp(probab_A2)
    # if (log(runif(1)) <= probab_A2){
    #   chain_A2[[i+1]] <- proposal_A2
    #   SigmaYinv_old <- fnew_A2_p$SigmaYinv
    #   logSigmaYdet_old <- fnew_A2_p$logSigmaYdet
    # }else{
    #   chain_A2[[i+1]] <- chain_A2[[i]]
    # }
    # d <- (dim(chain_A2[[1]])[1]+1)*dim(chain_A2[[1]])[1]/2
    # eta_n <- min(1,d*i^{-2/3})
    # alpha_star <- 0.234
    # a_n_1_inv <- (1/a_n_1)*(1+eta_n*(alpha_A2-alpha_star))
    # #a_n_1 <- max(1/a_n_1_inv,p+3)
    # a_n_1 <- max(1/a_n_1_inv,2)
    # show(c(alpha_A2,alpha_Phi))
    show(c(chain_beta[i+1,],chain_Phi[i+1,],sd_Phi,
           chain_tau[i+1],sd_tau))
    
    # Prediction
    muhat <- fnew_Phi_p$muhat
    SigmaYinv_old_f2 <- chol(forceSymmetric(SigmaYinv_old))
    Ysample[,i] <- as.matrix(SigmaYinv_old_f2%*%
                               rnorm(dim(SigmaYinv_old_f2)[1])+muhat)
    
    toc()
  }
  chain_result <- list(chain_beta=chain_beta,#,chain_A2=chain_A2,
                       chain_Phi=chain_Phi,chain_tau=chain_tau)
  return(list(chain_result=chain_result, Ysample=Ysample))
}
# 
print(paste("Model =",model,"/ Data =",datasetfile))
start_time <- Sys.time()
set.seed(100)
nreps = nreps
chain = run_metropolis_MCMC(start_beta,
                            #start_A2,
                            start_Phi,start_tau,nreps,types,
                            taper=taper)
end_time <- Sys.time()
total_time<-end_time-start_time
print(total_time)

##################
## hasta aquí ##
##################

labels = list(model,type,nres,nreps)
save(chain,labels, total_time,file=paste0("sim_res/chain",
                                          i,model,taper,type,nres,".Rdata"))

####################################################################
############################### Fin ################################
####################################################################



