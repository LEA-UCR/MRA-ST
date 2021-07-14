args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  variable_narccap <- 'Temp' # Temp or Prec
  type<-"NSpatial"
  model<-"SVC"
  analysis<-"M3"
  datasetfile=paste0("data_narccap/dataset",
                     variable_narccap,".Rdata")
} else {
  variable_narccap <-args[1]
  type<-args[2]
  model<-args[3]
  analysis<-args[4]
  datasetfile=paste0("data_narccap/dataset",
                     variable_narccap,".Rdata")
}

# i<-1:100
# type<-'Exponential', "Matern"
# model<-'SVC', "SVI"
# analysis<-"M1: likelihood", "M2: FSA", "M3: MRA2"

source("1.MRA_resolution_general.R")
source('covariances.R')
source('likelihoodK_general.R')
library(MCMCpack)
library(truncdist)
library(invgamma)
library(Matrix)
library(mvtnorm)
library(truncnorm)
library(CholWishart)
library(tmvtnorm)
library(tictoc)

nCov_f <- 3
nCov_v <- 2

nlevels_P <- 2
nlevels_A <- 2

aa_P<-gen_resolution(datasetfile,nCov_v,nlevels_P)
aa_A<-gen_resolution(datasetfile,nCov_v,nlevels_A)
#Tm <- aa[[1]]
#TSm <- aa[[2]]
#Qlist <- aa[[3]]
#nn<-aa[[4]]
#hh <- Qlist[[nn+2]]

Qlist <- aa_A[[3]]
nn <- aa_A[[4]]
hh <- Qlist[[nn+2]]

#library(gstat)
#vv <- variogram(Y~1,data = hh,cutoff=2200)
#plot(vv)
#### Metropolis Hastings

# initial values



beta <- c(0,rep(1,nCov_f-1))
taub <- 2
sigma2 <- 0.5
A2 <- diag(nCov_v)


start_beta <- beta
start_A2 <- A2
start_tau <- 0.1

N <- dim(hh)[1]
#npar <- length(startvalue)

# fixed
types <- rep(type,nCov_v)

#Data
Y <- hh$Y
#X <- data.matrix(st_drop_geometry(hh %>% mutate(interc = 1) %>% 
#                                    dplyr::select(interc,TREFHT,OMEGA,PSL,U,V)))
#XR <- scale(X[,c(2,3,4,5,6)])
if(variable_narccap == 'Temp'){
  X <- data.matrix(st_drop_geometry(hh %>% mutate(interc = 1) %>% 
                                      dplyr::select(interc,TREFHT,PSL)))
}else{
  X <- data.matrix(st_drop_geometry(hh %>% mutate(interc = 1) %>% 
                                      dplyr::select(interc,PRECL,PSL)))
}

if(model=="SVC"){
  #XR <- scale(X[,c(2,3)])
  XR <- X[,c(2,3)]
}else{
  XR <- X[,c(1)]
}

#XR <- X[,c(1)]


##################
# L functions    #
##################

##################
# Main M-H  loop #
##################

Gibbs_beta <- function(SigmaYinv,beta_n){
  #Sigma_beta <- 100*diag(beta_n)
  Sigma_beta <- diag(c(0.05,0.05,0.01))^2
  Sigma_beta_inv <- Sigma_beta
  mu_beta <- c(0,rep(1,beta_n-1))
  
  Sigma_beta_inv_p <- t(X)%*%SigmaYinv%*%X+Sigma_beta_inv
  Sigma_beta_p <- solve(Sigma_beta_inv_p)
  mu_beta_p <- Sigma_beta_p %*% (t(X)%*%SigmaYinv%*%Y+Sigma_beta_inv%*%mu_beta)
  
  Sigma_beta_p <- forceSymmetric(Sigma_beta_p)
  beta_sample <- rmvnorm(1,mean = mu_beta_p,sigma = as.matrix(Sigma_beta_p))
  return(beta_sample)
}




run_metropolis_MCMC <- function(start_beta,start_A2,start_Phi,start_Nu,start_tau,iterations){
  chain_beta <- array(dim = c(iterations+1,length(start_beta)))
  chain_beta[1,] <- start_beta
  
  SigmaYinv_old <- diag(N)

  Ysample <- matrix(0,nrow = dim(SigmaYinv_old)[1],ncol = iterations)
  
  for (i in 1:iterations){
    show(i)
    tic()
    ## Gibbs Beta###
    chain_beta[i+1,] <- Gibbs_beta(SigmaYinv_old,dim(chain_beta)[2])
    
    show(chain_beta[i+1,])
    # Prediction
    muhat <- X%*%chain_beta[i+1,]
#    SigmaYinv_old_f2 <- chol(forceSymmetric(SigmaYinv_old))
    Ysample[,i] <- as.matrix(rnorm(dim(SigmaYinv_old)[1])+muhat)
    
    toc()
  }
  chain_result <- list(chain_beta=chain_beta,chain_A2=NULL,chain_Phi=NULL,chain_Nu=NULL,chain_tau=NULL)
  return(list(chain_result=chain_result,Ysample=Ysample))
}

print(paste("Model =",analysis,"/ Data =",datasetfile))

start_time <- Sys.time()

set.seed(100)
MCMC_results = run_metropolis_MCMC(start_beta,start_A2,start_Phi,start_Nu,start_tau,2000)
#burnIn = 200

end_time <- Sys.time()

print(end_time-start_time)

save(MCMC_results, file=paste0("sim_res/chain",variable_narccap,model,type,'NARCCAP',".Rdata"))

