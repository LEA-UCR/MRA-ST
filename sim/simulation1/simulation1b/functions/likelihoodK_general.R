
# Constructs W matrices according to K&G-2020
W_maker <- function(nCov,phi,nu,A2,aa,sigma2, types){ # agregar types
  Tm <- aa[[1]]
  TSm <- aa[[2]]
  Qlist <- aa[[3]]
  nn<- aa[[4]]
  hh <- Qlist[[nn+2]]
  
  Wlist <- list() 
  WSlist <- list()
  # Initialize matrices
  for(m in 0:nn){
    Wlist[[m+1]] <- list()
    WSlist[[m+1]] <- list()
    for(l in 0:m){
      Wlist[[m+1]][[l+1]] <- list()
    }
  }
  
  for(m in 0:nn){
    #show(m)
    for(l in 0:m){
      if(l==0){ 
        #Formulas 3.10 and 3.11 of K&G-2020
        k <- 0
        Wlist[[m+1]][[l+1]][[k+1]] <- Matrix(cExpMat_mult(Qlist[[m+1]],
                            Qlist[[l+1]],A2,nCov,
                            types,range=phi,nu=nu, 
                            alpha=acau, beta=bcau,sigma2=sigma2),sparse = T)
        WSlist[[m+1]][[k+1]] <- Matrix(cExpMat_mult(Qlist[[nn+2]],
                            Qlist[[m+1]],A2,nCov,
                            types,range=phi,nu=nu, 
                            alpha=acau, beta=bcau,sigma2=sigma2),sparse = T)
      }else{
        Wlist[[m+1]][[l+1]][[1]] <- Matrix(cExpMat_mult(Qlist[[m+1]],
                            Qlist[[l+1]],A2,nCov,
                            types,range=phi,nu=nu, 
                            alpha=acau, beta=bcau,sigma2=sigma2),sparse = T)
        for(k in 0:(l-1)){
          Wlist[[m+1]][[l+1]][[k+2]] <- (Wlist[[m+1]][[l+1]][[k+1]]-
                                           Wlist[[m+1]][[k+1]][[k+1]]%*% 
                                           solve(Wlist[[k+1]][[k+1]][[k+1]])%*%
                                           t(Wlist[[l+1]][[k+1]][[k+1]]))*
                                           Tm[[m+1]][[l+1]][[k+2]]
          
          WSlist[[m+1]][[k+2]] <- (WSlist[[m+1]][[k+1]]-
                                     WSlist[[k+1]][[k+1]]%*%
                                     solve(Wlist[[k+1]][[k+1]][[k+1]])%*%
                                     t(Wlist[[m+1]][[k+1]][[k+1]]))*
                                     TSm[[m+1]][[k+2]]
        }
      }
    }
  }
  Lambda_list <- purrr::map(1:(nn+1),~return(Wlist[[.]][[.]][[.]])) 
  # Returns \Lambda_m=W_m^{m,m} Precision matrix of MRA basis
  B_list <- purrr::map(1:(nn+1),~return(WSlist[[.]][[.]])) 
  # Returns B_m=W_m^{S,m} Projection matrix.
  matrices_r <- list(Lambda=Lambda_list,B=B_list)
  return(matrices_r)
}

likelihoodMRA <- function(beta,A2,Phi,Nu,nCov,tau,sigma2,
                          model,nMRA,Y,X,XR,aa,types){
  XR <- as.matrix(XR) #design matrix (only spatially-dependent covariates)
  XX <- bdiag(purrr::map(1:dim(XR)[1],~t(XR[.,]))) 
  #Augmented design matrix (only spatially-dependent covariates)
  muhat <- X%*%beta # Mean of Y (design matrix includes all covariates)
  Y <- as.matrix(Y) 
  muhat <- as.matrix(muhat)
  
  matrices <- W_maker(nCov,Phi,Nu,A2,aa,sigma2,types)
  Lambda_m <- matrices$Lambda #Precision matrix of MRA basis
  B_m <- matrices$B # Projection matrix of observations on the knot space (Q)
  
  quad_SigmaY <- 0
  for(k in nMRA:0){
    #show(k)
    if(k==nMRA){
      Sigma_w <- B_m[[k+1]]%*%solve(Lambda_m[[k+1]])%*%t(B_m[[k+1]]) 
      # XSigmae <- XX%*%Sigma_w%*%t(XX)+(1/tau) * diag(dim(XX)[1])
      XSigmae <- XX%*%Sigma_w%*%t(XX)+(1/tau) * Diagonal(dim(XX)[1])
      XSigmae <- forceSymmetric(XSigmae)
      SigmaYinv <- chol2inv(chol(XSigmae)) #Precision matrix of Y
      logSigmaYdet <- log(det(XSigmae)) #log-determinant of covariance matrix of Y
    }else{
      SigmaYinv_old <- SigmaYinv
      XB_m <- XX %*% B_m[[k+1]]
      YXB_m <- SigmaYinv_old %*% XB_m
      #Precision matrix according to formula 2.2 (preprint) SWM Formula
      SigmaYinv <- SigmaYinv_old - YXB_m %*% 
        solve(Lambda_m[[k+1]]+ t(YXB_m) %*% XB_m) %*% t(YXB_m)
      #Determinant of Sigma_Y according to formula 2.3 (preprint) SWM Formula
      logSigmaYdet <- logSigmaYdet+log(det(Lambda_m[[k+1]]+t(YXB_m) %*% XB_m))-
        log(det(Lambda_m[[k+1]]))
    }
  }
  m2logv <- logSigmaYdet+t(Y-muhat)%*%SigmaYinv%*%(Y-muhat) # -2*loglikelihood
  lista_res <- list(m2logv=m2logv,SigmaYinv=SigmaYinv,
                    logSigmaYdet=logSigmaYdet,muhat=muhat)
  return(lista_res)
}

likelihood_no_MRA <- function(beta,SigmaYinv,logSigmaYdet,Y,X){
  muhat <- X%*%beta 
  Y <- as.matrix(Y) 
  muhat <- as.matrix(muhat)
  m2logv <- logSigmaYdet+t(Y-muhat)%*%SigmaYinv%*%(Y-muhat) # -2*loglikelihood
  return(m2logv)
}