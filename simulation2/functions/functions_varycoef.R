## objective_functions.R
## n2LL modification from varycoef::n2LL()
## modification: adding Sigma,...,

n2LL.my <- function(
  x, cov_func, outer.W, y, X, W,
  mean.est = NULL,
  taper = NULL,
  pc.dens = NULL,
  Rstruct = NULL,
  profile = TRUE
) {
  
  q <- dim(W)[2]
  p <- dim(X)[2]
  n <- length(y)
  
  # compute covariance matrices
  Sigma <- Sigma_y(x[1:(2*q+1)], cov_func, outer.W, taper = taper)
  
  # calculate Cholesky-Decompisition
  # powerboost function
  # spam pivot check
  if (is.spam(Sigma)) {
    cholS <- spam::chol.spam(Sigma, Rstruct = Rstruct)
  } else {
    cholS <- chol(Sigma)
  }
  
  
  ## profile LL
  mu <- if (profile) {
    # compute mu(theta)...
    if (is.null(mean.est)) {
      # ...using GLS
      GLS_chol(cholS, X, y)
    } else {
      # ...or set by some constant
      mean.est
    }
  } else {
    # given directly by objective parameters
    x[1 + 2*q + 1:p]
  }
  
  res <- y - X %*% mu
  
  ## quadratic form of residual, i.e., t(res) %*% Sigma^-1 %*% res
  quad_res <- if (is.matrix(cholS)) {
    as.numeric(crossprod(solve(t(cholS), res)))
  } else {
    as.numeric(crossprod(spam::forwardsolve(cholS, res,
                                            transpose = TRUE,
                                            upper.tri = TRUE)))
  }
  
  
  ## Modification
  
  m2logv.varycoef<-n * log(2 * pi) +
    2 * c(determinant(cholS)$modulus) +
    quad_res + pc_penalty(x, q, pc.dens)
  
  m2logv.modified<- 2 * c(determinant(cholS)$modulus) +
    quad_res + pc_penalty(x, q, pc.dens)
  
  SigmaYinv = solve(Sigma)
  
  logSigmaYdet = 2 * c(determinant(cholS)$modulus)
  
  muhat <- X %*% mu
  
  return(list(m2logv.varycoef=m2logv.varycoef,m2logv=m2logv.modified,SigmaYinv=SigmaYinv,
         logSigmaYdet=logSigmaYdet,muhat=muhat))
}

Sigma_y <- function(x, cov_func, outer.W, taper = NULL) {
  n <- nrow(outer.W[[1]])
  q <- length(outer.W)
  
  if (is.null(taper)) {
    # with no tapering computations are done on matrix objects
    Sigma <- matrix(0, nrow = n, ncol = n)
    
    for (k in 1:q) {
      # first argument: range, second argument: variance / sill
      Cov <- cov_func(x[2*(k-1) + 1:2])
      
      Sigma <- Sigma + (
        Cov * outer.W[[k]]
      )
    }
    
    nug <- if (n == 1) {
      x[2*q+1]
    } else {
      diag(rep(x[2*q+1], n))
    }
    
    return(Sigma + nug)
  } else {
    # With tapering computations are done on spam objects.
    # Specifically, due to their fixed structure and since we are only
    # pair-wise adding and multiplying, on the spam entries themselves
    
    stopifnot(
      all(sapply(outer.W, is.spam))
    )
    
    # construct a sparse matrix with 0 values as future entries
    # for k = 1
    Sigma <- outer.W[[1]] * cov_func(x[1:2])
    
    # if q > 1, build covariance matrix using components of other GPs
    if (q > 1) {
      for (k in 2:q) {
        Cov <- do.call(cov_func, list(c(x[2*(k-1) + 1:2], 0)))
        
        Sigma <- Sigma + (Cov * outer.W[[k]])
      }
    }
    
    options(spam.trivalues = TRUE)
    
    nug <- if (n == 1) {
      x[2*q+1]
    } else {
      spam::diag.spam(rep(x[2*q+1], n))
    }
    
    # Sigma <- Sigma * taper
    # add lower tri. cov-matrices up and mirror them to get full cov-matrix
    # due to spam::nearest.dist design
    
    return(spam::lower.tri.spam(Sigma) +
             spam::t.spam(Sigma) +
             nug)
  }
}
pc_penalty <- function(x, q, pc.dens) {
  
  if (is.null(pc.dens)) {
    return(0)
  } else {
    cov_vars <- x[1:(2*q)]
    
    pc.priors <- sapply(1:q, function(j) {
      pc.dens(cov_vars[2*(j-1) + 1:2])
    })
    
    return(sum(pc.priors))
  }
}
