

# Pendientes --------------------------------------------------------------

#copy from SVC_mle_control.default()
likelihood.control <- function(
  cov.name = c("exp", "sph", "mat", "wend1", "wend2"),
  taper.name= "wend1",
  tapering = NULL,
  init = NULL,
  extract_fun = FALSE,
  dist = list(method = "euclidean"),
  ...
) {
  
  list(
    cov.name = match.arg(cov.name),
    taper.name= taper.name,
    tapering = tapering,
    init = init,
    extract_fun = extract_fun,
    dist = dist,
    ...
  )
}

# MLE_computation dependencies --------------------------------------------

MLE.cov.func <- function(cov.name,nu=3/2) {
  if (is.character(cov.name)) {
    cov.func <- switch(cov.name,
                       "exp" = spam::cov.exp,
                       "mat" = function(h, theta) {
                         spam::cov.mat(h, theta = c(theta, nu))},
                       "sph" = spam::cov.sph,
                       "wend1" = spam::cov.wend1,
                       "wend2" = spam::cov.wend2,
                       stop("Cov.name argument not defined."))
  } else if (is.function(cov.name)) {
    cov.func <- cov.name
  } else {
    stop("Cov.name argument neither character, nor covariance function.")
  }
  return(cov.func)
}


own_dist <- function(x, y = NULL, taper = NULL, ...) {
  
  d <- if (is.null(taper)) {
    # without tapering
    if (is.null(y)) {
      # no cross distances
      as.matrix(do.call(
        dist,
        c(list(x = x, diag = TRUE, upper = TRUE), ...)
      ))
    } else {
      # cross distances
      as.matrix(do.call(
        spam::nearest.dist,
        c(list(x = x, y = y, delta = 1e99), ...)
      ))
    }
  } else {
    # with tapering
    if (is.null(y)) {
      # no cross distances
      do.call(
        spam::nearest.dist,
        c(list(x = x, delta = taper), ...)
      )
    } else {
      # cross distances
      do.call(
        spam::nearest.dist,
        c(list(x = x, y = y, delta = taper), ...)
      )
    }
  }
  # return output
  d
}


get_taper <- function(taper.name, d, tapering) {
  switch(
    taper.name,
    "sph" = spam::cov.sph(d, c(tapering, 1, 0)),
    "wend1" = spam::cov.wend1(d, c(tapering, 1, 0)),
    "wend2" = spam::cov.wend2(d, c(tapering, 1, 0))
  )
}

# MLE_computation ---------------------------------------------------------

MLE_computation <- function(y, X, locs, W,
                            control) {
  ## -- set important dimensions ----
  # number random effects and fixed effects
  q <- dim(W)[2]
  p <- dim(X)[2]

  # define distance matrix
  d <- do.call(
    own_dist,
    c(list(x = locs, taper = control$tapering), control$dist)
  )
  
  ## -- define distance matrices, covariance functions, and taper matrix -----
  # get covariance function

  #select nu parameter for each random coefficient
  nu<-control$init[3*(1:q)]
  
  # get covariance function
  raw.cov.func <- MLE.cov.func(control$cov.name,nu=nu)
  
  # covariance function
  cov.func <- function(x) raw.cov.func(d, x)
  
  Rstruct <- NULL
  # tapering?
  if (is.null(control$tapering)) {
    taper <-NULL
    outer.W <- lapply(1:q, function(k) W[, k]%o%W[, k])
  } else {
    taper <- get_taper(control$taper.name, d, control$tapering)
    outer.W <- lapply(1:q, function(k) {
      (W[, k]%o%W[, k]) * taper
    })
    
  }
  
    
    obj_fun <- function(x, ...)
      n2LL.my(x, ...)
    args <- list(
      cov_func = cov.func,
      outer.W  = outer.W,
      y        = y,
      X        = X,
      W        = W,
      taper    = taper,
      Rstruct  = Rstruct
    )
    
    return(list(
      obj_fun = obj_fun,
      args    = args
    ))
}


# control <- likelihood.control(extract_fun = T,
#                               cov.name= type.varycoef,
#                               taper.name= "wend1",
#                               tapering = taper,
#                               init = c(chain_Phi[1],
#                                        1,
#                                        chain_Nu[1],
#                                        chain_tau[1],
#                                        chain_beta[1,])
# )

# 
# ML_estimate <- MLE_computation(y = Y,
#                                X = X,
#                                locs = locs,
#                                W = XR,
#                                control = control)
# ML_estimate$obj_fun
# str(ML_estimate$args)
# 
# like_res_start<- do.call(n2LL.my,
#                          c(list(x = control$init), ML_estimate$args))
# like_res_start$m2logv.varycoef
# like_res_start$m2logv
# like_res_start$logSigmaYdet
# like_res_start$muhat

