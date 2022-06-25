# if you are working local, setwd in simulation5 first!

source(file="functions/packages.R")

function.to.gen.data <- function(nDEP,nres,i){
  ## DEP describes the spatial dependence 
  DEP <- c("Exponential","Matern")
  Phi <- c(0.1)
  Nu <-  0.8
  Sigma <- 0.001
  sigma2 <- c(0.001)
  res <- list(c(25,10),c(40,20),c(55,25))    #res <- list(c(75,25),c(100,40))
  ## i is the identifier for this data generation
  set.seed(1*i) 
  
  ### ############################ ###
  ###  REAL VALUES FOR PARAMETERS  ###
  ### ############################ ###
  #Non-spatial parameters
  beta_0g=5.65
  beta_0r=5.6
  
  beta_1g=0.015
  beta_1r=0.01
  #Spatial structure parameters
  type <- DEP[nDEP]
  phi <- Phi
  nu <- Nu  # roughness parameter for error
  sigma2 <- sigma2
  tau2 <- 5000
  
  ### ############################ ###
  ###   Grid size and definition   ###
  ### ############################ ###
  # Global:
  nlats <- res[[nres]][2]
  nlons <- res[[nres]][2]
  # Regional:
  nlatw <- res[[nres]][1]
  nlonw <- res[[nres]][1]
  
  gridbase_global <- expand.grid(lon=seq(0,1,length.out = nlons),
                          lat=seq(0,1,length.out = nlats))
  gridbase_regional <- expand.grid(lon=seq(0,1,length.out = nlonw),
                                 lat=seq(0,1,length.out = nlatw))
  n <- dim(gridbase_regional)[1] #Number of spatial points regional
  ng <- dim(gridbase_global)[1] #Number of spatial points global
  x = st_as_sf(gridbase_global, coords = c("lon", "lat"), crs = 4326)
  y = st_as_sf(gridbase_regional, coords = c("lon", "lat"), crs = 4326)
  assign = st_nn(y, x, k = 1, parallel = 4)
  index<-cbind(unlist(assign))
  k <- 1 #Observations through time 
  
  ### ############################ ###
  ###   Random Field Generator     ###
  ### ############################ ###
  rExpMat_regional <- function(n,coords,type,phi,nu=nu,sigma){
    if(type=='Exponential'){
      m <- stationary.cov(coords,Covariance = type,
                          Distance = 'rdist.earth',
                          theta = phi)
    }
    if(type=='Matern'){
      m <- stationary.cov(coords,Covariance = type,
                          Distance = 'rdist.earth',
                          theta = phi,nu=nu)
    }
    return(m*sigma)
  }
  
  ### ############################ ###
  ### Spatial parameter generation ###
  ### ############################ ###
  beta0.cov <- rExpMat_regional(n,gridbase_regional,type = type,phi=phi,nu=nu,sigma=Sigma)
  #Nugget effect generation
  epsilon_g <- rnorm(ng*k, 0, sqrt(sigma2)) ### nugget global
  epsilon_gr <- epsilon_g[index]# translate epsilon_g to regional resolution
  gamma_r <- rnorm(n*k, 0, sqrt(1/tau2)) ### nugget regional
  
  ### ############################ ###
  ###  Cov and dependent variable  ###
  ### ############################ ###
  
  X1g <- rgamma(ng*k,2,1)
  X1g.r <- X1g[index]              #asingn cov value to regional points.
  
  beta_0w <- crossprod(chol(beta0.cov),matrix(rnorm(n), nrow=n))
  
  # Model T(w) - T(s)
  logTw = beta_0r + beta_0w + beta_1r * X1g.r + gamma_r + epsilon_gr
  logTs = beta_0g + beta_1g * X1g.r + epsilon_gr
  Y <- logTw-logTs
  Tw <- exp(logTw)
  Ts <- exp(logTs)
  dataset<-tibble(Y=as.vector(Y), Tw=Tw, Ts=Ts, X1=X1g.r) 
  
  ### ############################ ###
  ###        Save data sets        ###
  ### ############################ ###
  #Spatial structure 
  hh <- SpatialPointsDataFrame(coords = gridbase_regional,data = dataset)
  proj4string(hh) <- '+proj=longlat +datum=WGS84'
  save(dataset, hh, file=paste0("sim_data/dataset",
                                i,type,nres,
                                ".Rdata"))
}

#hist(dataset$Y)
