
here::i_am("README.md")


for(i in c(1:1)){
  ####################################################################
  ######### Simulation options: model, i, type1, type2, nAMA #########
  ####################################################################
  
  model <- "INLA" # "MRA2" # varyco2ef
  #i <- 1 # simulation ID # from 1 to 10
  DEP <- "Matern"    #"Exponential" # "Matern"  ## types
  type <- DEP  
  Nu <- ifelse(DEP=="Exponential",1/2,0.8)
  nres <-1 # or 2 or 3
  Mesh <- "g"  
#  nrho <- 2
  ####################################################################
  
  ####################################################################
  ########################  Set up ###################################
  ####################################################################
  
  datasetfile=paste0("sim_data/dataset",
                     i,type,nres,
                     ".Rdata")
  load(datasetfile)
  source('functions/packages.R')
  library(INLA)
  #########################################
  # hay que modificar de aqui en adelante #
  #########################################
  source("functions/1.MRA_resolution_general.R")
  source('functions/covariances.R')
  source('functions/likelihoodK_general.R')
  
  
  # 
  nCov_f <- 1 # fixed betas
  nCov_v <- 1 # spatially varying betas
  nlevels_P <- ifelse(model=="MRA2",2,1) # MRA number for phi
  # nlevels_A <- ifelse(model=="MRA2",2,1) # MRA number for A (betas)
  aa_P<-gen_resolution(datasetfile,nCov_v,nlevels_P)
  # aa_A<-gen_resolution(datasetfile,nCov_v,nlevels_A)
  Qlist <- aa_P[[3]]
  nn <- aa_P[[4]]
  hh <- Qlist[[nn+2]]
  N <- dim(hh)[1] 
  
  locations_r <- as.matrix(hh$coords_r)
  locations_g <- unique(as.matrix(hh$coords_g))
  
  if(Mesh=="r"){
    locations <- locations_r
  } else{
    locations <- locations_g
  }
  
  ntime <- length(unique(hh$time))
  train.ntime <- floor(ntime*5/6)
  #prediction (last year)
  hh_pred <- hh %>% mutate(Y = ifelse(time<=train.ntime,Y,NA))

  # 
  print(paste("Model =",model,"/ Data =",datasetfile))
  
  start_time <- Sys.time()
  set.seed(100)
  
  
  
  # run INLA ----------------------------------------------------------------
  
  
  ##MESH##
  expected.range = 5
  max.edge = expected.range/5  
  bound.outer = expected.range/5
  loc.domain = rbind(c(-2,-2),c(-2,22),c(22,-2),c(22,22))
  mesh =
    inla.mesh.2d(loc=locations,
                 loc.domain = loc.domain,
                 #offset = c(max.edge, bound.outer),
                 max.edge=c(3, 3) * max.edge)
  #plot(mesh, asp=1)
  #points(hh$coords_r, col='red')
  
  rho.vcm <- c(3,0.5)
  sigma.vcm <- c(0.0000001,0.5)
  
  alpha <- Nu+1
  spde.spatial.vcm = inla.spde2.pcmatern(mesh=mesh, 
                                         alpha=alpha,
                                         prior.range=rho.vcm,
                                         prior.sigma=sigma.vcm)  
  
  A.PC0 = inla.spde.make.A(mesh, loc=locations_r,
                           group = hh_pred$time, 
                           weights = rep(1,N))
  
  idx.PC0 = inla.spde.make.index("idx.PC0", spde.spatial.vcm$n.spde,
                                 n.group=ntime)
  
  df.covar.expanded = data.frame(intercept= rep(1,N)) ##
  
  ## INLA 
  
  stk <- inla.stack(data=list(Y=hh_pred$Y), tag='data',
                    A=list(A.PC0,1),
                    effects=list(#idx.PC0=idx.PC0,
                      idx.PC0 = idx.PC0,
                      df.covar.expanded))
  
  #Create data structure for prediction
  # stk.pred <- inla.stack(data=list(Y=NA), tag='data.pred',
  #                   A=list(A.PC0,1),
  #                   effects=list(idx.PC0=idx.PC0,
  #                                df.covar.expanded))
  
  #Join stack
  #join.stk <- inla.stack(stk, stk.pred)
  join.stk <- stk
  
  
  rprior <- list(theta = list(prior = "pccor1", param = c(0.5, 0.9)))
  
  
  #Fit model
  # form <- Y ~  0+ intercept +  
  #                 PC1 +
  #                 f(idx.PC0, model = spde.spatial.vcm)
  # 
  # form <- Y ~  0+ intercept +  
  #   PC1 +
  #   f(idx.PC0, model = spde.spatial.vcm,group = idx.PC0.group)
  
  form <- Y ~  0+ intercept +
    # f(idx.PC0, model = spde.spatial.vcm,group = idx.PC0.group,
    # control.group = list(model = "ar1", hyper = rprior))+
    f(idx.PC0, model = spde.spatial.vcm,group = idx.PC0.group,
      control.group = list(model = "ar1", hyper = rprior))
  
  
  mod= inla(form,
            data = inla.stack.data(join.stk),
            control.predictor = list(A = inla.stack.A(join.stk),compute = TRUE),
            control.compute = list(dic=TRUE,cpo=TRUE), 
            control.inla = list(diagonal = 100, strategy = "gaussian",
                                int.strategy = "eb"),
            verbose=TRUE)
  
  end_time <- Sys.time()
  total_time<-end_time-start_time
  print(total_time)
  
  ##################
  ## hasta aquí ##
  ##################
  
  labels = list(model,type,nres,Mesh)
  save(mod,spde.spatial.vcm,stk,labels, total_time,file=paste0("sim_res_prediction/results",
                                                               i,model,type,nres,Mesh,"_prediction.Rdata"))
  
  ####################################################################
  ############################### Fin ################################
  ####################################################################
  
  
}
