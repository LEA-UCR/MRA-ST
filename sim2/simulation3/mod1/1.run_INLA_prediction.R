
here::i_am("README.md")


for(i in 4:10){
####################################################################
######### Simulation options: model, i, type1, type2, nAMA #########
####################################################################
  
model <- "INLA" # "MRA2" # varycoef
#i <- 1 # simulation ID # from 1 to 10
DEP <- "Matern"    #"Exponential" # "Matern"  ## types
type <- DEP  
Nu <- 1

nVAR <-1
nres <-1 # or 2 or 3
Mesh <- "g"  

####################################################################

####################################################################
########################  Set up ###################################
####################################################################

datasetfile=paste0("sim_data/dataset",
                   i,type,nVAR,nres,
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
#source('functions/L_functions.R')
#source('functions/L_functions_varycoef.R')
#source('functions/functions_varycoef.R')
#source('functions/MLE_computation_varycoef.R')
#source('functions/PCprior.R')
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
#points(locations, col='red')
#points(locations_r, col='blue',pch=16)

#rho.vcm <- c(,0.9)
#sigma.vcm <- c(5,0.9) 
rho.vcm <- c(3,0.5)
sigma.vcm <- c(0.0003,0.5)

alpha <- Nu+1
spde.spatial.vcm = inla.spde2.pcmatern(mesh=mesh, 
                                       alpha=alpha,
                                       prior.range=rho.vcm,
                                       prior.sigma=sigma.vcm)  
A.PC0 = inla.spde.make.A(mesh, loc=locations_r, 
                         weights = rep(1,N),
                         group = hh_pred$time)
idx.PC0 = inla.spde.make.index("idx.PC0", spde.spatial.vcm$n.spde,
                               n.group=ntime)

df.covar.expanded = data.frame(intercept=rep(1,N)) ##

## INLA 

stk <- inla.stack(data=list(Y=hh_pred$Y), tag='data',
                  A=list(A.PC0,1),
                  effects=list(idx.PC0=idx.PC0,
                               df.covar.expanded))

#Create data structure for prediction
# stk.pred <- inla.stack(data=list(Y=NA), tag='data.pred',
#                   A=list(A.PC0,1),
#                   effects=list(idx.PC0=idx.PC0,
#                                df.covar.expanded))

#Join stack
#join.stk <- inla.stack(stk, stk.pred)
join.stk <- stk

#Fit model
# form <- Y ~  0+ intercept +  
#                 PC1 +
#                 f(idx.PC0, model = spde.spatial.vcm)
# 

form <- Y ~  0+ intercept +  
                #PC1 +
                f(idx.PC0, model = spde.spatial.vcm, group = idx.PC0.group,
                  control.group = list(model = "iid"))



mod= inla(form,
            data = inla.stack.data(join.stk),
            control.predictor = list(A = inla.stack.A(join.stk),compute = TRUE),
            control.compute = list(dic=TRUE,cpo=TRUE), 
            verbose=TRUE)

end_time <- Sys.time()
total_time<-end_time-start_time
print(total_time)

##################
## hasta aquí ##
##################

labels = list(model,type,nres)
save(mod,spde.spatial.vcm,stk,labels, total_time,file=paste0("sim_res_prediction/results",
                                          i,model,type,nVAR,nres,"_prediction.Rdata"))

####################################################################
############################### Fin ################################
####################################################################


}
