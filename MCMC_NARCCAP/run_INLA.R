args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  variable_narccap <- 'Temp' # Temp or Prec
  type<-"Exponential"
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
library(INLA)
library(sf)
library(MCMCpack)
library(truncdist)
library(invgamma)
library(Matrix)
library(mvtnorm)
library(truncnorm)
library(CholWishart)
library(tmvtnorm)
library(tictoc)


nCov_f <- 4
nCov_v <- 3

nlevels_P <- 2
nlevels_A <- 2

aa_P<-gen_resolution(datasetfile,nCov_v,nlevels_P)
aa_A<-gen_resolution(datasetfile,nCov_v,nlevels_A)

Qlist <- aa_A[[3]]
nn <- aa_A[[4]]
hh <- Qlist[[nn+2]]

hh_extract <- hh %>% filter(Year==1999,Month==11)
locations <- st_coordinates(hh_extract$geometry)

##MESH##
expected.range = 1500
max.edge = expected.range/5  

mesh =
  inla.mesh.2d(loc=locations,
               max.edge=c(.6, 5) * max.edge)
#plot(mesh, asp=1)
#points(locations, col='red')

rho.vcm <- c(700,0.5)
sigma.vcm <- c(0.1/0.31,0.01) 

spde.spatial.vcm = inla.spde2.pcmatern(mesh=mesh, 
                                       alpha=2,
                                       prior.range=rho.vcm,
                                       prior.sigma=sigma.vcm)  
A.PC1 = inla.spde.make.A(mesh, loc=locations, 
                          weights = hh_extract$PC1)
idx.PC1 = inla.spde.make.index("idx.PC1", spde.spatial.vcm$n.spde)
A.PC2 = inla.spde.make.A(mesh, loc=locations, 
                         weights = hh_extract$PC2)
idx.PC2 = inla.spde.make.index("idx.PC2", spde.spatial.vcm$n.spde)
A.PC3 = inla.spde.make.A(mesh, loc=locations, 
                         weights = hh_extract$PC3)
idx.PC3 = inla.spde.make.index("idx.PC3", spde.spatial.vcm$n.spde)

df.covar.expanded = data.frame(intercept=1,
                               PC1 = hh_extract$PC1,
                               PC2 = hh_extract$PC2,
                               PC3 = hh_extract$PC3)


## INLA 

stk <- inla.stack(data=list(Y=hh_extract$Y), tag='est',
                  A=list(A.PC1, A.PC2, A.PC3,1),
                  effects=list(idx.PC1=idx.PC1,
                               idx.PC2=idx.PC2,
                               idx.PC3=idx.PC3, 
                               df.covar.expanded))

m.ex2= inla(Y ~ 0 + intercept +  PC1 + PC2 + PC3 +
              f(idx.PC1, model = spde.spatial.vcm)+ 
              f(idx.PC2, model = spde.spatial.vcm)+
              f(idx.PC3, model = spde.spatial.vcm),
            data = inla.stack.data(stk),
            control.predictor = list(A = inla.stack.A(stk),compute = TRUE),
            control.compute = list(dic=TRUE,cpo=TRUE), 
            verbose=TRUE)

#Efectos fijos
m.ex2$summary.fixed

# Nugget y varianzas
m.ex2$summary.hyperpar[1,]
inla.emarginal(function(x) 1/x, m.ex2$marginals.hyper[[1]])

# Rango y  varianza PC1 (Matern)
outputPC1.field <- inla.spde2.result(inla=m.ex2,
                                   name="idx.PC1", spde=spde.spatial.vcm, do.transf=TRUE)

inla.emarginal(function(x) x,
               outputPC1.field$marginals.variance.nominal[[1]])

inla.emarginal(function(x) x,
               outputPC1.field$marginals.range.nominal[[1]])

# Rango y  varianza PC2 (Matern)
outputPC2.field <- inla.spde2.result(inla=m.ex2,
                                     name="idx.PC2", spde=spde.spatial.vcm, do.transf=TRUE)

inla.emarginal(function(x) x,
               outputPC2.field$marginals.variance.nominal[[1]])

inla.emarginal(function(x) x,
               outputPC2.field$marginals.range.nominal[[1]])

# Rango y  varianza PC3 (Matern)
outputPC3.field <- inla.spde2.result(inla=m.ex2,
                                     name="idx.PC3", spde=spde.spatial.vcm, do.transf=TRUE)

inla.emarginal(function(x) x,
               outputPC3.field$marginals.variance.nominal[[1]])

inla.emarginal(function(x) x,
               outputPC3.field$marginals.range.nominal[[1]])
