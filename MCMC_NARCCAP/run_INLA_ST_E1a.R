library(INLA)
library(sf)
library(tidyverse)
library(tictoc)

variable_narccap <- 'Temp' # Temp or Prec
datasetfile=paste0("data_narccap/dataset",
                   variable_narccap,"-Luis0122.Rdata")
load(datasetfile)

hh_sf <- hh_sf %>% filter(Year>=1990)

fechas <- hh_sf %>% st_drop_geometry() %>%  
  dplyr::select(Year,Month) %>%
  distinct() %>% arrange(Year,Month) %>%
  mutate(ind = 1:n())


hh_sf <- hh_sf %>% left_join(fechas,by = c('Year','Month'))
timesn <- max(hh_sf$ind)

hh_est <- hh_sf %>% mutate(Y = ifelse(Year<1999,Y,NA))

#hh_extract <- hh_extract %>% mutate(Y=ifelse(Year==1999,NA,Y))
#hh_extract <- hh
#locations <- st_coordinates(hh_extract$geometry)
locations <- st_coordinates(pointsglobal)
colnames(locations) <- c('lon','lat')


##MESH##
expected.range = 1500
max.edge = expected.range/5  


mesh =
  inla.mesh.2d(loc=unique(locations),
               max.edge=c(.6, 5) * max.edge)
plot(mesh, asp=1)
points(locations, col='red')

rho.vcm <- c(700,0.5)
sigma.vcm <- c(0.1/0.31,0.01) 

spde.spatial.vcm = inla.spde2.pcmatern(mesh=mesh, 
                                       alpha=2,
                                       prior.range=rho.vcm,
                                       prior.sigma=sigma.vcm)  

idx.PC0 = inla.spde.make.index("idx.PC0", n.spde = spde.spatial.vcm$n.spde,
                               n.group = timesn)

locations_r <- st_coordinates(hh_est$geometry)
colnames(locations_r) <- c('lon','lat')
A.PC0 = inla.spde.make.A(mesh, loc=locations_r,group = hh_est$ind)

# locations_rp <- st_coordinates(hh_pred$geometry)
# colnames(locations_rp) <- c('lon','lat')
# A.PC0p = inla.spde.make.A(mesh, loc=locations_rp,group = hh_pred$ind)


idx.PC1 = inla.spde.make.index("idx.PC1", n.spde = spde.spatial.vcm$n.spde,
                               n.group = timesn)
A.PC1 = inla.spde.make.A(mesh, loc=locations_r,group = hh_est$ind, 
                         weights = hh_est$OMEGA)

#idx.PC1 = inla.spde.make.index("idx.PC1", n.spde = spde.spatial.vcm$n.spde)
#A.PC1 = inla.spde.make.A(mesh, loc=locations_r, 
#                          weights = hh_est$OMEGA)



# idx.PC2 = inla.spde.make.index("idx.PC2", n.spde = spde.spatial.vcm$n.spde,
#                                n.group = timesn)
# A.PC2 = inla.spde.make.A(mesh, loc=locations,group = hh_extract$ind, 
#                          weights = hh_extract$U)
# idx.PC3 = inla.spde.make.index("idx.PC3", n.spde = spde.spatial.vcm$n.spde,
#                                n.group = timesn)
# A.PC3 = inla.spde.make.A(mesh, loc=locations,group = hh_extract$ind, 
#                          weights = hh_extract$V)

df.covar.expanded = data.frame(intercept=1,
                               OMEGA = hh_est$OMEGA,
                               U = hh_est$U,
                               V = hh_est$V,
                               Month = factor(hh_est$Month))

# df.covar.expanded_pred = data.frame(intercept=1,
#                                OMEGA = hh_pred$OMEGA,
#                                U = hh_pred$U,
#                                V = hh_pred$V,
#                                Month = factor(hh_pred$Month))

## INLA 

stk.est <- inla.stack(data=list(Y=hh_est$Y), tag='est',
                      A=list(A.PC0,A.PC1,1),
                      effects=list(idx.PC0=idx.PC0,
                                   idx.PC1 = idx.PC1,
                                   df.covar.expanded))

# stk.pred <- inla.stack(data=list(Y=NA), tag='pred',
#                       A=list(A.PC0p,1),
#                       effects=list(idx.PC0=idx.PC0, 
#                                    df.covar.expanded_pred))



# stk <- inla.stack(data=list(Y=hh_extract$Y), tag='est',
#                   A=list(A.PC1, A.PC2, A.PC3,1),
#                   effects=list(idx.PC1=idx.PC1,
#                                idx.PC2=idx.PC2,
#                                idx.PC3=idx.PC3, 
#                                df.covar.expanded))

rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))

#https://avianecologist.com/tag/inla/

formula <- Y ~ 0 + intercept + OMEGA +
  #  f(idx.PC0, model = spde.spatial.vcm,group = idx.PC0.group,
  #    control.group = list(model = "iid", hyper = rprior))+
  f(idx.PC1, model = spde.spatial.vcm,group = idx.PC1.group,
    control.group = list(model = "iid", hyper = rprior))


# formula <- Y ~ 0 + intercept +  OMEGA + U + V +
#   f(idx.PC1, model = spde.spatial.vcm,group = idx.PC1.group,
#     control.group = list(model = "ar1", hyper = rprior))+ 
#   f(idx.PC2, model = spde.spatial.vcm,group = idx.PC2.group,
#     control.group = list(model = "ar1", hyper = rprior))+
#   f(idx.PC3, model = spde.spatial.vcm,group = idx.PC3.group,
#     control.group = list(model = "ar1", hyper = rprior))

tic()
m.ex2= inla(formula,
            data = inla.stack.data(stk.est),
            control.predictor = list(A = inla.stack.A(stk.est),compute = TRUE),
            control.compute = list(dic=TRUE,cpo=TRUE),
            control.inla = list(diagonal = 100, strategy = "gaussian",
                                int.strategy = "eb"),
            verbose=TRUE)


# m.ex2= inla(formula,
#            data = inla.stack.data(stk),
#            control.predictor = list(A = inla.stack.A(stk),compute = TRUE),
#            control.compute = list(dic=TRUE,cpo=TRUE),
#            control.inla = list(diagonal = 100, strategy = "gaussian",
#                                int.strategy = "eb"),
#            verbose=TRUE)
# 
# m.ex3= inla(formula,
#             data = inla.stack.data(stk),
#             control.predictor = list(A = inla.stack.A(stk),compute = TRUE),
#             control.compute = list(dic=TRUE,cpo=TRUE),
#             control.mode = list(result = m.ex2, restart = TRUE),
#             verbose=TRUE)
toc(log = T)

time_elapsed <- tic.log(format = T)
save(m.ex2,spde.spatial.vcm, stk.est, hh_sf,time_elapsed,
     file = './results/results_INLA_ST_E1a.RData')

