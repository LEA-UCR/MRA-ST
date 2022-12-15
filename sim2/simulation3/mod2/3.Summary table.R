library(tidyverse)
library(sp)
library(sf)
library(gridExtra)
library(INLA)
library(spdplyr)

#valores reales
beta_0g=5.707;beta_0r=5.706#;beta_1g=0.2;beta_1r=0.15

beta0.real <- beta_0r-beta_0g
#beta1.real <- beta_1r-beta_1g
phi.real <- 5
#sigma.real <- 0.0003
tau.real <- 1/700000
rho.real <- 0.8
parametro.real <- c(beta0.real,phi.real,sigma.real,tau.real,rho.real)
parametro <- c("beta0","phi","sigma","tau","rho")

model <- "INLA" 
type <- "Matern" 

Sigma <- c(0.003,0.0003,0.00003)


nVAR <- 2
sigma.real <- Sigma[nVAR]
nres <- 3       # 1 or 2

Mesh <- "g"

coef.summary.list<- list()
prediction.MSE.summary.list<- list()
prediction.IS.summary.list<- list()
time.exec <- as.numeric()

for(i in 1:10){
  print(i)
  ##parameter estimation
  scenario<-paste0("sim_res_prediction/results",i,model,type,nVAR,nres,Mesh,"_prediction.Rdata")
  load(scenario)
  
  ev_rho<-!is.null(mod$marginals.hyperpar$`GroupRho for idx.PC0`)
  ev_rho
  
  time.exec[i] <- as.numeric(total_time, units="mins")
  
  betas.est <- mod$summary.fixed
  outputPC0.field <- inla.spde2.result(inla=mod,
                                       name="idx.PC0", spde=spde.spatial.vcm, do.transf=TRUE)
  phi.est <- unlist(inla.zmarginal(outputPC0.field$marginals.range.nominal[[1]]))
  sigma.est <-unlist(inla.zmarginal(outputPC0.field$marginals.variance.nominal[[1]]))
  
  if(ev_rho){
    rho.est <- unlist(inla.zmarginal(mod$marginals.hyperpar$`GroupRho for idx.PC0`))
  }
  
  base_grafico_nugget <- data.frame(inla.tmarginal(function(x) 1/x, mod$marginals.hyper$`Precision for the Gaussian observations`))
  
  tau.est <- unlist(inla.zmarginal(base_grafico_nugget))
  
  coef.summary <- rbind(betas.est,phi.est,sigma.est,tau.est,rho.est)
  row.names(coef.summary)[2:5]<-c("phi","sigma","tau","rho")
  coef.summary <- data.frame(parametro,parametro.real,coef.summary,index=i)
  coef.summary.list[[i]]<-coef.summary
  
  ##prediction
  
  load(paste0("sim_data/dataset",i,type,nVAR,nres,".Rdata"))
  ntime <- length(unique(hh$time))
  train.ntime <- floor(ntime*5/6)
  
  index.pred <- inla.stack.index(stk, "data")$data
  Resumen_fitted <- mod$summary.fitted.values[index.pred,]
  
  hh_sf <- st_as_sf(hh)
  hh_sf <- hh_sf %>% bind_cols(Resumen_fitted)
  hh_sf <- hh_sf %>% rename(Yhat = mean, Ysd = sd, Ymed = `0.5quant`, 
                            Y025 = `0.025quant`, Y975 = `0.975quant`)
  hh_sf <- hh_sf %>% mutate(That = exp(log(Ts)+Yhat), 
                            Tmed = exp(log(Ts)+Ymed),
                            T025 = exp(log(Ts)+Y025),
                            T975 = exp(log(Ts)+Y975),
                            TR = T975-T025)
  
  MSE_time_ent <- hh_sf %>% 
    mutate(dif2 = (Tw-That)^2, train = ifelse(time<=train.ntime,"train","test")) %>%
    st_drop_geometry() %>%
    group_by(train) %>% summarise(MSE = mean(dif2))
  
  MSE_time_ent <- data.frame(MSE_time_ent,index=i)
  prediction.MSE.summary.list[[i]] <-MSE_time_ent
  
  IS_time_ent <- hh_sf %>% #filter(Year==1999) %>% 
    mutate(IS = (T975-T025)+
             (2/0.05)*(T025-Tw)*(Tw<T025)+ 
             (2/0.05)*(Tw-T975)*(Tw>T975),
           train = ifelse(time<=train.ntime,"train","test")) %>%
    st_drop_geometry() %>%
    group_by(train) %>% summarise(IS95 = mean(IS))
  
  IS_time_ent <- data.frame(IS_time_ent,index=i)
  prediction.IS.summary.list[[i]] <-IS_time_ent
  
}
coef.summary.list1 <- coef.summary.list %>% bind_rows(.) %>% mutate(dif2 = (mean-parametro.real)^2/abs(parametro.real),
                                                                    IS = ((X0.975quant-X0.025quant)+
                                                                            (2/0.05)*(X0.025quant-parametro.real)*(parametro.real<X0.025quant)+ 
                                                                            (2/0.05)*(parametro.real-X0.975quant)*(parametro.real>X0.975quant))/abs(parametro.real) ) %>% 
  group_by(parametro) %>% summarise( posterior.mean=mean(mean),posterior.sd=mean(sd) , 
                                     MSEn = mean(dif2),ISn=mean(IS))


prediction.MSE.summary.list1 <- prediction.MSE.summary.list %>% 
  bind_rows(.) %>% group_by(train) %>% 
  summarise(mean(MSE))

prediction.IS.summary.list1 <- prediction.IS.summary.list %>% bind_rows(.) %>% 
  bind_rows(.) %>% group_by(train) %>% 
  summarise(mean(IS95))

prediction.table<-cbind(prediction.MSE.summary.list1,prediction.IS.summary.list1)

time.exec <- as.data.frame(time.exec)
colnames(time.exec) <- paste0("AR",nres)

write_csv(coef.summary.list1, file = paste0("output/metrica_AR",nVAR,nres,".csv"))
write_csv(prediction.table, file = paste0("output/metrica_pred_AR",nVAR,nres,".csv"))
write_csv(time.exec, file = paste0("output/time_exec_AR",nVAR,nres,".csv"))

