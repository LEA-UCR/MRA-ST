library(tidyverse)
library(sp)
library(sf)
library(gridExtra)


load('sim_res/chainTempSVCMaternNARCCAP_0.Rdata')
burn <- 500

chain <- MCMC_results$chain_result

betas <- chain$chain_beta[-(1:burn),]
colnames(betas) <- paste0('Beta',0:(dim(betas)[2]-1))
betas <- data.frame(betas) %>% mutate(Index=1:n())

betas_plot <- betas %>% pivot_longer(-Index,names_to = 'Variable',values_to='Betas')

grafico_betas <- ggplot(data = betas_plot,mapping = aes(x = Index,y = Betas))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(betas)[2],scales = 'free')+
  theme_bw()


Phis <- chain$chain_Phi[-(1:burn),]
colnames(Phis) <- paste0('Phi',0:(dim(Phis)[2]-1))
Phis <- data.frame(Phis) %>% mutate(Index=1:n())
Phis_plot <- Phis %>% pivot_longer(-Index,names_to = 'Variable',values_to='Phi')

grafico_Phis <- ggplot(data = Phis_plot,mapping = aes(x = Index,y = Phi))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(betas)[2],scales = 'free')+
  theme_bw()

Nus <- chain$chain_Nu[-(1:burn),]
colnames(Nus) <- paste0('Nu',0:(dim(Nus)[2]-1))
Nus <- data.frame(Nus) %>% mutate(Index=1:n())
Nus_plot <- Nus %>% pivot_longer(-Index,names_to = 'Variable',values_to='Nu')

grafico_Nus <- ggplot(data = Nus_plot,mapping = aes(x = Index,y = Nu))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(betas)[2],scales = 'free')+
  theme_bw()

elements_A <- function(i){
  elements <- c(diag(chain$chain_A[[i]]),chain$chain_A[[i]][upper.tri(chain$chain_A[[1]])])
  return(elements)
}

As <- purrr::map(1:2000,~elements_A(.x))
As <- do.call(rbind,As)
As <- As[-(1:burn),]
#colnames(As) <- paste0('A',1:dim(As)[2])
colnames(As) <- c('A11','A22','A12')
As <- data.frame(As) %>% mutate(Index=1:n())

A_plot <- As %>% pivot_longer(-Index,names_to = 'Variable',values_to='A')

grafico_A <- ggplot(data = A_plot,mapping = aes(x = Index,y = A))+
  geom_line()+facet_wrap(vars(Variable),nrow = dim(As)[2]-1,scales = 'free')+
  theme_bw()


taus <- chain$chain_tau[-(1:burn)]
taus <- data.frame(Tau=taus) %>% mutate(Index=1:n())

grafico_tau <- ggplot(data = taus,mapping = aes(x = Index,y = Tau))+
  geom_line()+
  theme_bw()


## Prediction Y
variable_narccap <- 'Temp' # Temp/Prec

if(variable_narccap=='Temp'){
  load('./data_narccap/TStotal.RData')
  base_cruda <- TS_tot
  rm(TS_tot)
}else{
  load('./data_narccap/PRtotal.RData')
  base_cruda <- pr_tot
  rm(pr_tot)
}

base_recortada <- base_cruda %>% 
  filter(lat >= 20, lat <= 40, lon >= 240, lon <= 265)
rm(base_cruda)

base_recortada <- base_recortada %>%
  mutate(across(c(OMEGA,PSL,U,V),function(x) as.vector(scale(x))))

if(variable_narccap=='Temp'){
  datos_filt <- base_recortada %>% mutate(Y=ts) %>% 
    dplyr::select(Y,OMEGA,PSL,U,V) 
  coordenadas <- base_recortada %>% dplyr::select(lon,lat)
}else{
  datos_filt <- base_recortada %>% rename(Y=pr) %>% 
    dplyr::select(Y,PRECL,OMEGA,PSL,U,V) %>%
    mutate(Y=as.numeric(scale(log(Y))),
           PRECL=as.numeric(scale(log(PRECL))),
           PSL=as.numeric(scale(log(PSL))))
  coordenadas <- base_recortada %>% dplyr::select(lon,lat)
}
# if(variable_narccap=='Temp'){
#   datos_filt <- base_cruda %>% rename(Y=ts) %>% 
#     dplyr::select(Y,TREFHT,OMEGA,PSL,U,V) 
#   mY <- mean(log(datos_filt$Y))
#   sY <- sd(log(datos_filt$Y))
#   coordenadas <- base_cruda %>% dplyr::select(lon,lat)
# }else{
#   datos_filt <- base_cruda %>% rename(Y=pr) %>% 
#     dplyr::select(Y,PRECL,OMEGA,PSL,U,V)
#   mY <- mean(log(datos_filt$Y))
#   sY <- sd(log(datos_filt$Y))
#   coordenadas <- base_cruda %>% dplyr::select(lon,lat)
# }

dataset <- datos_filt

hh <- SpatialPointsDataFrame(coords = coordenadas,data = datos_filt)
proj4string(hh) <- '+proj=longlat +datum=WGS84'
bordes <- bbox(hh)


Ysample <- MCMC_results$Ysample+base_recortada$TREFHT
Ysample_m <- rowMeans(Ysample)
Ysample_s <- purrr::map_dbl(1:2469,~sd(Ysample[.,]))
Ysample_med <- purrr::map_dbl(1:2469,~median(Ysample[.,]))
hh$Yhat <- Ysample_m
hh$Ysd <- Ysample_s
hh$Ymed <- Ysample_med

hh_sf <- st_as_sf(hh)

plot(hh_sf['Y'])
plot(hh_sf['Yhat'])
plot(hh_sf['Ysd'])

#https://cengel.github.io/R-spatial/mapping.html
grafico1 <- ggplot(data = hh_sf)+geom_sf(aes(col = Y))+
  theme_bw()+
  scale_color_distiller(type = 'seq',palette = 'YlOrRd',direction = 1)

grafico2 <- ggplot(data = hh_sf)+geom_sf(aes(col = Yhat))+
  theme_bw()+
  scale_color_distiller(type = 'seq',palette = 'YlOrRd',direction = 1)

grafico3 <- ggplot(data = hh_sf)+geom_sf(aes(col = Ymed))+
  theme_bw()+
  scale_color_distiller(type = 'seq',palette = 'YlOrRd',direction = 1)


grafico4 <- ggplot(data = hh_sf)+geom_sf(aes(col = Ysd))+
  theme_bw()+
  scale_color_distiller(type = 'seq',direction = 1)

graficos_Yposterior <- grid.arrange(grafico1,grafico2,grafico3,grafico4)


hist(Ysample[1,])
abline(v = hh_sf$Y[1],col=2)

hist(Ysample[2,])
abline(v = hh_sf$Y[2],col=2)

hist(Ysample[3,])
abline(v = hh_sf$Y[3],col=2)


# Diagnostics

MSE <- sqrt(sum(purrr::map_dbl(1:length(hh_sf$Y),~(mean(Ysample[.,])-hh_sf$Y[.])^2)))
MSE_N <- MSE / mean(Ysample)
quantiles_d <- purrr::map_dfr(.x = 1:length(hh_sf$Y),.f = function(x) as.data.frame(t(quantile(Ysample[x,],probs=c(0.025,0.975)))))
colnames(quantiles_d) <- c('Lower','Upper')

IS=function(alpha,upper,lower,y){
  N=length(y)
  ISi=rep(0,N)
  for(i in 1:N){
    #show(i)
    if(y[i]<=lower[i]){
      ISi[i]=-2*alpha*(upper[i]-lower[i])-4*(lower[i]-y[i])
    }else{
      if(lower[i]<y[i] && y[i]<upper[i]){
        ISi[i]=-2*alpha*(upper[i]-lower[i])
      }else{
        ISi[i]=-2*alpha*(upper[i]-lower[i])-4*(y[i]-upper[i])
      }
    }
  }
  ISres=mean(ISi)
  ISres
}

IS_Y <- -IS(alpha = 0.05,upper = quantiles_d$Upper,lower = quantiles_d$Lower,y = hh_sf$Y)

