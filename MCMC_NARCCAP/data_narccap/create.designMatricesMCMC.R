library(tidyverse)
library(sp)
library(sf)

#Temperature dataset (November 1999)----
load('~/../Emuladores/datos/ts_Regional.Rdata')
ts_Regional <- ts_Regional %>% filter(Year>=1996)
load('~/../Emuladores/datos/TREFHT_Global.Rdata')
TREFHT_Global <- TREFHT_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/OMEGA_Global.Rdata')
OMEGA_Global <- OMEGA_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/PSL_Global.Rdata')
PSL_Global <- PSL_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/U_Global.Rdata')
U_Global <- U_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/V_Global.Rdata')
V_Global <- V_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/PRECL_Global.Rdata')
#PRECL_Global <- PRECL_Global %>% filter(Year>=1996)
load('~/../Emuladores/datos/resolucion/altitude.Rdata')


pointsglobal_pre <- TREFHT_Global %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
pointsglobal <- st_as_sf(SpatialPoints(pointsglobal_pre))
st_crs(pointsglobal) <- 4326
gridglobalsf <- st_buffer(pointsglobal,dist = 0.7,endCapStyle = 'SQUARE')


pointsregional_pre <- ts_Regional %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
pointsregional_sp <- SpatialPoints(pointsregional_pre)
pointsregional <- st_as_sf(pointsregional_sp)
st_crs(pointsregional) <- 4326

indicesgrid <- st_intersects(pointsregional,gridglobalsf)
nulos <- unlist(purrr::map(1:16100,~is_empty(indicesgrid[[.]])))
plot(pointsregional[nulos,])
indicesgrid <- unlist(indicesgrid)

pointsregional <- pointsregional[!nulos,]

pointsregional_tb <- pointsregional %>% st_coordinates() %>%
  as.data.frame() %>% mutate(indicegrid = indicesgrid) 

colnames(pointsregional_tb)[1:2] <- c('lon','lat') 

pointsglobal_tb <- pointsglobal %>% st_coordinates() %>%
  as.data.frame() %>% mutate(indicegrid=seq(1,dim(pointsglobal)[1]))
  
colnames(pointsglobal_tb)[1:2] <- c('lon','lat')   
  
TREFHT_Global <- TREFHT_Global %>% left_join(pointsglobal_tb,by = c("lon", "lat"))
OMEGA_Global <- OMEGA_Global %>% left_join(pointsglobal_tb,by = c("lon", "lat"))
PSL_Global <- PSL_Global %>% left_join(pointsglobal_tb,by = c("lon", "lat"))
U_Global <- U_Global %>% left_join(pointsglobal_tb,by = c("lon", "lat"))
V_Global <- V_Global %>%  left_join(pointsglobal_tb,by = c("lon", "lat"))
#PRECL_Global <- PRECL_Global %>%  left_join(pointsglobal_tb,by = c("lon", "lat"))
#altitude_Global <- pointsglobal_tb %>%  left_join(altitude,by = c("lon", "lat"))

ts_Regional <- ts_Regional %>% left_join(pointsregional_tb,by = c("lon", "lat")) %>%
  na.omit()

TS_tot <- ts_Regional %>% left_join(TREFHT_Global,by = c("Year", "Month", "indicegrid"))%>%
  left_join(OMEGA_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(PSL_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(U_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(V_Global,by = c("Year", "Month", "indicegrid")) %>%
  dplyr::select(Year,Month,ts,lat=lat.x,lon=lon.x,indicegrid,
         TREFHT,OMEGA,PSL,U,V)

save(TS_tot,file ='TStotal2.RData')

#Precipitation dataset (November 1999)----
# load('pr_Regional.Rdata')
# pr_Regional <- pr_Regional %>% filter(Year==1999,Month==11)
# 
# pointsregional_pre <- pr_Regional %>% select(lon,lat) %>% distinct(lon,lat)
# pointsregional_sp <- SpatialPoints(pointsregional_pre)
# pointsregional <- st_as_sf(pointsregional_sp)
# st_crs(pointsregional) <- 4326
# 
# indicesgrid <- st_intersects(pointsregional,gridglobalsf)
# nulos <- unlist(map(1:16100,~is_empty(indicesgrid[[.]])))
# plot(pointsregional[nulos,])
# indicesgrid <- unlist(indicesgrid)
# 
# pointsregional <- pointsregional[!nulos,]
# 
# pointsregional_tb <- pointsregional %>% st_coordinates() %>%
#   as.data.frame() %>% mutate(indicegrid = indicesgrid) 
# 
# colnames(pointsregional_tb)[1:2] <- c('lon','lat') 
# 
# pr_Regional <- pr_Regional %>% left_join(pointsregional_tb,by = c("lon", "lat")) %>%
#   na.omit()
# 
# pr_tot <- pr_Regional %>% left_join(PRECL_Global,by = c("Year", "Month", "indicegrid"))%>%
#   left_join(OMEGA_Global,by = c("Year", "Month", "indicegrid")) %>%
#   left_join(PSL_Global,by = c("Year", "Month", "indicegrid")) %>%
#   left_join(U_Global,by = c("Year", "Month", "indicegrid")) %>%
#   left_join(V_Global,by = c("Year", "Month", "indicegrid")) %>%
#   select(Year,Month,pr=ave_prpd,lat=lat.x,lon=lon.x,indicegrid,
#          PRECL,OMEGA,PSL,U,V)
# 
# save(pr_tot,file = 'PRtotal2.RData')
