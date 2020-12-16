setwd("~/MRA_ST/MRA2_mensual")
#setwd("/home/Emuladores/datos")
#getwd()

library(tidyverse)
library(sp)
library(sf)

#Temperature dataset ----

#define period
period <- 1981:1999

#Definir la región del monsón para las bases globales
blatitude <- c(20, 40)
blongitude <- c(240, 265)


load('ts_RegionalMensualMonson.Rdata')
ts_Regional <- ts_Regional %>% filter(Year %in% period)
load('/home/Emuladores/datos/TREFHT_Global-Shu.Rdata') 
TREFHT_Global <- TREFHT_Global %>% filter(Year %in% period)
TREFHT_Global <- TREFHT_Global %>% filter(lat >= blatitude[1],
                                          lat <= blatitude[2],
                                          lon >= blongitude[1],
                                          lon <= blongitude[2])
load('/home/Emuladores/datos/OMEGA_Global-Shu.Rdata')
OMEGA_Global <- OMEGA_Global %>% filter(Year %in% period)
OMEGA_Global <- OMEGA_Global %>% filter(lat >= blatitude[1],
                                          lat <= blatitude[2],
                                          lon >= blongitude[1],
                                          lon <= blongitude[2])
load('/home/Emuladores/datos/PSL_Global-Shu.Rdata')
PSL_Global <- PSL_Global %>% filter(Year %in% period)
PSL_Global <- PSL_Global %>% filter(lat >= blatitude[1],
                                          lat <= blatitude[2],
                                          lon >= blongitude[1],
                                          lon <= blongitude[2])
load('/home/Emuladores/datos/U_Global-Shu.Rdata')
U_Global <- U_Global %>% filter(Year %in% period)
U_Global <- U_Global %>% filter(lat >= blatitude[1],
                                          lat <= blatitude[2],
                                          lon >= blongitude[1],
                                          lon <= blongitude[2])
load('/home/Emuladores/datos/V_Global-Shu.Rdata')
V_Global <- V_Global %>% filter(Year %in% period)
V_Global <- V_Global %>% filter(lat >= blatitude[1],
                                          lat <= blatitude[2],
                                          lon >= blongitude[1],
                                          lon <= blongitude[2])
load('/home/Emuladores/datos/resolucion/altitude.Rdata')


pointsglobal_pre <- TREFHT_Global %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
pointsglobal <- st_as_sf(SpatialPoints(pointsglobal_pre))
st_crs(pointsglobal) <- 4326
gridglobalsf <- st_buffer(pointsglobal,dist = 0.7,endCapStyle = 'SQUARE')


pointsregional_pre <- ts_Regional %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
pointsregional_sp <- SpatialPoints(pointsregional_pre)
pointsregional <- st_as_sf(pointsregional_sp)
st_crs(pointsregional) <- 4326

indicesgrid <- st_intersects(pointsregional,gridglobalsf)
nulos <- unlist(purrr::map(1:2484,~is_empty(indicesgrid[[.]])))
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

save(TS_tot,file = 'TStotalmensual.RData')

#Precipitation dataset 

period <- 1981:1999

load('pr_RegionalMensualMonson.Rdata')
pr_Regional <- pr_Regional %>% filter(Year %in% period)

pointsregional_pre <- pr_Regional %>% select(lon,lat) %>% distinct(lon,lat)
pointsregional_sp <- SpatialPoints(pointsregional_pre)
pointsregional <- st_as_sf(pointsregional_sp)
st_crs(pointsregional) <- 4326

indicesgrid <- st_intersects(pointsregional,gridglobalsf)
nulos <- unlist(map(1:2484,~is_empty(indicesgrid[[.]])))
plot(pointsregional[!nulos,])
indicesgrid <- unlist(indicesgrid)

pointsregional <- pointsregional[!nulos,]

pointsregional_tb <- pointsregional %>% st_coordinates() %>%
  as.data.frame() %>% mutate(indicegrid = indicesgrid) 

colnames(pointsregional_tb)[1:2] <- c('lon','lat') 

pr_Regional <- pr_Regional %>% left_join(pointsregional_tb,by = c("lon", "lat")) %>%
  na.omit()

pr_tot <- pr_Regional %>% left_join(TREFHT_Global,by = c("Year", "Month", "indicegrid"))%>%
  left_join(OMEGA_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(PSL_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(U_Global,by = c("Year", "Month", "indicegrid")) %>%
  left_join(V_Global,by = c("Year", "Month", "indicegrid")) %>%
  select(Year,Month,pr=pr,lat=lat.x,lon=lon.x,indicegrid,
         TREFHT,OMEGA,PSL,U,V)

save(pr_tot,file = 'PRtotalmensual.RData')
