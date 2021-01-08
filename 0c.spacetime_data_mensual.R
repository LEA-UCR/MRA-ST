library(tidyverse)
library(sf)
library(sp)
library(spacetime)


setwd("~/MRA_ST/MRA2_mensual")
getwd()
#setwd("/home/Emuladores/datos")
#getwd()

variable_narccap <- 'Prec' # Temp/Prec
variable_narccap <- 'Temp'

if(variable_narccap=='Temp'){
  load('TStotalmensual.RData')
  base_cruda <- TS_tot
  rm(TS_tot)
}else{
  load('PRtotalmensual.RData')
  base_cruda <- PR_tot
  rm(PR_tot)
}

base_cruda <- base_cruda %>% arrange(Year,Month,lat,lon) #ordenar de acuerdo a la fecha y luego lat, lon.

if(variable_narccap=='Temp'){
  datos_filt <- base_cruda %>% rename(Y=ts) %>% 
    dplyr::select(Y,TREFHT,OMEGA,PSL,U,V) # %>% mutate(Y=log(Y),TREFHT=log(TREFHT))
  coordenadas.data <- base_cruda %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
  coordenadas.data$lon <- coordenadas.data$lon-360
  coordenadas <- SpatialPoints(coordenadas.data)
  tiempo <- base_cruda %>% 
    dplyr::mutate(tiempo=as.Date(with(base_cruda, paste(Year, Month,"15",sep="-")), "%Y-%m-%d")) %>%
    dplyr::select(tiempo) %>% distinct(tiempo)
  tiempo<- tiempo$tiempo
}else{
  datos_filt <- base_cruda %>% rename(Y=pr) %>% 
    dplyr::select(Y,TREFHT,OMEGA,PSL,U,V) # %>% mutate(Y=log(Y),TREFHT=log(TREFHT))
  coordenadas.data <- base_cruda %>% dplyr::select(lon,lat) %>% distinct(lon,lat)
  coordenadas.data$lon <- coordenadas.data$lon-360
  coordenadas <- SpatialPoints(coordenadas.data)
  tiempo <- base_cruda %>% 
    dplyr::mutate(tiempo=as.Date(with(base_cruda, paste(Year, Month, "15",sep="-")), "%Y-%m-%d")) %>%
    dplyr::select(tiempo) %>% distinct(tiempo)
  tiempo<- tiempo$tiempo
}

dataset <- datos_filt

hh<-STFDF(sp=coordenadas, time=tiempo, data=dataset)
class(hh)
proj4string(hh) <- '+proj=longlat +datum=WGS84'
bordes <- bbox(hh)

save(dataset, hh, file=paste0(variable_narccap,"_mensual.Rdata"))

