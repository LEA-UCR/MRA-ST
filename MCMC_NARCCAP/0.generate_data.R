library(tidyverse)
library(sf)
library(sp)

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
  datos_filt <- base_recortada %>% mutate(Y=ts-TREFHT) %>% 
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

dataset <- datos_filt

hh <- SpatialPointsDataFrame(coords = coordenadas,data = datos_filt)
proj4string(hh) <- '+proj=longlat +datum=WGS84'
bordes <- bbox(hh)

save(dataset, hh, file=paste0("data_narccap/dataset",
                              variable_narccap,"_2.Rdata"))