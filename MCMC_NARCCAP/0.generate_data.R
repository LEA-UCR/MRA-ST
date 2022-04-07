library(tidyverse)
library(sf)
library(sp)

variable_narccap <- 'Temp' # Temp/Prec

if(variable_narccap=='Temp'){
  load('./data_narccap/TStotalmensual-Luis0122.RData')
  base_cruda <- TS_tot
  rm(TS_tot)
}else{
  load('./data_narccap/PRtotal.RData')
  base_cruda <- pr_tot
  rm(pr_tot)
}

base_recortada <- base_cruda %>% 
  filter(lat >= 20, lat <= 40, lon >= 240, lon <= 265) %>%
  filter(Year >= 1990)
rm(base_cruda)

#base_recortada <- base_recortada %>%
#  mutate(across(c(OMEGA,PSL,U,V),function(x) as.vector(scale(x))))


if(variable_narccap=='Temp'){
  #datos_filt <- base_recortada %>% mutate(Y=log(ts)-log(TREFHT)) %>% 
  #  dplyr::select(Year,Month,Y,PC1,PC2,PC3)
  datos_filt <- base_recortada %>% mutate(Y=log(ts)-log(TREFHT),
                                          lPREC_T = log(pr)-log(PRECL+1e-20)) %>% 
      dplyr::select(Year,Month,Y,OMEGA,U,V,ts,TREFHT,lPREC_T)
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
hh_sf <- st_as_sf(hh)
crsglobal <- CRS('+proj=longlat +datum=WGS84')

##ST_PCA
#http://homepage.ntu.edu.tw/~wenthung/R_Spatial/RLab_6.html
# library(spdep)
# 
# hh_pixel <- SpatialPixelsDataFrame(points = coordenadas,data = datos_filt,tolerance = 0.88)
# proj4string(hh_pixel) <- '+proj=longlat +datum=WGS84'
# hh_grid <- as(hh_pixel, "SpatialGridDataFrame")
# hh_nb <- cell2nb(hh_grid)
# W_hh <-  nb2mat(hh_nb)
# 
# hh_sf <- st_as_sf(hh)
# hh_sf_t <- hh_sf %>% filter(Year==1996,Month==1)
# 
# X_t <- hh_sf_t %>% dplyr::select(OMEGA,PSL,U,V) %>%
#   st_drop_geometry()
# pca_X_t = prcomp(X_t, scale. = TRUE)

#load('~/../Emuladores/PCA/index_stPCA.Rdata')
#load('~/../Emuladores/PCA/PCA_data.Rdata')

save(dataset, hh_sf, pointsglobal, file=paste0("data_narccap/dataset",
                              variable_narccap,"-Luis0122.Rdata"))
