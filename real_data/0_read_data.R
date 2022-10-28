library("R.matlab")
library("ggplot2")
library("tidyverse")
library("lubridate")

library("gstat")
library("maps")
library("grid")
library("gridExtra")

data_matlab <- readMat('./data/tmeanwusa19502001.mat')

data_geo <- data.frame(lat=as.vector(data_matlab$LatN),
                   lon=as.vector(data_matlab$LonW),
                   id=1:length(data_matlab$LonW))

data <- data.frame(data_matlab$tmeanwusa19502001)
colnames(data) <- 1:length(data_matlab$LonW)
data <- data %>% mutate_all(~ifelse(is.nan(.), NA, .))


#definir la variable tiempo
data <- data %>% mutate(year = rep(1950:2001,each=12*31),
                month = rep(rep(1:12,each= 31),52),
                day = rep(1:(12*31),times= 52))

names(data)

data_final <- data  %>%
  pivot_longer(cols = '1':'784', 
               names_to = "id", 
               values_to = "temp") %>% 
  mutate(id = as.numeric(id)) %>%   
  left_join(data_geo, by = "id") 


data_month <- data_final %>% group_by(year, month, id, lat, lon) %>%
  summarise(temp = mean(temp,na.rm = TRUE)) %>% ungroup()

coordenadas <- data_month %>% dplyr::select(lon,lat)

real_data <- SpatialPointsDataFrame(coords = coordenadas,data = data_month)
proj4string(real_data) <- '+proj=longlat +datum=WGS84'
bordes <- bbox(real_data)
data_month_sf <- st_as_sf(real_data)
crsglobal <- CRS('+proj=longlat +datum=WGS84')

save(data_month,data_month_sf,file="./data/real_data.Rdata")

