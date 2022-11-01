library(nngeo)
library(ggplot2)
library(dplyr)

load(file="./data/real_data.Rdata")

#monsoon
lat.mon=c(21.45146, 39.99363)
lon.mon=c(-119.99590 , -95.00983)


# Ilustración con datos de 1999 -------------------------------------------

#drop_na()

#convert C to K.
data_month <- data_month %>% mutate(temp = temp + 273.15)

plot_1999 <- data_month %>% filter(year == 1999) %>%
  ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = temp),size = 2.5)+
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = map_data("world"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  facet_wrap(~month, nrow = 3) +        # facet by time
  coord_fixed(xlim = c(-130, -90),
              ylim = c(20, 55))  +      # zoom in
  theme_bw() 
          
plot_1999 + geom_vline(xintercept = lon.mon, col = "red")+ 
  geom_hline(yintercept = lat.mon, col = "red")


# Restringir datos en la zona de monsón. ----------------------------------

data_monson <- data_month %>% filter(between(lon, lon.mon[1],lon.mon[2]),
                                     between(lat, lat.mon[1],lat.mon[2]))

#prueba con datos de 1999

data_monson <- data_monson %>% filter(year %in% 1999:1999)

data_monson_reduced <- data_monson %>% filter(month %in% 1:1)

real_data_geo <- data_monson_reduced %>% mutate(lon=lon+360) %>% select(lon,lat)

real_data_geo_sf <- st_as_sf(real_data_geo, coords = c("lon", "lat"), crs = 4326)

# coordenadas del modelo regional -----------------------------------------

variable_narccap <- 'Temp' # Temp or Prec
datasetfile=paste0("../MCMC_NARCCAP/data_narccap/dataset",
                   variable_narccap,"-Luis0122.Rdata")
load(datasetfile)

# create id per time
hh_sf <- hh_sf %>% group_by(Year, Month) %>% mutate(id = 1:n()) %>% ungroup()

# select one slice of time.
hh_sf_reduced <- hh_sf %>% filter(Year == 1999,
                                  Month == 1)

coordenadas_regional <- st_coordinates(hh_sf_reduced)

# Coordinates -------------------------------------------------------------

dim(real_data_geo_sf)
dim(hh_sf_reduced)

assign = st_nn(real_data_geo_sf, hh_sf_reduced, k = 1, parallel = 4)    

data_monson %>% select(year,month) %>% unique() %>% dim()

ntime = 12
index<-rep(cbind(unlist(assign)),ntime)

length(index)
dim(data_monson)

data_monson_1 <- data_monson %>% mutate(assign = index)

#
range(data_monson_1$assign)
range(hh_sf_reduced$id)
test <- data_monson_1 %>% left_join(hh_sf, 
                            by = c("year" = "Year",
                                   "month" = "Month",
                                   "assign" = "id"))

test <- test %>%  mutate(dif = (temp-ts))

hist(test$dif)

plot_temp <- test %>% filter(year == 1999) %>%
  ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = temp),size = 2.5)+
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = map_data("world"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  facet_wrap(~month, nrow = 3) +        # facet by time
  coord_fixed(xlim = c(-130, -90),
              ylim = c(20, 55))  +      # zoom in
  theme_bw() 

plot_ts <- test %>% filter(year == 1999) %>%
  ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = ts),size = 2.5)+
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = map_data("world"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  facet_wrap(~month, nrow = 3) +        # facet by time
  coord_fixed(xlim = c(-130, -90),
              ylim = c(20, 55))  +      # zoom in
  theme_bw() 

plot_dif <- test %>% filter(year == 1999) %>%
  ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = dif),size = 2.5)+
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = map_data("world"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  facet_wrap(~month, nrow = 3) +        # facet by time
  coord_fixed(xlim = c(-130, -90),
              ylim = c(20, 55))  +      # zoom in
  theme_bw() 

plot_temp
plot_ts
plot_dif

ggsave(plot_temp, 
       filename = paste0("./figures/plot_temp.jpg"), 
       height = 20, width = 20,bg="white")

ggsave(plot_ts, 
       filename = paste0("./figures/plot_ts.jpg"), 
       height = 20, width = 20,bg="white")

ggsave(plot_dif, 
       filename = paste0("./figures/plot_dif.jpg"), 
       height = 20, width = 20,bg="white")
