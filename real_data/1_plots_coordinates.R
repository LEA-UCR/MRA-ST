library(nngeo)

load(file="./data/real_data.Rdata")

#monsoon
lat.mon=c(21.45146, 39.99363)
lon.mon=c(-119.99590 , -95.00983)


# Ilustración con datos de 1999 -------------------------------------------

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

#prueba con datos de 20
data_monson <- data_monson %>% filter(year %in% 1999:1999)


real_data_geo <- data_monson %>% mutate(lon=lon+360) %>% select(lon,lat)


real_data_geo_sf <- st_as_sf(real_data_geo, coords = c("lon", "lat"), crs = 4326)

# coordenadas del modelo regional -----------------------------------------

variable_narccap <- 'Temp' # Temp or Prec
datasetfile=paste0("../MCMC_NARCCAP/data_narccap/dataset",
                   variable_narccap,"-Luis0122.Rdata")
load(datasetfile)

coordenadas_regional <- st_coordinates(hh_sf)

# Coordinates -------------------------------------------------------------

dim(real_data_geo_sf)
dim(hh_sf)

assign = st_nn(real_data_geo_sf, hh_sf, k = 1, parallel = 4)
index<-rep(cbind(unlist(assign)),ntime)



