library(stringr)
library(ncdf4)
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)
library(PCICt)

### cambiar as.period

## GLOBAL VARIABLES

#VARNAME = "OMEGA"  
#VARNAME = "U"
#VARNAME = "V" 
VARNAME = "TREFHT"


blatitude <- c(19.12639, 74.40000)
blongitude <- c(198.6576, 326.4000)
dirbase <- "~/../Emuladores/datos/datosglobal_update14062021/"
listfilesg <- list.files(path = dirbase, 
                         pattern=paste0("*.",VARNAME,".*.nc"))   
#fecha <- '1870-01-01'
var <- NULL
#dataset.time <- substr(separate(tibble(listfilesg),1, 
#                    sep="_", as.character(c(1:3)))$'1',29,35) #OMEGA
dataset.time <- substr(separate(tibble(listfilesg),1, 
                    sep="_", as.character(c(1:3)))$'1',25,35) #V or U
#dataset.time <- substr(separate(tibble(listfilesg),1, 
#                    sep="_", as.character(c(1:3)))$'1',27,35) #PSL

#Crear un vector del inicio del tiempo de cada base

for (i in 1:length(listfilesg)) {
  show(paste0('Construccion datos mensuales-', i))
  vglobal <- ncdf4::nc_open(paste0(dirbase, listfilesg[i]))
  var_pre <- ncdf4::ncvar_get(vglobal, VARNAME)
  lonvar <- ncdf4::ncvar_get(vglobal, 'lon')
  latvar <- ncdf4::ncvar_get(vglobal, 'lat')
  timevar <- ncdf4::ncvar_get(vglobal, 'time')
  altvar  <- ncdf4::ncvar_get(vglobal, 'lev')
  ncdf4::nc_close(vglobal)
# fechabase <- ymd(fecha)
  
  ####Shu######## inicio
  date_base <- as.PCICt(paste0(dataset.time[i],'-01'), cal="365_day")
  timevar <- date_base+timevar*60*60*24
  #timevar <- seq(as.Date(paste0(dataset.time[i],"-01-15")), length.out=length(timevar)/2, by="months")
  ####Shu######## fin
  
  dimnames(var_pre)[[1]] <- lonvar
  dimnames(var_pre)[[2]] <- latvar
  dimnames(var_pre)[[3]] <- altvar
  dimnames(var_pre)[[4]] <- as.character(timevar)   
  var_pre <- melt(var_pre[,,19,])
  colnames(var_pre) <- c('lon', 'lat', 'Time', VARNAME)
  var_pre <-
    var_pre %>% filter(lat >= blatitude[1],
                       lat <= blatitude[2],
                       lon >= blongitude[1],
                       lon <= blongitude[2]) %>%
    mutate(Time = ymd(as.character(Time))) %>% 
    filter(Time >= ymd('1968-01-01')) %>%
    mutate(Year = year(Time), Month = month(Time)) %>%
    select(Year, Month,lon,lat, VARNAME) %>%
    distinct(Year,Month,lat,lon,.keep_all=T)
  
  #   group_by(Year, Month,Day, lon, lat) %>%
 #   summarise(m = mean(eval(as.name(VARNAME)))) %>% 
 #   ungroup()
  var <- bind_rows(var, var_pre)
}

colnames(var) <- c("Year", "Month", "lon", "lat", VARNAME )
assign(paste0(VARNAME, "_Global"),var,envir = .GlobalEnv)
rm(var);rm(var_pre);rm(vglobal)

#save(OMEGA_Global, file="OMEGA_Global.Rdata")
#save(U_Global, file="U_Global.Rdata")
save(V_Global, file="V_Global.Rdata")



VARNAME = "TREFHT" # or "PSL" # or 
#VARNAME = "PSL" 

listfilesg <- list.files(path = dirbase, 
                         pattern=paste0("*.",VARNAME,".*.nc"))
var <- NULL
dataset.time <- substr(separate(tibble(listfilesg),1, 
                                sep="_", as.character(c(1:3)))$'1',30,36) #TREFHT
#dataset.time <- substr(separate(tibble(listfilesg),1, 
#                                sep="_", as.character(c(1:3)))$'1',22,25) #PSL

for (i in 1:length(listfilesg)) {
  show(paste0('Construccion datos mensuales-', i))
  vglobal <- ncdf4::nc_open(paste0(dirbase, listfilesg[i]))
  var_pre <- ncdf4::ncvar_get(vglobal, VARNAME)
  lonvar <- ncdf4::ncvar_get(vglobal, 'lon')
  latvar <- ncdf4::ncvar_get(vglobal, 'lat')
  timevar <- ncdf4::ncvar_get(vglobal, 'time')
  altvar  <- ncdf4::ncvar_get(vglobal, 'lev')
  ncdf4::nc_close(vglobal)
  #fechabase <- ymd(fecha)

  ####Shu######## inicio
  date_base <- as.PCICt(paste0(dataset.time[i],'-01'), cal="365_day")
  timevar <- date_base+timevar*60*60*24
  #timevar <- seq(as.Date(paste0(dataset.time[i],"-01-15")), length.out=length(timevar), by="months")
  ####Shu######## fin
  
  dimnames(var_pre)[[1]] <- lonvar
  dimnames(var_pre)[[2]] <- latvar
  dimnames(var_pre)[[3]] <- as.character(timevar)
  var_pre <- melt(var_pre)
  colnames(var_pre) <- c('lon', 'lat', 'Time', VARNAME)
  var_pre <-
    var_pre %>% filter(lat >= blatitude[1],
                       lat <= blatitude[2],
                       lon >= blongitude[1],
                       lon <= blongitude[2]) %>%
    mutate(Time = ymd(as.character(Time))) %>% 
    filter(Time >= ymd('1968-01-01')) %>%
    mutate(Year = year(Time), Month = month(Time)) %>%
    select(Year, Month,lon,lat, VARNAME) %>%
    distinct(Year,Month,lat,lon,.keep_all=T)
  var <- bind_rows(var, var_pre)
}

colnames(var) <- c("Year", "Month", "lon", "lat", VARNAME )
assign(paste0(VARNAME, "_Global"),var,envir = .GlobalEnv)
rm(var);rm(var_pre);rm(vglobal)

#save(PSL_Global, file="PSL_Global-Shu.Rdata")
save(TREFHT_Global, file="TREFHT_Global.Rdata")

