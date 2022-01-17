library(stringr)
library(ncdf4)
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)

### cambiar as.period

## GLOBAL VARIABLES

VARNAME = "V" # "U", "OMEGA"

blatitude <- c(19.12639, 74.40000)
blongitude <- c(198.6576, 326.4000)
dirbase <- "~/../Emuladores/datos/datosglobal_update14062021/"
listfilesg <- list.files(path = dirbase, 
                         pattern=paste0("*.",VARNAME,".*.nc"))
fecha <- '1870-01-01'
var <- NULL
#dataset.time <- substr(separate(tibble(listfilesg),1, 
#                    sep="_", as.character(c(1:3)))$'1',24,27)
#Crear un vector del inicio del tiempo de cada base

#for (i in 1:length(listfilesg)) {
i<- 1
  show(paste0('Construccion datos mensuales-', i))
  vglobal <- ncdf4::nc_open(paste0(dirbase, listfilesg[i]))
  var_pre <- ncdf4::ncvar_get(vglobal, VARNAME)
  lonvar <- ncdf4::ncvar_get(vglobal, 'lon')
  latvar <- ncdf4::ncvar_get(vglobal, 'lat')
  timevar <- ncdf4::ncvar_get(vglobal, 'time')
  altvar  <- ncdf4::ncvar_get(vglobal, 'lev')
  ncdf4::nc_close(vglobal)
  fechabase <- ymd(fecha)
  timevar <- fechabase + as.period(ddays(timevar))
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
    group_by(Year, Month, lon, lat) %>%
    summarise(m = mean(eval(as.name(VARNAME)))) %>% 
    ungroup()
  var <- bind_rows(var, var_pre)
#}

colnames(var) <- c("Year", "Month", "lon", "lat", VARNAME )
assign(paste0(VARNAME, "_Global"),var,envir = .GlobalEnv)
rm(var);rm(var_pre);rm(vglobal)

#save(U_Global, file="U_Global.Rdata")
#save(V_Global, file="V_Global.Rdata")
#save(OMEGA_Global, file="OMEGA_Global.Rdata")

VARNAME =  "PSL" #"TREFHT" # or "PSL" # or 
listfilesg <- list.files(path = dirbase, 
                         pattern=paste0("*.",VARNAME,".*.nc"))
var <- NULL

#for (i in 1:length(listfilesg)) {
i<-1
  show(paste0('Construccion datos mensuales-', i))
  vglobal <- ncdf4::nc_open(paste0(dirbase, listfilesg[i]))
  var_pre <- ncdf4::ncvar_get(vglobal, VARNAME)
  lonvar <- ncdf4::ncvar_get(vglobal, 'lon')
  latvar <- ncdf4::ncvar_get(vglobal, 'lat')
  timevar <- ncdf4::ncvar_get(vglobal, 'time')
  altvar  <- ncdf4::ncvar_get(vglobal, 'lev')
  ncdf4::nc_close(vglobal)
  fechabase <- ymd(fecha)
  timevar <- fechabase + as.period(ddays(timevar))
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
    mutate(Time = ymd(as.character(Time))) %>% filter(Time >= ymd('1968-01-01')) %>%
    mutate(Year = year(Time), Month = month(Time)) %>% group_by(Year, Month, lon, lat) %>%
    summarise(m = mean(eval(as.name(VARNAME)))) %>% ungroup()
  var <- bind_rows(var, var_pre)
#}

colnames(var) <- c("Year", "Month", "lon", "lat", VARNAME )
assign(paste0(VARNAME, "_Global"),var,envir = .GlobalEnv)
rm(var);rm(var_pre);rm(vglobal)

save(PSL_Global, file="PSL_Global.Rdata")
#save(TREFHT_Global, file="TREFHT_Global.Rdata")

# VARNAME = "QBOT" 
# listfilesg <- list.files(path = dirbase, 
#                          pattern=paste0("*.",VARNAME,".*.nc"))
# 
#   vglobal <- ncdf4::nc_open(paste0(dirbase, listfilesg))
#   var_pre <- ncdf4::ncvar_get(vglobal, VARNAME)
#   lonvar <- ncdf4::ncvar_get(vglobal, 'lon')
#   latvar <- ncdf4::ncvar_get(vglobal, 'lat')
#   timevar <- ncdf4::ncvar_get(vglobal, 'time')
#   ncdf4::nc_close(vglobal)
#   fechabase <- ymd(fecha)
#   
#   timevar <- fechabase+lubridate::ddays(timevar) 
#   
#   #identificar los dias "29 de febrero".
#   find_leap = function(x){
#     day(x) == 29 & month(x) == 2 
#   }
#   timevar1<-timevar[!find_leap(timevar)] 
#   moredays<-timevar[length(timevar)]+ 
#     lubridate::ddays(seq(from=1/8, length=sum(find_leap(timevar)), by=1/8))
#   timevar<-c(timevar1,moredays)
#   
#   
#   dimnames(var_pre)[[1]] <- lonvar
#   dimnames(var_pre)[[2]] <- latvar
#   dimnames(var_pre)[[3]] <- as.character(timevar)
#   var_pre <- melt(var_pre[,,1201:1560])
#   colnames(var_pre) <- c('lon', 'lat', 'Time', VARNAME)
#   var_pre <-
#     var_pre %>% filter(lat >= blatitude[1],
#                        lat <= blatitude[2],
#                        lon >= blongitude[1],
#                        lon <= blongitude[2]) %>%
#     mutate(Time = ymd(as.character(Time))) %>% 
#     filter(Time >= ymd('1968-01-01')) %>%
#     mutate(Year = year(Time), Month = month(Time)) %>% 
#     group_by(Year, Month, lon, lat) %>%
#     summarise(m = mean(eval(as.name(VARNAME)))) %>% ungroup()
#   
# colnames(var_pre) <- c("Year", "Month", "lon", "lat", VARNAME )
# assign(paste0(VARNAME, "_Global"),var,envir = .GlobalEnv)
# rm(var);rm(var_pre);rm(vglobal)
# 
# save(QBOT_Global, file="QBOT_Global.Rdata")
# 
