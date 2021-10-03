library(stringr)
library(ncdf4)
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)

#########################################################
#########################################################



# VARIABLES REGIONALES

rm(list=ls())
VARnames <- c("ts","pr","hus","va","ua")
dirbase <- "~/../Emuladores/datos/datosregional/"
blatitude <- c(20, 40) 
blongitude <- c(240, 265)
for (VARname in VARnames) {
  show(VARname)
  listfilesr <- list.files(path = dirbase, 
                           pattern=paste0(VARname,"_CRCM_ccsm_.*10103.nc"))
  varreg <- NULL
  blatitudet <- NULL
  blongitudet <- NULL
  dataset.time <- str_sub(listfilesr, start= -13,end = -10)
  
  
  for(i in 4:length(listfilesr)){
    show(paste0('Construccion datos diarios Regional-',i))
    regional <- ncdf4::nc_open(paste0(dirbase,listfilesr[i])) 
    varreg_pre <- ncdf4::ncvar_get(regional,VARname)
    lonvar <- ncdf4::ncvar_get(regional,'lon')
    latvar <- ncdf4::ncvar_get(regional,'lat')
    xcvar <- ncdf4::ncvar_get(regional,'xc')
    ycvar <- ncdf4::ncvar_get(regional,'yc')
    timevar <- ncdf4::ncvar_get(regional,'time') 
    blatitude <- c(min(blatitude)-1.4,floor(max(blatitude))+1.4)
    blatitudet <- rbind(blatitudet,blatitude)
    blongitude <- c(min(blongitude)-1.4,floor(max(blongitude))+1.4)
    blongitudet <- rbind(blongitudet,blongitude)
    
    ############ Redefinir el tiempo para excluir los 29 de febrero.##### inicio
    fechabase <- lubridate::ymd(paste0(dataset.time[i],'-01-01'))
    timevar <- timevar-floor(timevar[1])    #redefinir el tiempo
    timevar <- fechabase+lubridate::ddays(timevar) ##Transforma la variable de tiempo a formato Año mes día
    
    #identificar los dias "29 de febrero".
    find_leap = function(x){
      day(x) == 29 & month(x) == 2 
    }
    timevar1<-timevar[!find_leap(timevar)] ##eliminar los dias "29 de febrero"
    moredays<-timevar[length(timevar)]+ lubridate::ddays(seq(from=1/8, length=sum(find_leap(timevar)), by=1/8))
    timevar<-c(timevar1,moredays)
    ############ Redefinir el tiempo para excluir los 29 de febrero.##### fin
    
    dimnames(varreg_pre)[[1]] <- xcvar
    dimnames(varreg_pre)[[2]] <- ycvar
    dimnames(varreg_pre)[[3]] <- as.character(timevar) 
    
    varreg_pre <- reshape2::melt(varreg_pre) ##Reorganiza los datos
    
    dimnames(latvar)[[1]] <- xcvar
    dimnames(lonvar)[[1]] <- xcvar
    dimnames(latvar)[[2]] <- ycvar
    dimnames(lonvar)[[2]] <- ycvar
    
    latlondata <- expand.grid(xcvar,ycvar)
    latlondatapre <- t(sapply(X=1:dim(latlondata)[1], 
                              FUN = function(x) return(c(latvar[as.character(
                                latlondata[x,1]),as.character(latlondata[x,2])],
                                lonvar[as.character(latlondata[x,1]),
                                       as.character(latlondata[x,2])]))))
    latlondata <- cbind(latlondata,latlondatapre)
    colnames(latlondata) <- c('xc','yc','lat','lon')
    colnames(varreg_pre) <- c('xc','yc','Time',VARname) 
    
    varreg_pre <- varreg_pre %>% 
      mutate(Time=ymd_hms(as.character(Time))) %>%
      mutate(Day = day(Time), Month = month(Time), 
             Year = year(Time)) 
    
    varreg_pre <- varreg_pre %>% 
      left_join(latlondata,by = c('xc', 'yc')) %>%
      dplyr::select(-xc,-yc) 
    
    ## Select locations
    
    varreg_pre <- varreg_pre %>% 
      mutate(ID = rep(i, dim(varreg_pre)[1])) %>% 
      filter(lat<40 & lat>20 & lon <265 & lon>240) 
    
    varreg <- bind_rows(varreg,varreg_pre) 
  }
  
  varreg <- varreg %>% 
    group_by(Month, Year,lon,lat) %>% 
    summarise(m=mean(eval(as.name(VARname)), na.rm = TRUE)) %>% 
    ungroup()  %>% 
    filter(!(Year==1999 & Month==12)) %>%                               #eliminar 1999-12 (media basada en un valor)
    filter(!(Year==2000 & Month==1))                                   #eliminar 2000-1 (media basada en un valor)
  
  
  colnames(varreg) <- c("Month", "Year", "lon", "lat", VARname)
  assign(x=paste0(VARname, "_Regional"),value=varreg, envir = .GlobalEnv)
  summary(varreg)
  
  
  
}



#save(ua_Regional, file="ua_RegionalMensualMonson.Rdata")
#save(va_Regional, file="va_RegionalMensualMonson.Rdata")
#save(hus_Regional, file="hus_RegionalMensualMonson.Rdata")

#########################################################
#########################################################
#########################################################
#########################################################