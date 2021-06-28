library(maps)
library(gstat)
library(maptools)
library(spacetime)
library(RColorBrewer)
library(fields)
library(mapdata)
library(geosphere)
library(pdist)

setwd("~/MRA_ST/MRA2_mensual")
getwd()

load('Temp_mensual.Rdata')
#load('Prec_mensual.Rdata')

str(hh)
class(hh)
attr(hh,"time")
attr(hh,"sp")

length(hh@sp)  #2469 points in a specific month
length(hh@time) #12 months * 18 years + 11 = 227 months time points in each spatial point

#spatial point
hh[1,]

#time point
hh[,1]

#spatio-temporal point
hh[1,1]

#selecting variables
str(hh[,,"Y"])
str(hh[,,"OMEGA"])
str(hh[,,"TREFHT"])
str(hh[,,"PSL"])
str(hh[,,"U"])
str(hh[,,"V"])


# 1. Variable regional: temperature ------------------------------------------

var_regional <- hh[,,"Y",drop=FALSE]
class(var_regional)

# 1.1 multi-panel plots ---------------------------------------------------

months = 1:12
yr = 1985 # or another year
time = as.POSIXct(paste(yr,months,"15", sep="-"), tz = "GMT")
stplot(var_regional[,time])
stplot(var_regional[,time],ylim=c(23,43),xlim=c(-123,-92))


# junio de cada año
yrs = 1981:1999
time = as.POSIXct(paste(yrs, "-06-15", sep=""), tz = "GMT")
stplot(var_regional[,time],main="Temperatura en la región del monzón")

# 1.2 Hovmoller diagrams ------------------------------------------------------

#pdf("grafico_pagina4.pdf")
scales=list(x=list(rot = 45))
stplot(var_regional, mode = "xt", scales = scales, xlab = NULL)
#dev.off()

# 1.3 Time series plot -------------------------------------------------------------

sample<- sample(1:length(hh@sp),10)
stplot(var_regional[sample,],mode="ts")
stplot(var_regional[sample,],mode="tp")

# 1.4 Map (5 locations) -----------------------------------------------------


locations <- unique(hh@sp@coords)
plot(locations[,1]-360,locations[,2],
     ylim=c(23,43),xlim=c(-123,-92),lwd=0.1,
     xlab="Easting", ylab="Northing",
     main="")
map("worldHires",xlim=c(-130,-85), ylim=c(20,50), 
    col=1, add=TRUE)
rect(-120,25,-95,40,border="red",lwd=3.5)

## puntos
nlon <- length(locations[,1])   #lon
nlat <- length(locations[,2])   #lat
p50_varlon <- sort(locations[,1])[(nlon+1)/2]
p50_varlat <- sort(locations[,2])[(nlat+1)/2]
p25_varlon <- sort(locations[,1])[(nlon+3)/4]
p25_varlat <- sort(locations[,2])[(nlat+1)/4]
p75_varlon <- sort(locations[,1])[(nlon+3)/4*3]
p75_varlat <- sort(locations[,2])[(nlat+1)/4*3]

p1<-c(p50_varlon,p50_varlat)
p2<-c(p25_varlon,p25_varlat)
p3<-c(p75_varlon,p75_varlat)
p4<-c(p25_varlon,p75_varlat)
p5<-c(p75_varlon,p25_varlat)
select_locations<-rbind(p1,p2,p3,p4,p5)

distancias<-pdist(locations, select_locations)
distancias<-as.matrix(distancias)
aprox_select_locations<-apply(distancias, 2, which.min)
locations_aprox<-locations[aprox_select_locations,]-matrix(c(rep(360,5),rep(0,5)),nrow=5)

points(locations_aprox, col="blue",pch=c("1","2","3","4","5"))

stplot(var_regional[aprox_select_locations,],mode="ts")
stplot(var_regional[aprox_select_locations,],mode="tp") #one panel for each variable


