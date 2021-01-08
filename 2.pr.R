library(maps)
library(gstat)
library(maptools)
library(spacetime)
library(RColorBrewer)
library(fields)
library(mapdata)
library(geosphere)
library(pdist)

library(tidyverse)
library(fracdiff)
library(arfima)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)
library(forecast)
library(fpp2)
library(ggfortify)
library(dplR)
library(wavethresh)
library(WaveletComp)
library(astsa)


setwd("~/MRA_ST/MRA2_mensual")
getwd()

#load('Temp_mensual.Rdata')
load('Prec_mensual.Rdata')

var_regional <- hh[,,"Y",drop=FALSE]
class(var_regional)

# 1. Map (5 locations) --------------------------------------------------

aprox_select_locations <- c(1237, 626, 1859, 1843, 634) # 5 locations from the previous script.
stplot(var_regional[aprox_select_locations,],mode="ts")

datos.ts<-var_regional[aprox_select_locations,,drop=TRUE]
y1<-ts(datos.ts[1,,"Y"]$Y,frequency = 12, start=c(1981,1))
y2<-ts(datos.ts[2,,"Y"]$Y,frequency = 12, start=c(1981,1))
y3<-ts(datos.ts[3,,"Y"]$Y,frequency = 12, start=c(1981,1))
y4<-ts(datos.ts[4,,"Y"]$Y,frequency = 12, start=c(1981,1))
y5<-ts(datos.ts[5,,"Y"]$Y,frequency = 12, start=c(1981,1))

datos.ts<- cbind(y1,y2,y3,y4,y5)

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("prec_lineal", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

autoplot(datos.ts, xlab = "Tiempo", ylab = "Precipitación") +
  scale_colour_discrete("Locación", labels = c("1", "2", "3","4","5"))

dev.off()

autoplot(datos.ts, facets = TRUE)

round(corr<-cor(datos.ts),2)
corrplot(corr, method="circle")


p1<-ggAcf(y1, lag.max = 50) + ggtitle("Locación 1")
p2<-ggAcf(y2, lag.max = 50) + ggtitle("Locación 2")
p3<-ggAcf(y3, lag.max = 50) + ggtitle("Locación 3")
p4<-ggAcf(y4, lag.max = 50) + ggtitle("Locación 4")
p5<-ggAcf(y5, lag.max = 50) + ggtitle("Locación 5")

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("prec_acf", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

grid.arrange(p1, p2, p3, p4, p5, nrow = 3)
dev.off()

# Location 1 --------------------------------------------------------------

par(mfrow=c(3,1))
ts.plot(y1)
acf(y1,lag.max=50,ci=0.95)
pacf(y1,lag.max=50,ci=0.95)

ts.plot(y4)
acf(y4,lag.max=500,ci=0.95)
pacf(y4,lag.max=500,ci=0.95)

##
library(arfima)
summary(y1.fd <- arfima::arfima(y1))  
summary(y1.fd)$coef

fdGPH(y1, bandw=.9)   
fdGPH(y2, bandw=.9)
fdGPH(y3, bandw=.9)
fdGPH(y4, bandw=.9)
fdGPH(y5, bandw=.9)

# Spectral Analysis
par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")

ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)

# Spectral analysis

par(mfrow=c(1,2))
raw.spec1 <- spec.pgram(y1, taper = 0)
plot(raw.spec1, log = "no")

max(raw.spec1$spec)
(freq<-raw.spec1$freq[(raw.spec1$spec==max(raw.spec1$spec))])
(periodo<-1/freq)

ts.plot(datos.ts, col=1:9)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1)


# Wavelets analysis

datos.ts<- as.data.frame(datos.ts)
colnames(datos.ts)<-c("y1","y2","y3","y4","y5")

my.w1 <- analyze.wavelet(datos.ts, "y1",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 100,make.pval = TRUE, n.sim = 10)
my.w2 <- analyze.wavelet(datos.ts, "y2",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 100,make.pval = TRUE, n.sim = 10)
my.w3 <- analyze.wavelet(datos.ts, "y3",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 100,make.pval = TRUE, n.sim = 10)
my.w4 <- analyze.wavelet(datos.ts, "y4",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 100,make.pval = TRUE, n.sim = 10)
my.w5 <- analyze.wavelet(datos.ts, "y5",loess.span = 0,
                         dt = 1, dj = 1/250,lowerPeriod = 1,
                         upperPeriod = 100,make.pval = TRUE, n.sim = 10)

wt.image(my.w1, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w2, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w3, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w4, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w5, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))





