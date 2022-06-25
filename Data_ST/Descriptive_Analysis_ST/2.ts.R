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

load('Temp_mensual.Rdata')
#load('Prec_mensual.Rdata')

var_regional <- hh[,,"Y",drop=FALSE]
class(var_regional)



# 0. Maps -----------------------------------------------------------------

ts.eof1<-eof(var_regional, "spatial", returnEOFs = FALSE, scale.=TRUE) #correlation
ts.eof2<-eof(var_regional, "spatial", returnEOFs = FALSE, scale.=FALSE) #covariance
plot(ts.eof1)
plot(ts.eof2)
summary(ts.eof1)$importance[,1:10]
summary(ts.eof2)$importance[,1:10]

ts.eof2a<-eof(var_regional, "spatial", returnEOFs = TRUE, scale.=FALSE) #covariance

str(ts.eof2a)
ts.comp1<-ts.eof2a[,"EOF1"]
ts.comp2<-ts.eof2a[,"EOF2"]

spplot(ts.comp1)
spplot(ts.comp2)

coordinates<-coordinates(ts.comp1)
EOF1a<-data.frame(coordinates,score=ts.comp1$"EOF1",componente=rep(1,nrow(coordinates)))
EOF2a<-data.frame(coordinates,score=ts.comp2$"EOF2",componente=rep(2,nrow(coordinates)))

plot(EOF1a$score,EOF2a$score)

plot1 <- {ggplot(EOF1a) +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = score),           # attribute color
             size = 2.5)+                # make all points larger
  col_scale(name = "Score") +
  xlab("Longitud") +             # x-axis label
  ylab("Latitud") +              # y-axis label
  labs(title = "", 
       subtitle = "", 
       caption = "") +
  geom_path(data = map_data("worldHires"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  coord_fixed(xlim = c(-122, -92),
              ylim = c(20, 42))  +      # zoom in
  theme_bw()     
}
plot2 <- {ggplot(EOF2a) +             # plot points
    geom_point(aes(x = lon,y = lat,       # lon and lat
                   colour = score),           # attribute color
               size = 2.5)+                # make all points larger
    col_scale(name = "Score") +
    xlab("Longitud") +             # x-axis label
    ylab("Latitud") +              # y-axis label
    labs(title = "", 
         subtitle = "", 
         caption = "") +
    geom_path(data = map_data("worldHires"),   # add map
              aes(x = long, y = lat, group = group), 
              col=1) +
    coord_fixed(xlim = c(-122, -92),
                ylim = c(20, 42))  +      # zoom in
    theme_bw()     
}
plot1
plot2

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("temp_EOF1", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
plot1
dev.off()

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("temp_EOF2", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
plot2
dev.off()

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
  file = paste("temp_lineal", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)

autoplot(datos.ts, xlab = "Time", ylab = "Temperature") +
  scale_colour_discrete("Location", labels = c("1", "2", "3","4","5"))

dev.off()

autoplot(datos.ts, facets = TRUE)

round(corr<-cor(datos.ts),2)
corrplot(corr, method="circle")

p1<-ggAcf(y1, lag.max = 50) + ggtitle("Location 1")
p2<-ggAcf(y2, lag.max = 50) + ggtitle("Location 2")
p3<-ggAcf(y3, lag.max = 50) + ggtitle("Location 3")
p4<-ggAcf(y4, lag.max = 50) + ggtitle("Location 4")
p5<-ggAcf(y5, lag.max = 50) + ggtitle("Location 5")

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("temp_acf", ".png", sep = ""),
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


#diff 1
dy1<-diff(y1)

par(mfrow=c(3,1))
ts.plot(dy1)
acf(dy1,lag.max=500,ci=0.95)
pacf(dy1,lag.max=500,ci=0.95)


fdGPH(y1, bandw=.9)


# Spectral analysis
y<-y1

par(mfrow=c(1,2))
raw.spec <- spec.pgram(y, taper = 0)
plot(raw.spec, log = "no")

tabla <- data.frame(freq=raw.spec$freq,spec=raw.spec$spec)
head(arrange(tabla,desc(spec)))
periodo <- 1/head(arrange(tabla,desc(spec)))[,1]*12
periodo


Cairo::Cairo(
  15, #length
  15, #width
  file = paste("temp_period", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
spec = mvspec(datos.ts, spans=c(3,3), taper=.1,col=1:5,lty=1:5)
legend("topright",
       c("location 1","location 2","location 3","location 4","location 5"),
       col=1:5,lty=1:5)
dev.off()

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



Cairo::Cairo(
  15, #length
  15, #width
  file = paste("temp1_wavelet", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
wt.image(my.w1, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()

wt.image(my.w2, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w3, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w4, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))
wt.image(my.w5, color.key = "quantile", n.levels = 250,
         legend.params = list(lab = "wavelet power levels", mar = 4.7))





