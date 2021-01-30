library(maps)
library(gstat)
library(maptools)
library(spacetime)
library(RColorBrewer)
library(fields)
library(mapdata)
library(geosphere)

library(tidyverse)
library(ggplot2)
library(reshape2)
library(dtw)
library(gridExtra)
library(reshape2)

load("ts_RegionalMensualMonson.Rdata")
names(ts_Regional)
head(ts_Regional)
ts_Regional$lon <- ts_Regional$lon-360

ts_Regional$time <- paste0(ts_Regional$Year,"-",ts_Regional$Month)

#converting from long format to wide format.
ts_Regional_wide0 <- dcast(ts_Regional, lon + lat ~ time, value.var="ts")
dim(ts_Regional_wide0)
names(ts_Regional_wide0)


spatial_info <- ts_Regional_wide0 %>% dplyr::select(lon,lat)%>% distinct()

ts_Regional_wide <- ts_Regional_wide0 %>% dplyr::select(-lon,-lat)
dim(ts_Regional_wide)

distMatrix <- dist(ts_Regional_wide, method="DTW")

#save(distMatrix,file="ts_DTW_dist.Rdata")
load(file="ts_DTW_dist.Rdata")


hc <- hclust(distMatrix, method="average")

plot(hc,  main="")
rect.hclust(hc, k = 6, border = "blue")

cluster3 <- cutree(hc, k = 3)
cluster4 <- cutree(hc, k = 4)
cluster5 <- cutree(hc, k = 5)
cluster6 <- cutree(hc, k = 6)
ts_Regional_wide0$cluster3 <- as.factor(cluster3)
ts_Regional_wide0$cluster4 <- as.factor(cluster4)
ts_Regional_wide0$cluster5 <- as.factor(cluster5)
ts_Regional_wide0$cluster6 <- as.factor(cluster6)

names(ts_Regional_wide0)

clusters<-c("cluster3","cluster4","cluster5","cluster6")


dim(ts_Regional_wide0)
names(ts_Regional_wide0)[230:233]
mapa<-list()
for(i in seq_along((clusters))){
mapa[[i]] <- {ggplot(ts_Regional_wide0) +             # plot points
    geom_point(aes_string(x = "lon",y = "lat",       # lon and lat
                   colour = clusters[i]),           # attribute color
               size = 2.5)+                # make all points larger
    xlab("Longitude") +             # x-axis label
    ylab("Latitude") +              # y-axis label
    labs(title = "", 
         subtitle = "", 
         caption = "") +
    scale_colour_discrete("Cluster") +
    geom_path(data = map_data("worldHires"),   # add map
              aes(x = long, y = lat, group = group), 
              col=1) +
    coord_fixed(xlim = c(-122, -92),
                ylim = c(20, 42))  +      # zoom in
    theme_bw()     
}
}


mapa0<-do.call("grid.arrange", c(mapa, ncol=2))

ggsave(plot=mapa0, filename="pr_clustering.png")

# printing maps -----------------------------------------------------------

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("ts_clustering1", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
mapa[[1]]
dev.off()

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("ts_clustering2", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
mapa[[2]]
dev.off()

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("ts_clustering3", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
mapa[[3]]
dev.off()

Cairo::Cairo(
  15, #length
  15, #width
  file = paste("ts_clustering4", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
mapa[[4]]
dev.off()





