#library("animation")
library("dplyr")
library("ggplot2")
library("gstat")
library("maps")
library("mapdata")
library("STRbook")
library("grid")
library("gridExtra")


#loading data
load("ts_RegionalMensualMonson.Rdata")
names(ts_Regional)

head(ts_Regional)

ts_Regional$lon <- ts_Regional$lon-360

ts_Regional$ts <- ts_Regional$ts- 273.15 

ts_Regional1998 <- filter(ts_Regional, Year %in% c(1998),
                                       Month %in% 1:12) 

Month_label <- c("ene","feb","mar","abr","may","jun","jul","ago","sept","oct","nov","dic")
names(Month_label) <- as.character(1:12)

plot1 <- ggplot(ts_Regional1998) +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = ts),           # attribute color
             size = 2.5)+                # make all points larger
  col_scale(name = "Temperatura (°C)") +
  xlab("Longitud") +             # x-axis label
  ylab("Latitud") +              # y-axis label
  labs(title = "Monzón Norteamericano: Temperatura mensual 1998", 
       subtitle = "", 
       caption = "Fuente: datos de NARCCAP") +
  geom_path(data = map_data("worldHires"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
  facet_wrap(~Month, nrow = 3,labeller = labeller( Month = Month_label)) +        # facet by time
  coord_fixed(xlim = c(-122, -92),
              ylim = c(20, 42))  +      # zoom in
  theme_bw()     

plot1


# Algunos gráficos para la página -----------------------------------------

pdf("grafico_pagina2.pdf")
print(plot1)
dev.off()

png(file = "mapa1.png", bg = "transparent")
print(plot1)
dev.off()

library(Cairo)
Cairo::Cairo(
  30, #length
  30, #width
  file = paste("mapa2", ".png", sep = ""),
  type = "png", #tiff
  bg = "transparent", #white or transparent depending on your requirement 
  dpi = 300,
  units = "cm" #you can change to pixels etc 
)
print(plot1)
dev.off()


